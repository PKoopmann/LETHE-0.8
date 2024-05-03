package uk.ac.man.cs.lethe.internal.dl.forgetting.abox

import uk.ac.man.cs.lethe.internal.dl.datatypes.extended.ExtendedABoxClause
import uk.ac.man.cs.lethe.internal.dl.forgetting.InverseRoleProcessor
import uk.ac.man.cs.lethe.internal.dl.forgetting.direct._
import uk.ac.man.cs.lethe.internal.dl.proofs.{AbstractInferenceLogger, InferenceLogger}
import uk.ac.man.cs.lethe.internal.tools.Timeoutable

import scala.collection.mutable
import scala.collection.mutable.{HashMap, HashSet, MultiMap, TreeSet, Set => mutSet}

//import com.dongxiguo.fastring.Fastring.Implicits._
import com.typesafe.scalalogging.Logger

import uk.ac.man.cs.lethe.internal.dl.datatypes._
import uk.ac.man.cs.lethe.internal.forgetting.Forgetter

import uk.ac.man.cs.lethe.internal.dl.proofs.InferenceLogger


abstract class AbstractABoxClauseForgetter[T](roleHierarchy: RoleHierarchy, inverseRoles: Boolean = false)
  extends Timeoutable with ClauseSetListener
{

  //implicit val (logger, formatter, appender) = ZeroLoggerFactory.newLogger(ABoxSingleConceptForgetter)
  //import formatter._
  val logger = Logger[AbstractABoxClauseForgetter[T]]

  val rememberDefiners = false

  var inferenceLogger: AbstractInferenceLogger = InferenceLogger

  var ordering: Ordering[ConceptLiteral] = _
  var clauseOrdering: Ordering[ConceptClause] = _

  var allTBox: mutSet[ConceptClause] = _
  var allABox: MultiMap[Individual, ABoxClause] = _

  var definerFactory: DefinerFactory = _
  val subsumptionChecker =
    new SimpleSubsumptionChecker() with RoleSubsumptionChecker with DefinerSubsumptionChecker
  subsumptionChecker.setRoleHierarchy(roleHierarchy)


  var resultABox: TreeSet[ABoxClause] = _
  var resultTBox: TreeSet[ConceptClause] = _


  var definingClauses = new HashMap[BaseConcept, mutSet[ConceptClause]]()
    with MultiMap[BaseConcept, ConceptClause]

  var definerUsages = new HashMap[BaseConcept, mutSet[(ConceptLiteral, ConceptClause)]]()
    with MultiMap[BaseConcept, (ConceptLiteral, ConceptClause)]

  var definerUsagesA: MultiMap[BaseConcept, (Individual, ConceptLiteral, ABoxClause)] = _

  var individualRoles: MultiMap[Individual, RoleAssertion] = _
  var individualRolesInv: MultiMap[Individual, RoleAssertion] = _


  var resolutionRule: ResolutionRule = _

  var roleAssertionPropagationRule = new RoleAssertionPropagationRule(roleHierarchy)
  var rolePropagationRule: SimpleRolePropagationRule = _
  var roleResolutionRule: RoleResolutionRule = _
  var transitivityRule: TransitivityRule = _



  def forget(aboxClauses: Set[ABoxClause], definitions: Set[ConceptClause])
  : (Set[ABoxClause], Set[ConceptClause], Set[RoleAssertion])= {

    startTiming

    logger.info(s"Input: \n ${aboxClauses.mkString("\n")} \n ${definitions.mkString("\n")}")

    init()

    aboxClauses.map(_.withOrdering(ordering)).foreach(addABoxClause)
    definitions.map(_.withOrdering(ordering)).foreach(addConceptClause)

    preprocessClauses()

    definerFactory.addListener(this)



    derive()

    definerFactory.removeListener(this)

    (resultABox.toSet, resultTBox.toSet, individualRoles.values.flatten.toSet)
  }

  def init() = {
    assert(ordering!=null)

    definerFactory = new DefinerFactory(ALCFormulaPreparations, ordering, roleHierarchy)
    definerFactory.addListener(this)
    inferenceLogger.notifyDefinerFactory(definerFactory)

    subsumptionChecker.setDefinerFactory(definerFactory)
    subsumptionChecker.setRoleHierarchy(roleHierarchy)

    resolutionRule = new ResolutionRule(ordering)
    resolutionRule.ignoreInvalid = false

    rolePropagationRule = new SimpleRolePropagationRule(roleHierarchy, definerFactory)



    // ABox part
    resultABox = new TreeSet[ABoxClause]()(new ABoxClauseOrdering(clauseOrdering))
    definerUsagesA = new HashMap[BaseConcept, mutSet[(Individual, ConceptLiteral, ABoxClause)]]
      with MultiMap[BaseConcept, (Individual, ConceptLiteral, ABoxClause)]
  }

  def derive(): Unit

  protected def preprocessClauses() = {
    // first do transitivity rule, then rest
    addTransitivityClauses(allTBox)


    if(inverseRoles){
      val inverseRoleProcessor = new InverseRoleProcessor(definerFactory)

      val newClauses = inverseRoleProcessor.process(allTBox);

      logger.info(s"Clauses to be added due to inverse role processing: \n${newClauses.mkString("\n")}")

      newClauses.foreach{ c => proceedTBox(c); addConceptClause(c)}
    }

    logger.info(s"Stage 1 finished! Now start proper reasoning")
  }



  def addTransitivityClauses(clauses: Iterable[ConceptClause]) = {

    var transSubRoles = new HashMap[Role, mutSet[Role]]() with MultiMap[Role, Role]

    roleHierarchy.transitiveRoles.foreach{ role =>
      roleHierarchy.getSuperRoles(role).foreach{ r =>
        transSubRoles.addBinding(r, role)
      }
    }

    clauses.foreach{ clause =>
      if(clause.literals.size>1)
        clause.literals.foreach{ l => l.concept match {
          case UniversalRoleRestriction(r: Role,
          d: BaseConcept) if transSubRoles.contains(r) =>{
            transSubRoles(r).foreach { role2 =>
              val newDefiner = definerFactory.newDefiner()

              val clause2 = new ConceptClause(Set(ConceptLiteral(false, newDefiner),
                ConceptLiteral(true,
                  UniversalRoleRestriction(role2,
                    newDefiner))),
                ordering)

              logger.info(s"for transitivity: ${clause2}")

              updateTables(clause2)

              val resolutionCandidates = definingClauses.getOrElse(d, Set())
              val clause1 = new ConceptClause(Set(ConceptLiteral(false, newDefiner),
                ConceptLiteral(true, d)), ordering)
              logger.info(s"for transitivity: ${clause1}")
              resolutionRule.getDerivations(clause1,
                resolutionCandidates).flatMap{
                d =>
                  inferenceLogger.sendInference(d)
                  d.conclusions}.foreach(addConceptClause)
              // <-- have to be added: possible resolution candidates for later
              // <--- only update: don't want to introduce cycle if not neccessary
              val clause3 = clause.without(l)._with(ConceptLiteral(true,
                UniversalRoleRestriction(role2,
                  newDefiner)))

              logger.info(s"for transitivity: ${clause3}")
              addConceptClause(clause3) // <--- has to be added: otherwise no connection to new definer
            }
          }
          case _ => ;
        }

        }
    }
  }



  def proceedABox(_clause: ABoxClause): Unit = {
    checkCanceled

    logger.debug(s"derived clause: ${_clause}")

    if(redundant(_clause)){
      logger.debug(s"derived clause ${_clause} is subsumed")
      //      println("Subsumed!")
      return
    }


    val clause1 = subsumptionChecker.condenseClause(_clause)
    val clause = new ABoxClause(clause1.literals.filter(_._2.size>0))
    logger.debug(s"${if(clause!=_clause) "condensed: "+clause}")

    //    clause.literals.keys.foreach{ ind => allABox.addBinding(ind, clause) }

    val negDefiners = clause.literals.keys.flatMap{ ind => clause.literals(ind).literals.collect{
      case ConceptLiteral(false, c: BaseConcept) if isDefiner(c) => (ind, c)
    }}
    val posDefiners = clause.literals.keys.flatMap{ ind => clause.literals(ind).literals.collect{
      case ConceptLiteral(true, c: BaseConcept) if isDefiner(c) => (ind, c)
    }}

    if(posDefiners.size>0){
      //      assert(posDefiners.size==1, "clause shouldn't be possible: "+clause)
      // <--- if we forget roles, more is actually possible
      val (ind, definer) = posDefiners.head
      val cand = definingClauses.getOrElse(definer, Set[ConceptClause]()).toSet
      getTBoxResolutionDerivations(clause, cand, ind).foreach{ proceedABox }
    }
    else if(negDefiners.isEmpty){
      addABoxClause(clause)
    } else  { // no positive definer, and more than one negativ definer
      //assert(negDefiners.size==1) <--- not neccessary, see above
      val (ind, definer) = negDefiners.head
      logger.trace("Trigger!")
      val relations = individualRolesInv.getOrElse(ind, Set())

      val candidatesA = definerUsagesA.getOrElse(definer, Set())
      var news = relations.flatMap(r => candidatesA.flatMap{ c =>
        val (i,l,cl) = c
        logger.trace(s"New derivation from $c and $r")
        roleAssertionPropagationRule.apply(cl, r, l)
      })

      val candidates = definerUsages.getOrElse(definer, Set())
      news ++= relations.flatMap(r => candidates.flatMap{ c =>
        val (l,cl) = c
        logger.trace(s"New derivation from $c and $r")
        roleAssertionPropagationRule.apply(cl, r, l)
      })
      news.flatMap(getABoxResolutionDerivations(_, Set(clause), ind)).foreach{ proceedABox }
      // something to do with the other clauses?
    }
  }

  def getTBoxResolutionDerivations(clause: ABoxClause, clauses: Set[ConceptClause], ind: Individual) = {
    checkCanceled

    logger.debug(s"tbox resolution derivations for ${clause}")
    var der = resolutionRule.getDerivations(clause.literals(ind),
      clauses)

    inferenceLogger.sendInference(der)

    logger.debug(s"${der.mkString("\n")}")
    der.flatMap(_.conclusions).map(clause.replace(ind, _))
  }


  def getTBoxResolutionDerivations(clause: ConceptClause, clauses: Set[ConceptClause]) = {
    checkCanceled

    logger.debug(s"tbox resolution derivations for ${clause}")
    var der = resolutionRule.getDerivations(clause, clauses)

    inferenceLogger.sendInference(der)

    logger.debug(s"${der.mkString("\n")}")
    der.flatMap(_.conclusions)
  }


  def getABoxResolutionDerivations(clause: ABoxClause, clauses: Set[ABoxClause], ind: Individual) = {
    checkCanceled

    clauses.flatMap{ aboxClause =>
      var der = resolutionRule.getDerivations(clause.literals(ind),
        Set(aboxClause.literals(ind)))
      inferenceLogger.sendInference(der)

      der.flatMap(_.conclusions).map(cl =>
        clause.combineWith(aboxClause).replace(ind, cl))
    }
  }

  // def getTBoxRoleResolutionDerivations(clause: ABoxClause, clauses: Set[ConceptClause], ind: Individual) = { 
  //   var der = roleResolutionRule.getDerivations(clause.literals(ind), clauses)
  //   der.flatMap(_.conclusions).map(cl => clause.replace(ind, cl))
  // }

  def getABoxRoleResolutionDerivations(clause: ABoxClause,
                                       abClauses: Set[ABoxClause],
                                       tbClauses: Set[ConceptClause],
                                       ind: Individual): Set[ABoxClause] = {
    getTBoxRoleResolutionDerivations(clause.literals(ind),
      abClauses,
      tbClauses,
      ind).map{ ab => clause.combineWith(ab).replace(ind,ab.literals(ind))}
  }


  def getTBoxRoleResolutionDerivations(clause: ConceptClause,
                                       abClauses: Set[ABoxClause],
                                       tbClauses: Set[ConceptClause],
                                       ind: Individual): Set[ABoxClause] = {
    checkCanceled

    val result = new HashSet[ABoxClause]()
    val usage = new HashMap[ConceptClause, mutSet[ABoxClause]]() with MultiMap[ConceptClause, ABoxClause]

    abClauses.foreach{ aboxClause =>
      usage.addBinding(aboxClause.literals(ind), aboxClause)
    }

    // ADDED 31.12.2019 as it looked like a bug without:
    tbClauses.foreach{
      usage.addBinding(_, ABoxClause.empty)
    }

    var derivations = roleResolutionRule.getDerivations(clause, abClauses.map(_.literals(ind))++tbClauses)

    derivations.foreach{ derivation =>
      connect(derivation.premisses.map(usage.getOrElse(_, Set())).toList).foreach{ aboxClauses =>
        val combined = ABoxClause.combine(aboxClauses)
        derivation.conclusions.foreach{ conc =>
          result.add(new ABoxClause((combined.literals-ind)+(ind -> conc)))
        }
      }
    }
    result.toSet
  }

  def getTBoxRolePropagationDerivations(clause: ABoxClause,
                                        clauses: Set[ConceptClause],
                                        ind: Individual): Set[ABoxClause] = {
    checkCanceled
    rolePropagationRule.getDerivations(clause.literals(ind),
      clauses,
      clause.literals(ind).literals.head).flatMap{
      d =>
        inferenceLogger.sendInference(d)
        d.conclusions.map(clause.replace(ind,_))
    }
  }

  def getABoxRolePropagationDerivations(clause: ABoxClause,
                                        clauses: Set[ABoxClause],
                                        ind: Individual): Set[ABoxClause] = {
    checkCanceled
    clauses.flatMap{ cl2 =>
      val combined = clause.combineWith(cl2)
      rolePropagationRule.getDerivations(clause.literals(ind),
        cl2.literals(ind),
        clause.literals(ind).literals.head).flatMap{
        d=>
          inferenceLogger.sendInference(d)
          d.conclusions.map(c => combined.replace(ind, c))
      }
    }
  }

  /**
   * Apply role propagation so that the universal is replaced by an existential, so that role resolution
   * becomes possible.
   */
  def getTBoxRolePropagationDerivationsEx(clause: ABoxClause,
                                          clauses: Iterable[ConceptClause],
                                          ind: Individual): Iterable[ABoxClause] = {
    checkCanceled
    val lit1 = clause.literals(ind).literals.head
    clauses.flatMap{ cl2 =>
      val lit2 = cl2.literals.find(_.concept.isInstanceOf[ExistentialRoleRestriction])
      if(lit2==None)
        Set[ABoxClause]()
      else
        rolePropagationRule.derive(clause.literals(ind), lit1,
          cl2, lit2.get).map{ c =>
          clause.replace(ind, c)}

    }
  }


  def getABoxRolePropagationDerivationsWithEx(clause: ABoxClause,
                                              clauses: Set[ABoxClause],
                                              ind: Individual): Set[ABoxClause] = {
    checkCanceled
    val lit1 = clause.literals(ind).literals.head
    clauses.flatMap{ cl2 =>
      val combined = clause.combineWith(cl2)
      val lit2 = cl2.literals(ind).literals.find(_.concept.isInstanceOf[ExistentialRoleRestriction])
      if(lit2==None)
        Set[ABoxClause]()
      else
        rolePropagationRule.derive(clause.literals(ind), lit1,
          cl2.literals(ind), lit2.get).map{ c=>
          combined.replace(ind, c) }
    }
  }


  protected def connect(lists: List[Iterable[ABoxClause]]): Iterable[List[ABoxClause]] = lists match {
    case Nil => List()
    case list::rest => list.flatMap(value => connect(rest).map(value::_))
  } // <--- Maybe later put into some tools package


  def deriveTBoxABox(clause1: ConceptClause, literal1: ConceptLiteral,
                     clause2: ABoxClause, individual: Individual, literal2: ConceptLiteral) = {
    checkCanceled
    rolePropagationRule.derive(clause1, literal1,
      clause2.literals(individual), literal2).map { cl =>
      clause2.replace(individual, cl)
    }

  }


  def deriveABoxABox(individual: Individual,
                     clause1: ABoxClause, literal1: ConceptLiteral,
                     clause2: ABoxClause, literal2: ConceptLiteral) = {
    checkCanceled
    rolePropagationRule.derive(clause1.literals(individual), literal1,
      clause2.literals(individual), literal2).map { cl =>
      clause1.combineWith(clause2).replace(individual, cl)
    }
  }

  // def setTBoxClauses(tboxClauses: Set[ConceptClause]) = { 
  //   resultTBox = new TreeSet[ConceptClause]()(clauseOrdering)
  //   tboxClauses.foreach(addConceptClause)
  // }

  def setRoleAssertions(roleAssertions: Set[RoleAssertion]) = {
    individualRoles =
      new HashMap[Individual, mutSet[RoleAssertion]]() with MultiMap[Individual, RoleAssertion]
    individualRolesInv =
      new HashMap[Individual, mutSet[RoleAssertion]]() with MultiMap[Individual, RoleAssertion]

    roleAssertions.foreach{ ra => addRoleAssertion(ra)}
  }

  def addRoleAssertion(ra: RoleAssertion) = {
    logger.trace(s"Adding role assertion ${ra}")
    ra match {
      case RoleAssertion(_, a, b) => {
        individualRoles.addBinding(a, ra)
        individualRolesInv.addBinding(b, ra)
      }
    }
  }

  final def addABoxClause(_clause: ABoxClause): Unit = {
    logger.debug(s"Adding ${_clause}")

    //val clause = subsumptionChecker.condenseClause(_clause)
    //logger.debug(s"${if(clause!=_clause) "condensed: "+clause}")
    // <-- redundant in most cases: inferred clauses get condensed before we continue

    if(redundant(_clause)) {
      //      println(clause+ " subsumed!")
      logger.debug(s"New clause ${_clause} is subsumed")
    } else
      _addABoxClause(_clause)
  }

  def _addABoxClause(clause: ABoxClause): ABoxClause = {

    reduceClauseSets(clause)
    updateTables(clause)
    clause.literals.keys.foreach{ ind => allABox.addBinding(ind, clause) }

    clause
  }

  final def addConceptClause(_clause: ConceptClause): Unit = {
    logger.debug(s"Adding ${_clause}")

    assert(!_clause.literals.isEmpty, "empty clause derived! ontology inconsistent?")

    if(redundant(_clause)) {
      logger.debug(s"New clause ${_clause} is subsumed")
      return
    } else _clause.literals.head match {
      case ConceptLiteral(true, d: BaseConcept) if isDefiner(d) => {
        val resolutionCandidates = definingClauses.getOrElse(d, Set())
        resolutionRule.getDerivations(_clause, resolutionCandidates).foreach{ d =>
          logger.trace(s"  - Derivation on positive definer: $d")
          inferenceLogger.sendInference(d)
          d.conclusions.foreach(proceedTBox)
          // TODO check: is this done twice?
        }
      }
      case _ => _addConceptClause(_clause)
    }

  }

  def _addConceptClause(_clause: ConceptClause): ConceptClause = {
    val clause = subsumptionChecker.condenseClause(_clause)
    logger.debug(s"${if(clause!=_clause) "condensed: "+clause}")


    reduceClauseSets(clause)
    updateTables(clause)
    allTBox.add(clause)

    clause
  }

  def validResultClause(clause: ConceptClause): Boolean =
    clause.literals.filter(_ match {
      case ConceptLiteral(false, d: BaseConcept) if isDefiner(d) => true
      case _ => false
    }).size<2


  def proceedTBox(_clause: ConceptClause): Unit = {
    checkCanceled

    if(redundant(_clause)) {
      logger.debug(s"derived clause ${_clause} is subsumed")
      return
    }

    val clause = subsumptionChecker.condenseClause(_clause)
    logger.debug(s"derived clause: ${_clause}")

    val negDefiners = clause.literals.collect{
      case ConceptLiteral(false, c: BaseConcept) if isDefiner(c) => c
    }

    if(negDefiners.size<2){
      val posDefiner = clause.literals.collectFirst{
        case ConceptLiteral(true, c: BaseConcept) if isDefiner(c) => c
      }

      posDefiner match {
        case None =>
          addConceptClause(clause) // safe to filter out non-result clauses here?
        // no: mechanism used requires
        // additional res-steps when definers
        // are introduced
        case Some(definer: BaseConcept) =>
          val cand = definingClauses.getOrElse(definer, Set[ConceptClause]()).toSet
          getTBoxResolutionDerivations(clause, cand).foreach{ proceedTBox }
      }

    } else { // more than one negative definer!
      definerFactory.representative(negDefiners) match {
        case Some(_) => logger.trace("Combination already known.")
        // case None if negDefiners.exists(n1 => negDefiners.exists(n2 => cantCombine(Set(n1,n2)))) =>
        //   println("cant combine "+negDefiners)
        case None => {
          val (abox, tbox) = combineDefiners(negDefiners.toList) //cumulateDefinerCombination(negDefiners.head, negDefiners.tail.toList)
          abox.toSeq.sortBy(_.literals.size).foreach(proceedABox)
          tbox.toSeq.sortBy(_.literals.size).foreach(proceedTBox) // if there were more than 2 neg definers,
          // proceedTBox will trigger new definer combinations
        }
      }
      val premises = definingClauses.getOrElse(definerFactory.representative(negDefiners).get,
        negDefiners.flatMap(x => definerFactory.getBaseDefiners(x)).flatMap(definingClauses))
        //Set())
      val newClause = new ConceptClause(clause.literals--negDefiners.map(ConceptLiteral(false,_)) +
            ConceptLiteral(false,definerFactory.representative(negDefiners).get), ordering);

      if(!premises.isEmpty) { // TODO in what cases does this happen?
        inferenceLogger.sendInference(new Derivation(
          premises, Set(newClause), resolutionRule
        ))
      }
      proceedTBox(newClause)
    }
  }

  def combineDefiners(definers: List[BaseConcept])
  : (Set[ABoxClause], Set[ConceptClause]) = {
    if(definers.size<2)
      return (Set(),Set())
    else {
      val d1::rest = definers
      val d2::rest2 = rest
      val (result1, result2) = combineDefiners(d1,d2)
      definerFactory.representative(Set(d1,d2)) match {
        case None => // combining was not possible
          (Set(),Set())
        case Some(d3) =>
          val (result1rest,result2rest) = combineDefiners(d3::rest2)
          (result1++result1rest, result2++result2rest)
      }
    }
  }

  val cantCombine = new HashSet[Set[BaseConcept]]()

  def combineDefiners(d1: BaseConcept, d2: BaseConcept)
  :(Set[ABoxClause], Set[ConceptClause]) = {
    logger.trace(s"Combining $d1 and $d2")

    var abox = new HashSet[ABoxClause]()
    var tbox = new HashSet[ConceptClause]()
    var d3: BaseConcept = BaseConcept("")

    if(cantCombine(Set(d1, d2)))
      return (Set(), Set())

    definerFactory.representative(d1, d2) match {
      case Some(_d3) =>
        return (Set(), Set())
      case None => {
        logger.debug("Trigger!")


        // combine the clauses containing -d1 and -d2: definerFactory+resolution steps
        val (_d3, intermediateDefinitions) = definerFactory.combineDefiners(d1, d2)
        d3 = _d3

        var resolutionCandidates = definingClauses.getOrElse(d1, Set())++definingClauses.getOrElse(d2, Set())

        intermediateDefinitions.foreach(resolutionRule.getDerivations(_,
          resolutionCandidates).foreach{ d=>

          checkCanceled

          inferenceLogger.sendInference(d)

          logger.trace(s"  - Derivation: $d")

          d.conclusions.foreach{ proceedTBox } // adding clauses updates definerUsages, and has therefore
          // to be performed before the next step (combining usages).

        })

        // combining the clauses containining d1 and d2 positively
        logger.trace("Now checking role propagations")

        var dus1 = definerUsages.getOrElse(d1, Set())
        var dus2 = definerUsages.getOrElse(d2, Set())
        var dus1A = definerUsagesA.getOrElse(d1, Set())
        var dus2A = definerUsagesA.getOrElse(d2, Set())

        logger.trace(s"${d1} -> ${dus1}")
        logger.trace(s"${d2} -> ${dus2}")

        /////////////////////////////////////////////
        // APPLY ROLE ASSERTION PROPAGATION
        /////////////////////////////////////////////
        dus1.foreach(p1 =>
          individualRoles.values.foreach{
            checkCanceled
            _.foreach{ ra =>
              roleAssertionPropagationRule.applyAll(p1._2, ra).foreach(abox.add)
            }
          })

        dus1A.foreach(p1 =>
          individualRoles.values.foreach{
            checkCanceled
            _.foreach{ ra =>
              roleAssertionPropagationRule.applyAll(p1._3, ra).foreach(abox.add)
            }
          })

        if(abox.size>0) {
          dus2.foreach(p1 =>
            individualRoles.values.foreach{
              checkCanceled
              _.foreach{ ra =>
                roleAssertionPropagationRule.applyAll(p1._2, ra).forall(abox.add)
              }
            })

          dus2A.foreach(p1 =>
            individualRoles.values.foreach{
              checkCanceled
              _.foreach{ ra =>
                roleAssertionPropagationRule.applyAll(p1._3, ra).forall(abox.add)
              }
            })

          // following line seems to be meaningless (we are in an if block where
          // abox.size>0, and furthermore, emptying an empty ABox doesn't do
          // anything). Hence commented.
          //if(abox.isEmpty)
          //  abox = new HashSet[ABoxClause]()
        }

        var newUsages = true
        while(newUsages) {
          newUsages = false
          dus1.foreach { p1 =>
            val (l1, c1) = p1
            dus2.foreach { p2 =>
              val (l2, c2) = p2

              logger.trace(s"${p1} + ${p2}")

              checkCanceled
              rolePropagationRule.derive(c1, l1, c2,
                l2).foreach { conclusion =>
                //inferenceLogger.sendInference(Derivation(List(c1, c2), List(conclusion), rolePropagationRule))
                newUsages ||= updateTables(conclusion)
                tbox.add(conclusion) //
              }
              if(!inferenceLogger.equals(InferenceLogger.dummyLogger)){
                inferenceLogger.sendInference(
                  rolePropagationRule.fullDerivations(c1,l1,c2,l2)
                )
              }
            }
          }
          val dus1old= dus1
          val dus2old = dus2
          dus1 = definerUsages.getOrElse(d1, Set())
          dus2 = definerUsages.getOrElse(d2, Set())
          if(newUsages)
            logger.trace(s"new round in finding usages for ${d1} and ${d2}")
        }


        dus1.foreach(p1 =>
          dus2A.foreach(p2 =>
            deriveTBoxABox(p1._2, p1._1, p2._3, p2._1, p2._2).foreach(abox.add)))

        dus1A.foreach(p2 =>
          dus2.foreach(p1 =>
            deriveTBoxABox(p1._2, p1._1, p2._3, p2._1, p2._2).foreach(abox.add)))

        dus1A.foreach(p1 =>
          dus2A.foreach(p2 =>
            if(p1._1==p2._1) // same individual
              deriveABoxABox(p1._1, p1._3, p1._2, p2._3, p2._2).foreach(abox.add)))

        if(abox.isEmpty && tbox.isEmpty){
          logger.debug(s"cant combine $d1 and $d2")
          cantCombine.add(Set(d1, d2))
          return (Set(), Set())
        } else {
          (abox.toSet, tbox.toSet)
        }}
    }
  }




  def reduceClauseSets(clause: ConceptClause): Unit = {
    val redundant = allTBox.filter(subsumptionChecker.subsumes(clause,_))
    redundant.foreach(remove)

    allABox.keys.foreach{ ind =>
      if(allABox.keySet(ind)){
        val redundant = allABox(ind).filter(subsumes(clause,_))
        redundant.foreach(remove(_))
      }
    }
  }

  def reduceClauseSets(clause: ABoxClause): Unit = {
    clause.literals.keys.foreach{ ind =>
      if(allABox.keySet(ind)){
        val redundant = allABox(ind).filter(subsumes(clause,_))
        redundant.foreach(remove(_))
        // if(redundant.size>0)
        //   println("removed "+redundant)
      }
    }

  }

  def remove(clause: ConceptClause): Unit = {
    //allTBox.remove(clause)
    resultTBox.remove(clause)

    definingClauses.values.foreach(_.remove(clause)) // EXPENSIVE?
    //    definerUsages.values.foreach(_.remove(clause)) // EXPENSIVE?
  }

  def remove(clause: ABoxClause): Unit = {
    clause.literals.keySet.foreach(allABox.removeBinding(_, clause))
    resultABox.remove(clause)
  }

  def updateTables(clause: ConceptClause): Boolean = {
    var changed = false
    logger.trace(s"updating tables for ${clause} ")
    clause.literals.foreach{ literal =>
      literal match {
        case ConceptLiteral(false, d: BaseConcept) if isDefiner(d) => definingClauses.addBinding(d, clause)
        case ConceptLiteral(true, ExistentialRoleRestriction(_,d: BaseConcept))
          if(!definerUsages.contains(d) || !definerUsages(d).contains((literal,clause))) =>
            logger.trace(s"new definer usage for ${d}: ${clause}")
            changed = true
            definerUsages.addBinding(d, (literal, clause))
        case ConceptLiteral(true, UniversalRoleRestriction(_,d: BaseConcept))
          if(!definerUsages.contains(d) || !definerUsages(d).contains((literal,clause))) =>
            logger.trace(s"new definer usage for ${d}: ${clause}")
            changed = true
            definerUsages.addBinding(d, (literal, clause))
        case _ =>
      }}
    changed
  }

  def updateTables(clause: ABoxClause): Unit = {
    clause.literals.keys.foreach{ ind => clause.literals(ind).literals.foreach { literal => literal match {
      case ConceptLiteral(true, ExistentialRoleRestriction(_,d: BaseConcept)) =>
        definerUsagesA.addBinding(d, (ind, literal, clause))
      case ConceptLiteral(true, UniversalRoleRestriction(_,d: BaseConcept)) =>
        definerUsagesA.addBinding(d, (ind, literal, clause))
      case _ =>
    }}}
  }

  def redundant(clause: ConceptClause): Boolean =
    allTBox.exists(subsumptionChecker.subsumes(_, clause))

  def redundant(clause: ABoxClause): Boolean =
    clause.literals.values.exists(redundant)
    // TODO can we use proper subsumption between ABox clauses here?


  def subsumes(clause1: ConceptClause, clause2: ABoxClause) = {
    clause2.literals.values.exists(subsumptionChecker.subsumes(clause1,_))
  }

  def subsumes(clause1: ABoxClause, clause2: ABoxClause) = {
    clause1==clause2||
      clause1.literals.keys.forall{ ind =>
        clause2.literals.keySet(ind) && subsumptionChecker.subsumes(clause1.literals(ind), clause2.literals(ind))
      }
  }

  def isDefiner(d: BaseConcept) = ALCFormulaPreparations.isDefiner(d)

  override def notifyClauseAddition(clauses: Set[ConceptClause]) = {
    clauses.foreach(addConceptClause)
  }
}
