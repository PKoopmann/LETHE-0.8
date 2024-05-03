package uk.ac.man.cs.lethe.internal.dl.abduction.forgetting

import uk.ac.man.cs.lethe.internal.dl.datatypes.extended.ExtendedABoxClause
import uk.ac.man.cs.lethe.internal.dl.forgetting.InverseRoleProcessor
import uk.ac.man.cs.lethe.internal.dl.forgetting.abox.SimpleRolePropagationRule
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

import java.lang.System

/**
 *
 */
abstract class AbstractExtendedABoxClauseForgetter[T]()
  extends Timeoutable with ClauseSetListener
{

  //implicit val (logger, formatter, appender) = ZeroLoggerFactory.newLogger(ABoxSingleConceptForgetter)
  //import formatter._
  val logger = Logger[AbstractExtendedABoxClauseForgetter[T]]

  val rememberDefiners = false

  var inferenceLogger: AbstractInferenceLogger = InferenceLogger.dummyLogger

  var backgroundABox: mutSet[ExtendedABoxClause] = _
  var backgroundTBox: mutSet[ConceptClause] = _

  var ordering: Ordering[ConceptLiteral] = _
  var clauseOrdering: Ordering[ConceptClause] = _

  var allTBox: mutSet[ConceptClause] = new HashSet[ConceptClause]()
  var allABox: MultiMap[Individual, ExtendedABoxClause] = new HashMap[Individual, mutSet[ExtendedABoxClause]]
    with MultiMap[Individual, ExtendedABoxClause]
  var allABoxSet = new HashSet[ExtendedABoxClause]()

  var definerFactory: DefinerFactory = _
  val subsumptionChecker =
    new SimpleSubsumptionChecker() with RoleSubsumptionChecker with DefinerSubsumptionChecker


  var resultABox: TreeSet[ExtendedABoxClause] = _
  var resultTBox: TreeSet[ConceptClause] = _


  var definingClauses = new HashMap[BaseConcept, mutSet[ConceptClause]]()
    with MultiMap[BaseConcept, ConceptClause]

  var definerUsages = new HashMap[BaseConcept, mutSet[(ConceptLiteral, ConceptClause)]]()
    with MultiMap[BaseConcept, (ConceptLiteral, ConceptClause)]

  var definerUsagesA: MultiMap[BaseConcept,
    (Individual, ConceptLiteral, ExtendedABoxClause)] = _

  var negDefinerABox = new HashMap[BaseConcept, mutSet[(Individual, ExtendedABoxClause)]]()
    with MultiMap[BaseConcept, (Individual, ExtendedABoxClause)]

  var resolutionRule: ResolutionRule = _

  var roleAssertionPropagationRule = new RoleAssertionPropagationRule()
  var rolePropagationRule: SimpleRolePropagationRule = _
  var roleResolutionRule: RoleResolutionRule = _
  var transitivityRule: TransitivityRule = _

  // maps individual to clauses where it occurs in a role assertion on the first
  // position
  var roleAssertions1st = new HashMap[Individual, mutSet[ExtendedABoxClause]]()
    with MultiMap[Individual, ExtendedABoxClause]
  // maps individual to clauses where it occurs in a role assertion on the second
  // position
  var roleAssertions2nd = new HashMap[Individual, mutSet[ExtendedABoxClause]]()
    with MultiMap[Individual, ExtendedABoxClause]


  def setBackground(tboxBackground: Set[ConceptClause], aboxBackground: Set[ExtendedABoxClause]) = {

    backgroundTBox = new mutable.HashSet[ConceptClause]()
    backgroundABox = new mutable.HashSet[ExtendedABoxClause]()

    val orderedTBox = tboxBackground.map(_.withOrdering(ordering))
    orderedTBox.foreach{cl =>
      _addConceptClause(cl) // no redundancy checks, since likely in allTBox
      backgroundTBox.add(cl)
    }
    resultTBox --= orderedTBox // for redundancy, and to avoid endless loops

    val orderedABox = aboxBackground.map(_.withOrdering(ordering))
    orderedABox.foreach{ cl =>
      _addABoxClause(cl) // no redundancy checks, since likely in allABox
      backgroundABox.add(cl)
    }
    resultABox --= orderedABox // for redundancy, and to avoid endless loops

    logger.trace(s"Current Result:")
    logger.trace(s"${resultTBox}")
    logger.trace(s"${resultABox}")
    logger.trace(s"end of current result.")
  }


  def forget(aboxClauses: Set[ExtendedABoxClause], definitions: Set[ConceptClause])
  : (Set[ExtendedABoxClause], Set[ConceptClause])= {

    startTiming

    checkCanceled

    logger.info(s"Input: \n ${aboxClauses.mkString("\n")} \n ${definitions.mkString("\n")}")

    // we have to initialise so that we don't forget the backgroundW
    //init()

    aboxClauses.map(_.withOrdering(ordering)).foreach{
      cl =>
    //  if(!allABoxSet(cl))
    //    addABoxClause(cl) // if we don't know the clause, do normal redundancy check
    //  else
        _addABoxClause(cl) // if we know the clause, redundancy check would delete it
      // before we can do further reasoning
      // but we can assume redundancy checks to have applied when the clause was first added
    }

    definitions.map(_.withOrdering(ordering)).foreach{ cl =>
    //  if(!allTBox(cl))
    //    addConceptClause(cl)
    //  else
        _addConceptClause(cl)
    }

    logger.trace(s"Current Result:")
    logger.trace(s"${resultTBox}")
    logger.trace(s"${resultABox}")
    logger.trace(s"end of current result.")


    preprocessClauses()

    logger.trace("background:")
    logger.trace(s"${backgroundABox.mkString("\n")}")
    logger.trace(s"${backgroundTBox.mkString("\n")}")
    logger.trace("main clauses:")
    logger.trace(s"${aboxClauses.mkString("\n")}")
    logger.trace(s"${definitions.mkString("\n")}")

    definerFactory.addListener(this)

    derive()

    definerFactory.removeListener(this)

    (resultABox.toSet, resultTBox.toSet)
  }

  def preprocessClauses() = {

  }

  def setDefinerFactory(definerFactory: DefinerFactory): Unit ={
    this.definerFactory = definerFactory
    inferenceLogger.notifyDefinerFactory(definerFactory)
    subsumptionChecker.setDefinerFactory(definerFactory)
  }

  def init() = {
    assert(ordering!=null)

    inferenceLogger.notifyDefinerFactory(definerFactory)

    subsumptionChecker.setDefinerFactory(definerFactory)

    resolutionRule = new ResolutionRule(ordering)
    resolutionRule.ignoreInvalid = false

    rolePropagationRule = new SimpleRolePropagationRule(EmptyRoleHierarchy, definerFactory)


    // ABox part
    resultABox = new TreeSet[ExtendedABoxClause]()(new ABoxClauseOrdering(clauseOrdering))
    definerUsagesA = new HashMap[BaseConcept,
      mutSet[(Individual, ConceptLiteral, ExtendedABoxClause)]]
      with MultiMap[BaseConcept, (Individual, ConceptLiteral, ExtendedABoxClause)]
    roleAssertions1st = new HashMap[Individual, mutSet[ExtendedABoxClause]]()
      with MultiMap[Individual, ExtendedABoxClause]
    roleAssertions2nd = new HashMap[Individual, mutSet[ExtendedABoxClause]]()
      with MultiMap[Individual, ExtendedABoxClause]

  }

  def derive(): Unit

  def proceedABox(_clause: ExtendedABoxClause): Unit = {
    checkCanceled

    logger.debug(s"derived clause: ${_clause}")

    if (redundant(_clause)) {
      logger.debug(s"derived clause ${_clause} is subsumed")
      //      println("Subsumed!")
      return
    }


    val clause = subsumptionChecker.condenseClause(_clause)

    logger.debug(s"${if (clause != _clause) "condensed: " + clause}")

    //    clause.literals.keys.foreach{ ind => allABox.addBinding(ind, clause) }

    var negDefiners = Set[(Individual,BaseConcept)]()
    var posDefiners = Set[(Individual, BaseConcept)]()
    var univRoles = Set[(Individual, UniversalRoleRestriction)]()

    clause.literals.keys.foreach{ ind =>
      clause.literals(ind).literals.foreach{ _ match {
        case ConceptLiteral(false, c: BaseConcept) if isDefiner(c) => negDefiners += ((ind, c))
        case ConceptLiteral(true, c: BaseConcept) if isDefiner(c) => posDefiners += ((ind, c))
        case ConceptLiteral(true, urr: UniversalRoleRestriction) => univRoles += ((ind, urr))
        case other => ; //ignore
      }}
    }


    if (posDefiners.size > 0) {
      //      assert(posDefiners.size==1, "clause shouldn't be possible: "+clause)
      // <--- if we forget roles, more is actually possible
      val (ind, definer) = posDefiners.head
      val cand = definingClauses.getOrElse(definer, Set[ConceptClause]()).toSet
      getTBoxResolutionDerivations(clause, cand, ind).foreach {
        proceedABox
      }
      // else if(negDefiners.isEmpty){
      //   addABoxClause(clause)
      // crucial difference to ABox forgetter: we need those classes, which is why the previous
      // code is commented
    } else {
      addABoxClause(clause) // we add also if it contains negative definer literal,
      // but we still make the other inferences
      // (this is crucial for the definer elimination with inverse roles, as used for
      // the forgetting procedure for abduction)

      if (negDefiners.size > 0) {

        // there is now two roles that are relevant:
        // 1) NegRoleAssertionIntroduction applies on a negative definer and a universal role restriction
        // 2) RoleAssertionPropagation introduces ABox clauses with positive definer, on which we can then
        // apply resolution.
        // those are the only cases where a negative definer in the premise plays a role.
        // the only remaining rule that would introduce a positive ABox definer is resolution on another
        // positive ABox definer.
        // we can omit this case since it would not give us any new inferences:
        // whatever inference introduced the negative definer can also applied to introduce the negation on
        // that other positive ABox definer. We thus get only inferences we would already have gotten due 2)

        val (ind, definer) = negDefiners.head
        logger.trace(s"Trigger for -${definer}(${ind}) in ${clause}")

        //////////
        // 1) Apply NegRoleAssertionIntroduction
        //////////
        logger.trace(s"definer usages in ABox: ${definerUsagesA}")
        definerUsagesA.get(definer).foreach(_.foreach{
          case (ind2: Individual, ConceptLiteral(true, UniversalRoleRestriction(r, _)), clause2) =>
            val newClause = NegRoleAssertionIntroductionRule.apply(clause, definer, ind,
              clause2, r, ind2)
            proceedABox(newClause)
          case other => ; // do nothing
        })

        ///////////
        // 2) Introduce clauses with that definer positively, for further reoslution inferences
        //////////
        val relRoleAssertions = roleAssertions2nd.getOrElse(ind, Set())

        val candidatesA = definerUsagesA.getOrElse(definer, Set())
        var news = relRoleAssertions.flatMap(rra => candidatesA.flatMap { c =>
          val (i, l, cl) = c
          logger.trace(s"check derivation on $c and $rra")
          roleAssertionPropagationRule.applyAll(cl, rra)
        })

        val candidates = definerUsages.getOrElse(definer, Set())
        news ++= relRoleAssertions.flatMap(rra => candidates.flatMap { c =>
          val (l, cl) = c
          logger.trace(s"Check derivation on $c and $rra")
          roleAssertionPropagationRule.applyAll(cl, rra)
        })
        // on the results, which contain a positive definer,
        // new resolutions can be applied on the clause we are currently
        // processing, which contained that definer negatively
        news.flatMap(getABoxResolutionDerivations(_, Set(clause), ind)).foreach {
          proceedABox
        }
        // something to do with the other clauses?
      }

      // now for the inferences on universal role restrictions
      univRoles.foreach{
        case (i1: Individual, UniversalRoleRestriction(role, definer: BaseConcept)) =>
          negDefinerABox.get(definer).foreach(_.foreach{
            case (i2: Individual, clause2: ExtendedABoxClause) =>
              proceedABox(NegRoleAssertionIntroductionRule.apply(clause2, definer, i2, clause, role, i1))
          })
      }
    }


  }

  /**
   * used on clauses that require resolution on the definer
   */
  def getTBoxResolutionDerivations(clause: ExtendedABoxClause,
                                   clauses: Set[ConceptClause], ind: Individual) = {
    checkCanceled

    logger.debug(s"tbox resolution derivations for ${clause}")
    var der = resolutionRule.getDerivations(clause.literals(ind),
      tboxAdmissibleFor(clause, clauses))

    inferenceLogger.sendInference(der)

    logger.debug(s"${der.mkString("\n")}")
    der.flatMap(_.conclusions).map(clause.replace(ind, _))
  }


  def getTBoxResolutionDerivations(clause: ConceptClause, clauses: Set[ConceptClause]) = {
    checkCanceled

    logger.debug(s"tbox resolution derivations for ${clause}")

    var der = resolutionRule.getDerivations(clause, tboxAdmissibleFor(clause,clauses))

    inferenceLogger.sendInference(der)

    logger.debug(s"${der.mkString("\n")}")
    der.flatMap(_.conclusions)
  }


  /**
   * used on clauses that require resolution on the definer
   */
  def getABoxResolutionDerivations(clause: ExtendedABoxClause,
                                   clauses: Set[ExtendedABoxClause],
                                   ind: Individual) = {
    checkCanceled

    aboxAdmissibleFor(clause,clauses).flatMap{ aboxClause =>
      var der = resolutionRule.getDerivations(clause.literals(ind),
        Set(aboxClause.literals(ind)))
      inferenceLogger.sendInference(der)

      der.flatMap(_.conclusions).map(cl =>
        clause.combineWith(aboxClause).replace(ind, cl))
    }
  }

  def getABoxRoleResolutionDerivations(clause: ExtendedABoxClause,
                                       abClauses: Set[ExtendedABoxClause],
                                       tbClauses: Set[ConceptClause],
                                       ind: Individual): Set[ExtendedABoxClause] = {
    getTBoxRoleResolutionDerivations(clause.literals(ind),
      abClauses,
      tbClauses,
      ind).map{ ab => clause.combineWith(ab).replace(ind,ab.literals(ind))}
  }


  def getTBoxRoleResolutionDerivations(clause: ConceptClause,
                                       abClauses: Set[ExtendedABoxClause],
                                       tbClauses: Set[ConceptClause],
                                       ind: Individual): Set[ExtendedABoxClause] = {
    checkCanceled

    val result = new HashSet[ExtendedABoxClause]()
    val usage = new HashMap[ConceptClause, mutSet[ExtendedABoxClause]]()
      with MultiMap[ConceptClause, ExtendedABoxClause]

    abClauses.foreach{ aboxClause =>
      usage.addBinding(aboxClause.literals(ind), aboxClause)
    }

    tbClauses.foreach{
      usage.addBinding(_, ExtendedABoxClause.empty)
    }

    val derivationsTBox = roleResolutionRule.getDerivations(clause, tboxAdmissibleFor(clause,tbClauses))


    val derivationsABox = roleResolutionRule.getDerivations(clause,
      tboxAdmissibleFor(clause, usage.keySet))

    // TODO: STOPPED HERE!

    val secondPremises = aboxAdmissibleFor(clause, abClauses).map(_.literals(ind))++
      tboxAdmissibleFor(clause, tbClauses)

    val derivations =
      roleResolutionRule.getDerivations(clause, secondPremises)

    derivationsABox.foreach{ derivation =>
      connect(derivation
        .premisses
        .map(usage).toList)
        // <-- inner expression: for each premise, a set of corresponding ABox clauses
        // after connect: one list for each assignment from those lists
        // note: tbox premises are represented as empty ABox clauses here
        .foreach{ aboxClauses: List[ExtendedABoxClause] =>
          val combined = ExtendedABoxClause.combine(aboxClauses)
          derivation.conclusions.foreach{ conc =>
            result.add(new ExtendedABoxClause((combined.literals-ind)+(ind -> conc),
              combined.roleAssertions, combined.negatedRoleAssertions))
          }
        }
    }
    result.toSet
  }

  def getTBoxRolePropagationDerivations(clause: ExtendedABoxClause,
                                        clauses: Set[ConceptClause],
                                        ind: Individual): Set[ExtendedABoxClause] = {
    checkCanceled
    rolePropagationRule.getDerivations(clause.literals(ind),
      tboxAdmissibleFor(clause,clauses),
      clause.literals(ind).literals.head).flatMap{
      d =>
        inferenceLogger.sendInference(d)
        d.conclusions.map(clause.replace(ind,_))
    }
  }

  def getABoxRolePropagationDerivations(clause: ExtendedABoxClause,
                                        clauses: Set[ExtendedABoxClause],
                                        ind: Individual): Set[ExtendedABoxClause] = {
    checkCanceled
    aboxAdmissibleFor(clause,clauses).flatMap{ cl2 =>
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

  def getTBoxRolePropagationDerivationsEx(clause: ExtendedABoxClause,
                                          clauses: Set[ConceptClause],
                                          ind: Individual): Set[ExtendedABoxClause] = {
    checkCanceled
    val lit1 = clause.literals(ind).literals.head
    tboxAdmissibleFor(clause,clauses).flatMap{ cl2 =>
      val lit2 = cl2.literals.find(_.concept.isInstanceOf[ExistentialRoleRestriction])
      if(lit2==None)
        Set[ExtendedABoxClause]()
      else {
        rolePropagationRule.derive(clause.literals(ind), lit1,
          cl2, lit2.get).map{ c =>
          clause.replace(ind, c)}
      }

    }
  }


  def getABoxRolePropagationDerivationsWithEx(clause: ExtendedABoxClause,
                                              clauses: Set[ExtendedABoxClause],
                                              ind: Individual): Set[ExtendedABoxClause] = {
    checkCanceled
    val lit1 = clause.literals(ind).literals.head
    aboxAdmissibleFor(clause,clauses).flatMap{ cl2 =>
      val combined = clause.combineWith(cl2)
      val lit2 = cl2.literals(ind).literals.find(_.concept.isInstanceOf[ExistentialRoleRestriction])
      if(lit2==None)
        Set[ExtendedABoxClause]()
      else
        rolePropagationRule.derive(clause.literals(ind), lit1,
          cl2.literals(ind), lit2.get).map{ c=>
          combined.replace(ind, c) }
    }
  }

  /**
   * Take a list [[a,b], [a], [b,c]]. compute product
   * [[a,a,b], [a,a,c], [b,a,b], [b,a,c]]
   *
   */
  protected def connect(lists: List[Iterable[ExtendedABoxClause]])
  : Iterable[List[ExtendedABoxClause]] = lists match {
    case Nil => List()
    case list::rest => list.flatMap(value => connect(rest).map(value::_))
  } // <--- Maybe later put into some tools package


  /**
   * Used for definer combination when processing new clauses with more than one definer.
   * Check for admissibility is not needed as the result always has to be added.
   */
  def deriveTBoxABox(clause1: ConceptClause, literal1: ConceptLiteral,
                     clause2: ExtendedABoxClause, individual: Individual,
                     literal2: ConceptLiteral) = {
    checkCanceled
    rolePropagationRule.derive(clause1, literal1,
      clause2.literals(individual), literal2).map { cl =>
      clause2.replace(individual, cl)
    }

  }


  def deriveABoxABox(individual: Individual,
                     clause1: ExtendedABoxClause, literal1: ConceptLiteral,
                     clause2: ExtendedABoxClause, literal2: ConceptLiteral) = {
    checkCanceled
    rolePropagationRule.derive(clause1.literals(individual), literal1,
      clause2.literals(individual), literal2).map { cl =>
      clause1.combineWith(clause2).replace(individual, cl)
    }
  }

  final def addABoxClause(_clause: ExtendedABoxClause): Unit = {
    logger.debug(s"Adding ${_clause}")

    //val clause = subsumptionChecker.condenseClause(_clause)
    //logger.debug(s"${if(clause!=_clause) "condensed: "+clause}")
    // <-- redundant in most cases: inferred clauses get condensed before we continue

    if(redundant(_clause)) {
      //      println(clause+ " subsumed!")
      logger.debug(s"New clause ${_clause} is subsumed")
    } else {
      pullDefinersFromBackground(_clause)
      _addABoxClause(_clause)
    }
    logger.trace(s"Current Result:")
    logger.trace(s"${resultTBox}")
    logger.trace(s"${resultABox}")
    logger.trace(s"end of current result.")

   // logger.debug("all ABox: "+allABoxSet.size+", result ABox: "+resultABox.size+", backgroundABox: "+backgroundABox.size)
  }

  def _addABoxClause(clause: ExtendedABoxClause): ExtendedABoxClause = {

    logger.trace(s"adding non-redundant ${clause}")

    reduceClauseSets(clause)
    updateTables(clause)

    clause
  }

  final def addConceptClause(_clause: ConceptClause): Unit = {
    logger.debug(s"Checking to add ${_clause}")

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
          // <-- for call in 515 it's not called, but also not necessary
        }
      }
      case _ => {
        pullDefinersFromBackground(_clause)
        _addConceptClause(_clause)
      }

    }

    logger.trace(s"Current Result:")
    logger.trace(s"${resultTBox}")
    logger.trace(s"${resultABox}")
    logger.trace(s"end of current result.")

  }

  def _addConceptClause(_clause: ConceptClause): ConceptClause = {
    logger.trace(s"adding non-redundant ${_clause}")
    val clause = subsumptionChecker.condenseClause(_clause)
    logger.debug(s"${if(clause!=_clause) "condensed: "+clause}")

    reduceClauseSets(clause)
    updateTables(clause)

    //logger.debug("all tbox: "+allTBox.size+", tbox result: "+resultTBox.size+", tbox background: "+backgroundTBox.size)
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
          val (abox, tbox) = combineDefiners(negDefiners.toList) //negDefiners.head, negDefiners.tail.head)

          abox.toSeq.sortBy(_.size).foreach(proceedABox)
          tbox.toSeq.sortBy(_.literals.size).foreach(proceedTBox) // if there were more than 2 neg definers, proceedTBox will trigger new definer combinations

          if(!(abox.isEmpty && tbox.isEmpty)) // this happens if definers can't be combined
            proceedTBox(new ConceptClause(clause.literals--negDefiners.map(ConceptLiteral(false,_)) +
              ConceptLiteral(false,definerFactory.representative(negDefiners).get), ordering))
        }
      }
    }
  }

  val cantCombine = new HashSet[Set[BaseConcept]]()

  def combineDefiners(definers: List[BaseConcept])
  : (Set[ExtendedABoxClause], Set[ConceptClause]) = {
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

  def combineDefiners(d1: BaseConcept, d2: BaseConcept)
  :(Set[ExtendedABoxClause], Set[ConceptClause]) = {
    logger.trace(s"Combining $d1 and $d2")

    var abox = new HashSet[ExtendedABoxClause]()
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
        val (_d3, intermediateDefinitions) =
          definerFactory.combineDefiners(d1, d2)
        d3 = _d3

        var resolutionCandidates =
          definingClauses.getOrElse(d1, Set())++
            definingClauses.getOrElse(d2, Set())

        intermediateDefinitions.foreach(
          resolutionRule.getDerivations(_,
            resolutionCandidates).foreach{ d=>

            checkCanceled

            inferenceLogger.sendInference(d)

            logger.trace(s"  - Derivation: $d")

            d.conclusions.foreach{ proceedTBox } // adding clauses updates definerUsages, and has therefore
            // to be performed before the next step (combining usages).

          })

        // combining the clauses containining d1 and d2 positively
        logger.trace("Now checking role propagations")

        val dus1 = definerUsages.getOrElse(d1, Set())
        val dus2 = definerUsages.getOrElse(d2, Set())
        val dus1A = definerUsagesA.getOrElse(d1, Set())
        val dus2A = definerUsagesA.getOrElse(d2, Set())

        logger.trace(s"${d1} -> ${dus1}")
        logger.trace(s"${d2} -> ${dus2}")

        dus1.foreach(p1 =>
          roleAssertions1st.values.foreach{
            checkCanceled
            _.foreach{ rac =>
              roleAssertionPropagationRule.applyAll(p1._2, rac).foreach(abox.add)
            }
          })

        dus1A.foreach(p1 =>
          roleAssertions1st.values.foreach{
            checkCanceled
            _.foreach{ rac =>
              roleAssertionPropagationRule.applyAll(p1._3, rac).foreach(abox.add)
            }
          })

        if(abox.size>0) {
          dus2.foreach(p1 =>
            roleAssertions1st.values.foreach{
              checkCanceled
              _.foreach{ rac =>
                roleAssertionPropagationRule.applyAll(p1._2, rac).forall(abox.add)
              }
            })

          dus2A.foreach(p1 =>
            roleAssertions1st.values.foreach{
              checkCanceled
              _.foreach{ ra =>
                roleAssertionPropagationRule.applyAll(p1._3, ra).forall(abox.add)
              }
            })
          // from the original ABox forgetter - apparently meaningless, thus
          // commented
          //if(abox.isEmpty)
          //  abox = new HashSet[ExtendedABoxClause]()
        }


        dus1.foreach{ p1 =>
          val (l1, c1) = p1
          dus2.foreach{ p2 =>
            val (l2, c2) = p2

            logger.trace(s"${p1} + ${p2}")

            checkCanceled
            rolePropagationRule.derive(c1, l1, c2,
              l2).foreach{conclusion =>
              inferenceLogger.sendInference(Derivation(List(c1,c2), List(conclusion), rolePropagationRule))
              tbox.add(conclusion)
            }

          }
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
    logger.trace(s"backwards subsumption")
    val redundant = allTBox.filter(subsumptionChecker.subsumes(clause,_))
    redundant.foreach(remove)

    allABox.keys.foreach{ ind =>
      if(allABox.keySet(ind)){
        val redundant = allABox(ind).filter(subsumes(clause,_))
        redundant.foreach(remove(_))
      }
    }
  }

  def reduceClauseSets(clause: ExtendedABoxClause): Unit = {
    clause.literals.keys.foreach{ ind =>
      if(allABox.keySet(ind)){
        val redundant = allABox(ind).filter(subsumes(clause,_))
        redundant.foreach(remove(_))
      }
    }

  }

  def remove(clause: ConceptClause): Unit = {
    logger.trace(s"removing ${clause}")
    resultTBox.remove(clause)
    allTBox.remove(clause)

    definingClauses.values.foreach(_.remove(clause)) // EXPENSIVE?


    // TODO removing clauses from definerUsages might be considered at later point, but could also
    // be expensive
    //definerUsages.values.foreach(_.remove(clause))
  }

  def remove(clause: ExtendedABoxClause): Unit = {
    clause.literals.keySet.foreach(allABox.removeBinding(_, clause))
    resultABox.remove(clause)
    //definerUsagesA.remove(clause)
    roleAssertions1st.values.foreach(_.remove(clause))
    roleAssertions2nd.values.foreach(_.remove(clause))

    allABoxSet.remove(clause)
  }

  def updateTables(clause: ConceptClause): Unit = {
    allTBox += clause
    clause.literals.foreach{ literal =>
      literal match {
        case ConceptLiteral(false, d: BaseConcept) if isDefiner(d) =>
          logger.trace(s"adding binding: defining clause for ${d} is ${clause}")
          definingClauses.addBinding(d, clause)
          logger.trace(s"defining clauses is now ${definingClauses}")
        case ConceptLiteral(true, ExistentialRoleRestriction(_,d: BaseConcept)) =>
          logger.trace(s"adding binding: clause using ${d} is ${clause}")
          definerUsages.addBinding(d, (literal, clause))
        case ConceptLiteral(true, UniversalRoleRestriction(_,d: BaseConcept)) =>
          logger.trace(s"adding binding: clause using ${d} is ${clause}")
          definerUsages.addBinding(d, (literal, clause))
        case _ =>
      }}
  }

  def updateTables(clause: ExtendedABoxClause): Unit = {
    allABoxSet += clause
    clause.literals.keys.foreach{ ind => clause.literals(ind).literals.foreach {
      literal => literal match {
        case ConceptLiteral(true, ExistentialRoleRestriction(_,d: BaseConcept)) =>
          definerUsagesA.addBinding(d, (ind, literal, clause))
        case ConceptLiteral(true, UniversalRoleRestriction(_,d: BaseConcept)) =>
          definerUsagesA.addBinding(d, (ind, literal, clause))
        case ConceptLiteral(false, definer: BaseConcept) if isDefiner(definer) =>
          negDefinerABox.addBinding(definer, (ind, clause))
        case _ =>
      }
    }}

    clause.literals.keys.foreach{ ind => allABox.addBinding(ind, clause) }

    clause.roleAssertions.foreach(_ match {
      case RoleAssertion(_,a,b) => {
        roleAssertions1st.addBinding(a, clause)
        roleAssertions2nd.addBinding(b, clause)

        // the following should stay at least for now as it is assumed elsewhere
        // that allABox contains all abox clauses
        allABox.addBinding(a, clause)
        allABox.addBinding(b, clause)
      }
    })
  }

  def redundant(clause: ConceptClause): Boolean =
    allTBox.contains(clause) ||
    allTBox.exists(subsumptionChecker.subsumes(_, clause))

  def redundant(clause: ExtendedABoxClause): Boolean =
    allABoxSet.contains(clause) ||
      clause.literals.values.exists(redundant) ||
      allABox.values.flatten.exists(subsumptionChecker.subsumes(_, clause))


  def subsumes(clause1: ConceptClause, clause2: ExtendedABoxClause) = {
    clause2.literals.values.exists(subsumptionChecker.subsumes(clause1,_))
  }

  def subsumes(clause1: ExtendedABoxClause, clause2: ExtendedABoxClause) = {
    clause1==clause2||
      subsumptionChecker.subsumes(clause1, clause2)
  }

  def isDefiner(d: BaseConcept) = ALCFormulaPreparations.isDefiner(d)

  def admissiblePremises(clause1: ExtendedABoxClause, clause2: ExtendedABoxClause) = {
    !(backgroundABox.contains(clause1) && backgroundABox.contains(clause2))
  }
  def admissiblePremises(clause1: ExtendedABoxClause, clause2: ConceptClause) = {
    !(backgroundABox.contains(clause1) && backgroundTBox.contains(clause2))
  }
  def admissiblePremises(clause1: ConceptClause, clause2: ConceptClause) = {
    !(backgroundTBox.contains(clause1) && backgroundTBox.contains(clause2))
  }
  def aboxAdmissibleFor(clause1: ExtendedABoxClause, clauses: Iterable[ExtendedABoxClause]) =
    clauses.filter(admissiblePremises(clause1,_)).toSet[ExtendedABoxClause]
  def tboxAdmissibleFor(clause1: ExtendedABoxClause, clauses: Iterable[ConceptClause]) =
    clauses.filter(admissiblePremises(clause1, _)).toSet[ConceptClause]
  def aboxAdmissibleFor(clause: ConceptClause, clauses: Iterable[ExtendedABoxClause]) =
    clauses.filter(admissiblePremises(_,clause)).toSet[ExtendedABoxClause]
  def tboxAdmissibleFor(clause: ConceptClause, clauses: Iterable[ConceptClause]) =
    clauses.filter(admissiblePremises(clause,_)).toSet[ConceptClause]


  /**
   * When a new clause is added that contains definers, we have to move corresponding clauses
   * from the background set of clauses to the main set of clauses.
   */
  def pullDefinersFromBackground(clause: ExtendedABoxClause): Unit = {
    clause.literals.values.foreach(pullDefinersFromBackground)
  }


  def pullDefinersFromBackground(clause: ConceptClause): Unit = {
    logger.trace(s"pulling definers in ${clause}")
    clause.literals.foreach {
      case ConceptLiteral(false, d: BaseConcept) if isDefiner(d) => pullQuantifiedFromBackground(d);pullDefining(d)
      case ConceptLiteral(true, UniversalRoleRestriction(_, d: BaseConcept)) => pullQuantifiedFromBackground(d);pullDefining(d)
      case ConceptLiteral(true, ExistentialRoleRestriction(_, d: BaseConcept)) => pullQuantifiedFromBackground(d);pullDefining(d)
      case other => ; // do nothing
    }
  }

  /**
   * We pull if a literal ¬D(x) or ¬D(a) has been inferred. Then, TBox clauses that have a literal
   * (forall r.D) have to be moved from the background to the main clause set, as they will be
   * used when definers get eliminated. For simplicity, we pull all definer usages.
   */
  def pullQuantifiedFromBackground(d: BaseConcept) = {
    logger.trace(s"pulling quantified occurrences of ${d}")
    definerUsages.get(d).foreach(_.foreach{
      case (_, clause) => pullFromBackground(clause)
    })
  }

  /**
   * We pull if a literal forall r.D or exists r.D has been inferred. Then, we need all clauses that
   * contain ¬D
   */
  def pullDefining(d: BaseConcept) = {
    logger.trace(s"pulling defining clauses of ${d}")
    logger.trace(s"defining clauses are ${definingClauses}")
    definingClauses.get(d).foreach(_.foreach{pullFromBackground})
  }

  def pullFromBackground(clause: ConceptClause) = {

  // crucial here: this can happen at most once per clause, as nothing adds clauses back to the background.
    // a second adding thus can't cause a termination problem
    if(backgroundTBox.contains(clause)){
      logger.trace(s"pulling ${clause} from background")
      backgroundTBox.remove(clause)

      // we have to add without redundancy check, as clause would be immediately redundant due allTBox
      //_addConceptClause(clause)

      // directly add to result, even if not in signature: we can deal with it in later forgetting iterations.
      // (adding the concept clause wouldn't do it, as we process the negative occurrences one after the other.
      // if the pulled clause is a positive occurrence, we miss inferences)
      pullDefinersFromBackground(clause) // recursively add what else we need (termination problem?)
      resultTBox += clause
    }
  }

  override def notifyClauseAddition(clauses: Set[ConceptClause]) = {
    logger.trace(s"got notified for new clauses: ${clauses}")
    clauses.foreach(addConceptClause)
  }
}
