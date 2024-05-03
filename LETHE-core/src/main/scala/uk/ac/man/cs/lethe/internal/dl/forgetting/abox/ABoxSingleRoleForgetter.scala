package uk.ac.man.cs.lethe.internal.dl.forgetting.abox

import uk.ac.man.cs.lethe.internal.dl.AbstractMappedReasonerFactory
import uk.ac.man.cs.lethe.internal.dl.forgetting.abox.ABoxSingleRoleForgetter.alwaysPropagate
import uk.ac.man.cs.lethe.internal.dl.forgetting.direct._
import uk.ac.man.cs.lethe.internal.dl.proofs.InferenceLogger

import scala.collection.mutable.{HashMap, HashSet, MultiMap, TreeSet, Set => mutSet}

//import com.dongxiguo.fastring.Fastring.Implicits._
import com.typesafe.scalalogging.Logger

import uk.ac.man.cs.lethe.internal.dl.datatypes._
import uk.ac.man.cs.lethe.internal.forgetting.Forgetter

/**
 * TODO: currently we only log inferences of the role resolution rule.
 *
 * If we have role hierarchies or ABox axioms, the logging will thus be incomplete
 */

object ABoxSingleRoleForgetter {
  var alwaysPropagate=false // used for generating proofs from inference logging
}

class ABoxSingleRoleForgetter(roleHierarchy: RoleHierarchy,
                              inverseRoles: Boolean = false,
                              reasonerFactory: AbstractMappedReasonerFactory)
  extends AbstractABoxClauseForgetter[BaseRole](roleHierarchy, inverseRoles) {

  //override implicit val (logger, formatter, appender) = ZeroLoggerFactory.newLogger(ABoxSingleConceptForgetter)
  //import formatter._
  override val logger = Logger[ABoxSingleRoleForgetter]

  var forgetRole: BaseRole = _


  var univTBox: mutSet[ConceptClause] = _
  var univABox: MultiMap[Individual, ABoxClause] = _

  var exisTBox: mutSet[ConceptClause] = _
  var exisABox: MultiMap[Individual, ABoxClause] = _

  var backgroundTBox: Set[Axiom] = Set[Axiom]()

  var forgetRolePropagationRule: LiteralBasedRule = _

  override def init() = {
    super.init()


    univABox = new HashMap[Individual, mutSet[ABoxClause]]() with MultiMap[Individual, ABoxClause]
    exisABox = new HashMap[Individual, mutSet[ABoxClause]]() with MultiMap[Individual, ABoxClause]
  }


  def setForgetRole(role: BaseRole) = {
    assert(role.roleSymbols.size == 1)

    this.forgetRole = role

    ordering = new ConceptLiteralOrdering(Seq(forgetRole.name))


    definerFactory = new DefinerFactory(ALCFormulaPreparations, ordering, roleHierarchy)
    definerFactory.addListener(this)
    inferenceLogger.notifyDefinerFactory(definerFactory)
    subsumptionChecker.setDefinerFactory(definerFactory)
    subsumptionChecker.setIgnoreRole(forgetRole)

    clauseOrdering = new ConceptClauseOrdering(ordering)
    resultTBox = new TreeSet[ConceptClause]()(clauseOrdering)
    exisTBox = new TreeSet[ConceptClause]()(clauseOrdering)
    univTBox = new TreeSet[ConceptClause]()(clauseOrdering)

    allTBox = new TreeSet[ConceptClause]()(clauseOrdering)
    allABox = new HashMap[Individual, mutSet[ABoxClause]] with MultiMap[Individual, ABoxClause]
  }

  def setBackground(tboxClauses: Set[Axiom]) =
    backgroundTBox = tboxClauses

  def instantiateRoleAssertions() = {
    individualRoles.values.flatten.foreach{ ra =>
      checkCanceled
      // TODO no inference logging for inferences on role assertions yet, as those are not yet supported by the Derivation class
      ra match {
      case RoleAssertion(r, a, b) if r==forgetRole => {
        logger.trace(s"checking for $ra")
        logger.trace(s" in ${univABox.get(a)}")
        univABox.get(a).foreach(_.foreach(roleAssertionPropagationRule.applyAll(_, ra).foreach(proceedABox)))
        logger.trace(s" and in $univTBox")
        univTBox.flatMap(roleAssertionPropagationRule.applyAll(_, ra)).foreach(proceedABox)
        individualRoles.removeBinding(a, ra)
      }
      case RoleAssertion(InverseRole(r), a, b) if r==forgetRole => {
        logger.trace(s"checking for $ra")
        logger.trace(s" in ${univABox.get(a)}")
        univABox.get(a).foreach(_.foreach(roleAssertionPropagationRule.applyAll(_, ra).foreach(proceedABox)))
        logger.trace(s" and in $univTBox")
        univTBox.flatMap(roleAssertionPropagationRule.applyAll(_, ra)).foreach(proceedABox)
        individualRoles.removeBinding(a, ra)
      }
      case _ => ;
    }}
  }

  def roleMonotonicity(ra: RoleAssertion) = ra match {
    case RoleAssertion(r: BaseRole, a, b) => {
      assert(r==forgetRole)
      roleHierarchy.getDirectSuperRoles(r).foreach{ r2 =>
        if(r2!=r && r2!=DLHelpers.inverse(r))
          addRoleAssertion(RoleAssertion(r2, a, b))
      }
    }
  }

  private def needsRoleResolution =
    !roleHierarchy.directSuperRoles.contains(forgetRole) ||
    roleHierarchy.directSuperRoles(forgetRole).isEmpty


  override def derive(): Unit = {

    logger.debug(s"")

    if(needsRoleResolution) {
      val mappedReasoner = reasonerFactory.newReasoner(allTBox, backgroundTBox,
        roleHierarchy.rbox)

      //    println("TBox: "+allTBox)
      roleResolutionRule = new RoleResolutionRule(ordering,
        forgetRole,
        definerFactory,
        mappedReasoner)
    }
    if(!needsRoleResolution || alwaysPropagate) {
      forgetRolePropagationRule =
        new SimpleRolePropagationForgetRule(forgetRole,
          roleHierarchy,
          definerFactory)
    }


    while(!exisABox.isEmpty || !univABox.isEmpty || !exisTBox.isEmpty) {

      instantiateRoleAssertions()

      //      println("Hey!")
      while (!exisABox.isEmpty) {
        val nextInd = exisABox.keys.head
        val next = exisABox(nextInd).head
        exisABox.removeBinding(nextInd, next)

        logger.debug(s"\nInferences on existential in ${next} for ${nextInd}")

        var derivations = inferencesWithABoxExistential(
          next,
          univABox.getOrElse(nextInd, Set()).toSet,
          univTBox.toSet,
          nextInd)

        derivations.toSeq.sortBy(_.size).foreach {
          proceedABox
        }
      }

      if (!univABox.isEmpty) {
        val nextInd = univABox.keys.head
        val next = univABox(nextInd).head
        univABox.removeBinding(nextInd, next)

        logger.debug(s"\nInferences on universal in ${next} for ${nextInd}")

        var derivations = inferencesWithABoxUniversal(next,
          exisTBox.toSet,
          nextInd)

        derivations.toSeq.sortBy(_.size).foreach {
          proceedABox
        }
      }


      while (!exisTBox.isEmpty) {
        val next = exisTBox.head
        exisTBox.remove(next)

        logger.debug(s"\nResolving on ${next}")
        //      println(univTBox)

        inferencesWithTBoxExistential(next,univTBox).foreach(proceedTBox)

        // TODO inferenceLogger is not taking care of the inferences with the ABox
        var derivations2 = getTBoxWithABoxInferences(next)
        derivations2.toSeq.sortBy(_.literals.size).foreach {
          proceedABox
        }
      }
    }
  }


  def inferencesWithABoxExistential(clause: ABoxClause,
                                    aboxUniversals: Set[ABoxClause],
                                    tboxUniversals: Set[ConceptClause],
                                    individual: Individual): Iterable[ABoxClause] = {
    if(needsRoleResolution)
      getABoxRoleResolutionDerivations(clause,
        aboxUniversals,
        tboxUniversals,
        individual)
    else {

      checkCanceled

      val conceptClause = clause.literals(individual)

      var result = List[ABoxClause]()

      // first: inferences with universal restrictions in the TBox
      inferencesWithTBoxExistential(conceptClause, tboxUniversals).foreach{ conclusion =>
        result = clause.replace(individual, conclusion)::result
      }

      // next: inferences with universal restrictions in the ABox
      aboxUniversals.foreach{ aboxUniversal =>
                val conceptClause2 = aboxUniversal.literals(individual)
                inferencesWithTBoxExistential(conceptClause, Some(conceptClause2)).foreach{
                  conceptConclusion =>
                    result = clause.combineWith(aboxUniversal).replace(individual,
                      conceptConclusion)::result
                }
      }

      result
    }
  }

  def inferencesWithABoxUniversal(clause: ABoxClause,
                                  tboxExistentials: Iterable[ConceptClause],
                                  individual: Individual) = {
        getTBoxRolePropagationDerivationsEx(clause, tboxExistentials, individual)
  }

  /**
   * pure TBox inferences, using role resolution or forget role propagation.
   */
  def inferencesWithTBoxExistential(clause: ConceptClause,
                                    tboxUniversals: Iterable[ConceptClause]): Iterable[ConceptClause] = {

    logger.debug(s"inferences with TBox ex: ${clause}")


    var result = Set[ConceptClause]()

    if(needsRoleResolution) {
      var newPremises = tboxUniversals.toList
      while (!newPremises.isEmpty) {
        logger.trace(s"new premises: ${newPremises}")
        val oldPremises = newPremises
        newPremises = List[ConceptClause]()
        roleResolutionRule.getDerivations(clause, oldPremises).foreach { d =>
          inferenceLogger.sendInference(d)
          d.conclusions.map(_.asInstanceOf[ConceptClause]).foreach { c =>
            if (!result.contains(c)) {
              result += c
              newPremises ::= c
            }
          }
        }
      }
    }
    if(alwaysPropagate || !needsRoleResolution){

      checkCanceled


      assert(clause.literals.head.concept.isInstanceOf[ExistentialRoleRestriction])

      var premisesToProcess = tboxUniversals.toList
      var premiseCandidates = tboxUniversals.toSet

      while(!premisesToProcess.isEmpty){


        logger.trace("premisesToProcess: "+premisesToProcess)

        val clause2 = premisesToProcess.head
        premisesToProcess = premisesToProcess.tail

        getForgetUniversalLiterals(clause2).foreach { universal =>

          val newInferences = forgetRolePropagationRule.derive(
            clause,
            clause.literals.head,
            clause2,
            universal)

          inferenceLogger.sendInference(Derivation(
            premisses = Set(clause, clause2),
            conclusions = newInferences,
            rule = forgetRolePropagationRule))

          newInferences.foreach(inf =>
            if(!premiseCandidates.contains(inf)){
              premisesToProcess::=inf
              premiseCandidates += inf
            }
          )

          newInferences.filterNot(result).foreach(cl => result ++=
            inferencesWithTBoxExistential(cl, premiseCandidates))

          result ++= newInferences

          // TODO can be optimised: subsets processed more than once with this recursive method
        }
      }
    }

    result
  }

  /**
   * return literals in clause with a universal role on the current concept to be forgotten.
    *
   * @param clause
   * @return
   */
  def getForgetUniversalLiterals(clause: ConceptClause): Iterable[ConceptLiteral] = {
    var done = false
    var result = List[ConceptLiteral]()
    clause.literals.foreach{ l =>
      if(!done) l.concept match {
        case UniversalRoleRestriction(role, _) if role.equals(forgetRole) =>
          result = l::result
        case _ => done = true // assuming an ordering here
      }
    }
    result
  }

  def getTBoxWithABoxInferences(clause: ConceptClause): Iterable[ABoxClause] =
      univABox.keySet.toSet[Individual].flatMap(ind =>
        getTBoxWithABoxInferences(clause,
          univABox(ind).toSet[ABoxClause],
          univTBox.toSet,
          ind))

  def getTBoxWithABoxInferences(clause: ConceptClause,
                                aboxUniversals: Set[ABoxClause],
                                tboxUniversals: Set[ConceptClause],
                                individual: Individual): Iterable[ABoxClause] = {
    if(needsRoleResolution)
      getTBoxRoleResolutionDerivations(clause,
        aboxUniversals,
        univTBox.toSet,
        individual)
    else {
        checkCanceled

        // in this case tboxUniversals not relevant, as we do the role propagation step-wise

        var result = List[ABoxClause]()

        aboxUniversals.foreach{
          aboxClause =>
            result ++= inferencesWithABoxUniversal(aboxClause, Some(clause),individual)
        }
        result
    }
  }


  override def _addABoxClause(_clause: ABoxClause): ABoxClause = {

    val clause = super._addABoxClause(_clause)
    var result = true
    clause.literals.keys.foreach{ ind =>
      clause.literals(ind).literals.head.concept match {
        case UniversalRoleRestriction(r: BaseRole, _) if r==forgetRole => univABox.addBinding(ind, clause); result=false
        case ExistentialRoleRestriction(r: BaseRole, _) if r==forgetRole => exisABox.addBinding(ind, clause); result=false
        case UniversalRoleRestriction(InverseRole(r: BaseRole), _) if r==forgetRole => univABox.addBinding(ind, clause); result=false
        case ExistentialRoleRestriction(InverseRole(r: BaseRole), _) if r==forgetRole => exisABox.addBinding(ind, clause); result=false
        case _ => ;
      }
    }
    if(result){
      logger.trace(s"new result clause!")
      resultABox.add(clause)
    } else {
      addMonotonicityResults(clause)
    }

    clause
  }


  def addMonotonicityResults(clause: ABoxClause): Unit = {
    val add = applyMonotonicity(clause, forgetRole)

    logger.debug(s"monotonicity results: ${add}")

    add.foreach(addABoxClause)
  }

  def applyMonotonicity(clause: ABoxClause, role: BaseRole): Iterable[ABoxClause] = {
    logger.info(s"Applying monotonicity for ${role} for ${clause}")

    def inner(restClause: List[(Individual, ConceptClause)]): Iterable[ABoxClause] = restClause match {
      case (ind, clause)::rest => applyMonotonicity(Set(clause), role).flatMap{ clauseX =>
        val head = new ABoxClause(ind, clauseX)
        inner(rest).map(cl => ABoxClause.combine(Set(head, cl)) )
      }
        // REMARK: changed 5.1.2020 (bug?)
      //case Nil => Set()
      case Nil => Set(new ABoxClause())
    }


    inner(clause.literals.toList)
  }


  override def _addConceptClause(_clause: ConceptClause): ConceptClause = {
    val clause = super._addConceptClause(_clause)

    var result = true
    var exists = false

    clause.literals.head.concept match {
      case UniversalRoleRestriction(r: BaseRole, _) if r==forgetRole => univTBox.add(clause); result = false
      case ExistentialRoleRestriction(r: BaseRole, _) if r==forgetRole => exisTBox.add(clause); result = false;
        exists=true
      case UniversalRoleRestriction(InverseRole(r: BaseRole), _) if r==forgetRole => univTBox.add(clause);
        result = false
      case ExistentialRoleRestriction(InverseRole(r: BaseRole), _) if r==forgetRole => exisTBox.add(clause);
        result = false; exists=true
      case _ if validResultClause(clause) => logger.debug(s"new result clause!"); resultTBox.add(clause)
      case _ => ;
    }

    if(exists) {
      // universal restrictions can hide behind existential restrictions
      clause.literals.tail.find(_.concept.isInstanceOf[UniversalRoleRestriction]) match {
        case Some(ConceptLiteral(true,UniversalRoleRestriction(r: BaseRole, _))) if r==forgetRole =>
          univTBox.add(clause); result = false
        case Some(ConceptLiteral(true,UniversalRoleRestriction(InverseRole(r: BaseRole), _))) if r==forgetRole =>
          univTBox.add(clause); result = false
        case _ => ;
      }
    }

    // assert(!resultTBox.exists(_.roles(forgetRole)), clause+", "+clause.literals.head)
    // <-- expensive test!

    if(result){
      logger.trace(s"new result clause!")
    } else {
      addMonotonicityResults(clause)
    }

    clause
  }

  def addMonotonicityResults(clause: ConceptClause): Unit = {
    val add = applyMonotonicity(Set(clause), forgetRole)
      logger.debug(s"monotonicity results: ${add}")
    add.foreach(addConceptClause)
  }

  def applyMonotonicity(clauses: Iterable[ConceptClause], role: BaseRole): Iterable[ConceptClause] = {

    checkCanceled

    logger.info(s"Applying monotonicity for ${role} for ${clauses}")

    def inner(literals: List[ConceptLiteral], currentResult: List[ConceptLiteral]): Set[ConceptClause] = literals match {
      case ConceptLiteral(true, ExistentialRoleRestriction(r, c)) :: rest if r.signature.contains(role.name) =>
        (roleHierarchy.getDirectSuperRoles(r) - DLHelpers.inverse(r) - r).flatMap{r2 =>

          inner(rest, ConceptLiteral(true, ExistentialRoleRestriction(r2, c)) :: currentResult)
        }
      case ConceptLiteral(true, UniversalRoleRestriction(r, c)) :: rest if r.signature.contains(role.name) =>
        (roleHierarchy.getDirectSubRoles(r) - DLHelpers.inverse(r) - r).flatMap(r2 =>
          inner(rest, ConceptLiteral(true, UniversalRoleRestriction(r2, c)) :: currentResult))
      case c :: rest =>
        inner(rest, c :: currentResult)
      case Nil =>
        Set(new ConceptClause(currentResult, ordering))
    }

    // clauses.map(cl => cl.literals.map{ _ match { 
    //   case ConceptLiteral(true, ExistentialRoleRestriction(r: Role, c)) if r.signature.contains(role.name) => 
    // 	roleHierarchy.getDirectSuperRoles(r).map{ r2 => 
    // 	  ConceptLiteral(true, ExistentialRoleRestriction(r2, c))
    //   } 
    //   case ConceptLiteral(true, UniversalRoleRestriction(r: Role, c)) if r.signature.contains(role.name) => 
    // 	roleHierarchy.getDirectSubRoles(r).map{ r2 => 
    // 	  ConceptLiteral(true, UniversalRoleRestriction(r2, c))
    //   }
    //   case cl => Set(cl)				     

    // }}).flatMap { set => 
    //   attach(set).map(new ConceptClause(_))
    // }		 

    def innerLogInference(toVisit: List[ConceptLiteral],
                          visited: List[ConceptLiteral],
                          premises: List[Expression]):Unit = toVisit match {
      case ConceptLiteral(true, ExistentialRoleRestriction(r, c)) :: rest if r.signature.contains(role.name) =>
        (roleHierarchy.getDirectSuperRoles(r) - DLHelpers.inverse(r) - r).foreach( r2 =>
          innerLogInference(rest, ConceptLiteral(true, ExistentialRoleRestriction(r2, c)) :: visited,
            RoleSubsumption(r,r2)::premises)
          )
      case ConceptLiteral(true, UniversalRoleRestriction(r, c)) :: rest if r.signature.contains(role.name) =>
        (roleHierarchy.getDirectSubRoles(r) - DLHelpers.inverse(r) - r).foreach( r2 =>
          innerLogInference(rest, ConceptLiteral(true, UniversalRoleRestriction(r2, c)) :: visited,
            RoleSubsumption(r2,r)::premises)
        )
      case c :: rest =>
        innerLogInference(rest, c :: visited, premises)
      case Nil =>
        inferenceLogger.sendInference(
          ExtendedDerivation(premises,
            Set(new ConceptClause(visited, ordering)), "MonotonicityRule"))
    }

    if(!inferenceLogger.equals(InferenceLogger.dummyLogger)){
      clauses.foreach{cl => innerLogInference(cl.literals.toList, List(), List[Expression](cl))}
    }

    clauses.flatMap{cl => inner(cl.literals.toList, List())}
  }


  override def remove(clause: ConceptClause) = {
    super.remove(clause)
    univTBox.remove(clause)
    exisTBox.remove(clause)
  }

  override def remove(clause: ABoxClause) = {
    super.remove(clause)
    clause.literals.keySet.foreach{ individual =>
      univABox.removeBinding(individual, clause)
      exisABox.removeBinding(individual, clause)
    }
  }


  override def redundant(clause: ConceptClause): Boolean =
    if(needsRoleResolution)
      super.redundant(clause)
    else
      // the normal redundancy test against all clauses is not sufficient anymore here,
      // since role monotonicity results become directly redundant and are thus not added
      // to the result.
      resultTBox.exists(subsumptionChecker.subsumes(_, clause))
  // TODO check whether this can be improved, could it introduce a termination problem?
}

