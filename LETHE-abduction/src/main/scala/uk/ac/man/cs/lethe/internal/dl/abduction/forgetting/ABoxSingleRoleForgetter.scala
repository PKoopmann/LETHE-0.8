package uk.ac.man.cs.lethe.internal.dl.abduction.forgetting

import uk.ac.man.cs.lethe.internal.dl.datatypes.extended.ExtendedABoxClause
import uk.ac.man.cs.lethe.internal.dl.forgetting.abox.SimpleRolePropagationForgetRule
import uk.ac.man.cs.lethe.internal.dl.forgetting.direct._

import scala.collection.mutable.{HashMap, HashSet, MultiMap, TreeSet, Set => mutSet}

//import com.dongxiguo.fastring.Fastring.Implicits._
import com.typesafe.scalalogging.Logger

import uk.ac.man.cs.lethe.internal.dl.datatypes._
import uk.ac.man.cs.lethe.internal.forgetting.Forgetter


class ExtendedABoxSingleRoleForgetter()
  extends AbstractExtendedABoxClauseForgetter[BaseRole]() {

  //override implicit val (logger, formatter, appender) = ZeroLoggerFactory.newLogger(ABoxSingleConceptForgetter)
  //import formatter._
  override val logger = Logger[ExtendedABoxSingleRoleForgetter]

  var forgetRole: BaseRole = _


  var univTBox: mutSet[ConceptClause] = _
  var univABox: MultiMap[Individual, ExtendedABoxClause] = _

  var exisTBox: mutSet[ConceptClause] = _
  var exisABox: MultiMap[Individual, ExtendedABoxClause] = _

  var negRoleAssertions: MultiMap[RoleAssertion, ExtendedABoxClause] = _

  // background tbox only needed for role resolution, which we do not use here
  // var backgroundTBox: Set[Axiom] = Set[Axiom]()

  var forgetRolePropagationRule: LiteralBasedRule = _

  override def init() = {
    super.init()


    univABox = new HashMap[Individual, mutSet[ExtendedABoxClause]]()
      with MultiMap[Individual, ExtendedABoxClause]
    exisABox = new HashMap[Individual, mutSet[ExtendedABoxClause]]()
      with MultiMap[Individual, ExtendedABoxClause]
    negRoleAssertions = new HashMap[RoleAssertion, mutSet[ExtendedABoxClause]]()
      with MultiMap[RoleAssertion, ExtendedABoxClause]
  }


  def setForgetRole(role: BaseRole) = {
    assert(role.roleSymbols.size == 1)

    this.forgetRole = role

    ordering = new ConceptLiteralOrdering(Seq(forgetRole.name))

    //subsumptionChecker.setDefinerFactory(definerFactory)
    subsumptionChecker.setIgnoreRole(forgetRole)

    //    definerFactory = new DefinerFactory(ALCFormulaPreparations, ordering, EmptyRoleHierarchy)
    //    subsumptionChecker.setDefinerFactory(definerFactory)

    clauseOrdering = new ConceptClauseOrdering(ordering)
    resultTBox = new TreeSet[ConceptClause]()(clauseOrdering)
    exisTBox = new TreeSet[ConceptClause]()(clauseOrdering)
    univTBox = new TreeSet[ConceptClause]()(clauseOrdering)

    // we have to keep the memory of former forgetting results for subsequent forgetting steps
    // this is needed for termination when forgetting with background
    //allTBox = new TreeSet[ConceptClause]()(clauseOrdering)
    //allABox = new HashMap[Individual, mutSet[ExtendedABoxClause]]
    //  with MultiMap[Individual, ExtendedABoxClause]


  }

  def instantiateRoleAssertions() =

    if(univABox.size>0 || univTBox.size>0){ // we can avoid looping possibly huge sets of role assertions if no inference
                                            // would be possible

      // avoid iterating over role assertions that we can't make inferences on
      val relevant =
        if(univABox.values.forall(_.forall(backgroundABox)) && univTBox.forall(backgroundTBox))
      roleAssertions1st.values.filter(_.exists(!backgroundABox(_)))
          else
        roleAssertions1st.values

      relevant.foreach(_.foreach { rrc: ExtendedABoxClause =>
        rrc.roleAssertions.foreach{ ra =>
          checkCanceled

          ra match {
            case RoleAssertion(r, a, b) if r.equals(forgetRole) => {
              logger.trace(s"checking for $ra")
              logger.trace(s" in ${univABox.get(a)}")
              univABox.get(a)
                .foreach(aboxAdmissibleFor(rrc, _).foreach(
                  roleAssertionPropagationRule
                    .applyAll(_, rrc)
                    .foreach(proceedABox)))

              logger.trace(s" and in $univTBox")
              tboxAdmissibleFor(rrc, univTBox)
                .flatMap(roleAssertionPropagationRule
                  .applyAll(_, rrc))
                .foreach(proceedABox)
            }
            case other => ; // ignore
          }
        }
      })
    }


  override def derive(): Unit = {

    logger.debug(s"")

    forgetRolePropagationRule =
      new SimpleRolePropagationForgetRule(forgetRole,
        EmptyRoleHierarchy,
        definerFactory)

    while(!exisABox.isEmpty || !univABox.isEmpty || !exisTBox.isEmpty || !negRoleAssertions.isEmpty) {

      checkCanceled

      instantiateRoleAssertions()

      while(!negRoleAssertions.isEmpty){
        val nextAssert = negRoleAssertions.keys.head
        val next = negRoleAssertions(nextAssert).head
        negRoleAssertions.removeBinding(nextAssert,next)

        logger.debug(s"\n inferences on ${nextAssert} in ${next}")

        var derivations =
          roleAssertions1st.getOrElse(nextAssert.individual1, Set[ExtendedABoxClause]())
            .filter(roleAssertions2nd.getOrElse(nextAssert.individual2, Set[ExtendedABoxClause]()))
            .filter(admissiblePremises(next, _))
            .map(RoleAssertionResolutionRule.apply(next, _, nextAssert))

        derivations.toSeq.sortBy(_.size).foreach {
          proceedABox
        }
      }


      while (!exisABox.isEmpty) {

        checkCanceled

        val nextInd = exisABox.keys.head
        val next = exisABox(nextInd).head
        exisABox.removeBinding(nextInd, next)

        logger.debug(s"\nInferences on existential in ${next} for ${nextInd}")

        var derivations = inferencesWithABoxExistential(
          next,
          aboxAdmissibleFor(next, univABox.getOrElse(nextInd, Set())),
          tboxAdmissibleFor(next, univTBox.toSet),
          nextInd)

        derivations.toSeq.sortBy(_.size).foreach {
          proceedABox
        }
      }

      if (!univABox.isEmpty) {

        checkCanceled

        val nextInd = univABox.keys.head
        val next = univABox(nextInd).head
        univABox.removeBinding(nextInd, next)

        logger.debug(s"\nInferences on universal in ${next} for ${nextInd}")

        var derivations = inferencesABoxUniversalWithTBox(next,
          tboxAdmissibleFor(next, exisTBox.toSet),
          nextInd)

        derivations.toSeq.sortBy(_.size).foreach {
          proceedABox
        }
      }


      while (!exisTBox.isEmpty) {

        checkCanceled

        val next = exisTBox.head
        exisTBox.remove(next)

        logger.debug(s"\nResolving on ${next}")
        /*

          TODO: find solution that uses inferenceLogger (current doesn't)
          */
        inferencesWithTBoxExistential(next,univTBox).foreach(proceedTBox)

        var derivations2 = getTBoxWithABoxInferences(next)
        derivations2.toSeq.sortBy(_.size).foreach {
          proceedABox
        }
      }
    }
  }


  def inferencesWithABoxExistential(clause: ExtendedABoxClause,
                                    aboxUniversals: Set[ExtendedABoxClause],
                                    tboxUniversals: Set[ConceptClause],
                                    individual: Individual): Iterable[ExtendedABoxClause] = {

    checkCanceled

    val conceptClause = clause.literals(individual)

    var result = List[ExtendedABoxClause]()

    // first: inferences with universal restrictions in the TBox
    inferencesWithTBoxExistential(conceptClause, tboxAdmissibleFor(clause, tboxUniversals))
      .foreach{
        conclusion =>
          result = clause.replace(individual, conclusion)::result
      }

    // next: inferences with universal restrictions in the ABox
    aboxAdmissibleFor(clause, aboxUniversals).foreach{ aboxUniversal =>
      val conceptClause2 = aboxUniversal.literals(individual)
      inferencesWithTBoxExistential(conceptClause,
        Some(conceptClause2)).foreach{
        conceptConclusion =>
          result = clause.combineWith(aboxUniversal).replace(individual,
            conceptConclusion)::result
      }
    }

    result
  }

  def inferencesABoxUniversalWithTBox(clause: ExtendedABoxClause,
                                      tboxExistentials: Iterable[ConceptClause],
                                      individual: Individual) = {
    getTBoxRolePropagationDerivationsEx(clause, tboxExistentials.toSet[ConceptClause], individual)
  }

  /**
   * pure TBox inferences, using role resolution or forget role propagation.
   */
  def inferencesWithTBoxExistential(clause: ConceptClause,
                                    tboxUniversals: Iterable[ConceptClause]): Iterable[ConceptClause] = {

    logger.debug(s"inferences with TBox ex: ${clause}")

    checkCanceled

    var result = Set[ConceptClause]()

    assert(clause.literals.head.concept.isInstanceOf[ExistentialRoleRestriction])
    tboxAdmissibleFor(clause, tboxUniversals).foreach { clause2 =>
      getForgetUniversalLiterals(clause2).foreach { universal =>

        val newInferences = forgetRolePropagationRule.derive(
          clause,
          clause.literals.head,
          clause2,
          universal)

        println("New inferences:")
        newInferences.foreach(c => println("    -  "+c))

        newInferences.filterNot(result).foreach(cl => result ++= inferencesWithTBoxExistential(cl, tboxUniversals))

        result ++= newInferences

        // TODO can be optimised: subsets processed more than once with this recursive method
      }
    }

    result

  }

  /**
   * return literals in clause with a universal role on the current concept to be
   * forgotten.
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

  def getTBoxWithABoxInferences(clause: ConceptClause)
  : Iterable[ExtendedABoxClause] =
    univABox.keySet.toSet[Individual].flatMap(ind =>
      getTBoxWithABoxInferences(clause,
        aboxAdmissibleFor(clause, univABox(ind)).toSet[ExtendedABoxClause],
        tboxAdmissibleFor(clause, univTBox),
        ind))

  def getTBoxWithABoxInferences(clause: ConceptClause,
                                aboxUniversals: Set[ExtendedABoxClause],
                                tboxUniversals: Set[ConceptClause],
                                individual: Individual)
  : Iterable[ExtendedABoxClause] = {
    checkCanceled

    // in this case tboxUniversals not relevant, as we do the role propagation step-wise

    var result = List[ExtendedABoxClause]()

    aboxAdmissibleFor(clause, aboxUniversals).foreach{
      aboxClause =>
        result ++= inferencesABoxUniversalWithTBox(aboxClause,
          Some(clause),individual)
    }
    result
  }


  override def _addABoxClause(_clause: ExtendedABoxClause)
  : ExtendedABoxClause = {

    val clause = super._addABoxClause(_clause)
    var result = true
    clause.literals.keys.foreach{ ind =>
      clause.literals(ind).literals.head.concept match {
        case UniversalRoleRestriction(r: BaseRole, _) if r==forgetRole
        => univABox.addBinding(ind, clause); result=false
        case ExistentialRoleRestriction(r: BaseRole, _) if r==forgetRole
        => exisABox.addBinding(ind, clause); result=false
        case UniversalRoleRestriction(InverseRole(r: BaseRole), _) if r==forgetRole
        => univABox.addBinding(ind, clause); result=false
        case ExistentialRoleRestriction(InverseRole(r: BaseRole), _) if r==forgetRole
        => exisABox.addBinding(ind, clause); result=false
        case _ => ;
      }
    }
    clause.negatedRoleAssertions.foreach{nra =>
      if(nra.role.equals(forgetRole)) {
        negRoleAssertions.addBinding(nra, clause)
        result = false
      }
    }
    if(clause.roleAssertions.exists(_.role.equals(forgetRole)))
      result = false

    if(result && !backgroundABox.contains(clause)){
      logger.trace(s"new result clause!")
      resultABox.add(clause)
    } else {
      addMonotonicityResults(clause)
    }


    clause
  }


  def addMonotonicityResults(clause: ExtendedABoxClause): Unit = {
    // here the admissibility check is done explicitly.
    // we assume here that role axioms only occur in the background KB.
    if (!backgroundABox(clause)) {
      val add = applyMonotonicity(clause, forgetRole)

      logger.debug(s"monotonicity results: ${add}")

      add.foreach(addABoxClause)
    }
  }

  def applyMonotonicity(clause: ExtendedABoxClause, role: BaseRole)
  : Iterable[ExtendedABoxClause] = {
    logger.info(s"Applying monotonicity for ${role} for ${clause}")

    // if the role to be forgotten occurs in a role assertion, we
    // have to discard this clause
    // (recall we have no role hierarchy/only the TOP role as
    // super role
    if(clause.roleAssertions.exists(_.role.equals(forgetRole)))
      return Set()
    if(clause.negatedRoleAssertions.exists(_.role.equals(forgetRole)))
      return Set()

    // otherwise we apply monotonicity on the role assertions
    // (this loop is copied from the abox forgetter, which uses
    // role hierarchies. with our empty role hierarchy, still
    // replaces existential role restrictions, or returns empty
    // set if role occurs under universal role restriction
    def inner(restClause: List[(Individual, ConceptClause)])
    : Iterable[ExtendedABoxClause] = restClause match {
      case (ind, clause)::rest
      => applyMonotonicity(Set(clause), role).flatMap{ clauseX =>
        val head = new ExtendedABoxClause(ind, clauseX)
        inner(rest).map(cl => ExtendedABoxClause.combine(Set(head, cl)) )
      }
      case Nil => Set(
        new ExtendedABoxClause(Map[Individual, ConceptClause](),
          clause.roleAssertions,
          clause.negatedRoleAssertions))
    }

    inner(clause.literals.toList)
  }




  override def _addConceptClause(_clause: ConceptClause): ConceptClause = {
    val clause = super._addConceptClause(_clause)

    var result = true

    clause.literals.head.concept match {
      case UniversalRoleRestriction(r: BaseRole, _) if r==forgetRole =>
        univTBox.add(clause); result = false
      case ExistentialRoleRestriction(r: BaseRole, _) if r==forgetRole =>
        exisTBox.add(clause); result = false
      case UniversalRoleRestriction(InverseRole(r: BaseRole), _) if r==forgetRole =>
        univTBox.add(clause); result = false
      case ExistentialRoleRestriction(InverseRole(r: BaseRole), _) if r==forgetRole =>
        exisTBox.add(clause); result = false
      case _ if validResultClause(clause) && !backgroundTBox.contains(clause) =>
        logger.debug(s"new result clause!"); resultTBox.add(clause)
      case _ => ;
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
    // here the admissibility check is done explicitly.
    // we assume here that role axioms only occur in the background KB.
    if (!backgroundTBox(clause)) {
      val add = applyMonotonicity(Set(clause), forgetRole)
      logger.debug(s"monotonicity results: ${add}")
      add.foreach(addConceptClause)
    }
  }

  def applyMonotonicity(clauses: Iterable[ConceptClause], role: BaseRole): Iterable[ConceptClause] = {

    checkCanceled

    logger.info(s"Applying monotonicity for ${role} for ${clauses}")

    // REMARK: mostly kept the code from the abox forgetter, despite it could be
    // much simpler in the absence of a role hierarchy, as we might want to extend
    // the implementation to role hierarchies one day
    def inner(literals: List[ConceptLiteral], currentResult: List[ConceptLiteral]): Set[ConceptClause] = literals match {
      case ConceptLiteral(true, ExistentialRoleRestriction(r, c)) :: rest
        if r.signature.contains(role.name) =>
        (EmptyRoleHierarchy.getDirectSuperRoles(r) - DLHelpers.inverse(r) - r).flatMap(r2 =>
          inner(rest, ConceptLiteral(true, ExistentialRoleRestriction(r2, c)) :: currentResult)
        )
      case ConceptLiteral(true, UniversalRoleRestriction(r, c)) :: rest if r.signature.contains(role.name) =>
        (EmptyRoleHierarchy.getDirectSubRoles(r) - DLHelpers.inverse(r) - r).flatMap(r2 =>
          inner(rest, ConceptLiteral(true, UniversalRoleRestriction(r2, c)) :: currentResult))
      case c :: rest =>
        inner(rest, c :: currentResult)
      case Nil =>
        Set(new ConceptClause(currentResult, ordering))
    }

    clauses.flatMap(cl => inner(cl.literals.toList, List()))
  }


  override def remove(clause: ConceptClause) = {
    super.remove(clause)
    univTBox.remove(clause)
    exisTBox.remove(clause)
  }

  override def remove(clause: ExtendedABoxClause) = {
    super.remove(clause)
    clause.literals.keySet.foreach{ individual =>
      univABox.removeBinding(individual, clause)
      exisABox.removeBinding(individual, clause)
    }
    clause.negatedRoleAssertions.foreach{ nra =>
      negRoleAssertions.removeBinding(nra, clause)
    }
  }


  override def redundant(clause: ConceptClause): Boolean =
  // the normal redundancy test against all clauses is not sufficient anymore here,
  // since role monotonicity results become directly redundant and are thus not added
  // to the result.
    allTBox.contains(clause) ||
      resultTBox.contains(clause) ||
      resultTBox.exists(subsumptionChecker.subsumes(_, clause))
}

