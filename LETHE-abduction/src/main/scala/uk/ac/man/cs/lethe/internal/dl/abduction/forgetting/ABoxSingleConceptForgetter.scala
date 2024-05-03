package uk.ac.man.cs.lethe.internal.dl.abduction.forgetting

import uk.ac.man.cs.lethe.internal.dl.datatypes.extended.ExtendedABoxClause
import uk.ac.man.cs.lethe.internal.dl.forgetting.direct._
import uk.ac.man.cs.lethe.internal.tools.Timeoutable

import scala.collection.mutable.{HashMap, HashSet, MultiMap, TreeSet, Set => mutSet}

//import com.dongxiguo.fastring.Fastring.Implicits._
import com.typesafe.scalalogging.Logger

import uk.ac.man.cs.lethe.internal.dl.datatypes._
import uk.ac.man.cs.lethe.internal.forgetting.Forgetter


class ExtendedABoxSingleConceptForgetter()
  extends AbstractExtendedABoxClauseForgetter[BaseConcept]()
    with Timeoutable {


  //override implicit val (logger, formatter, appender) = ZeroLoggerFactory.newLogger(ABoxSingleConceptForgetter)
  //import formatter._
  override val logger = Logger[ExtendedABoxSingleConceptForgetter]

  var forgetConcept: BaseConcept = _

  var indPos: MultiMap[Individual, ExtendedABoxClause] = _
  var indNeg: MultiMap[Individual, ExtendedABoxClause] = _

  var posClauses: TreeSet[ConceptClause] = _
  var negClauses: TreeSet[ConceptClause] = _

  override def init() = {
    super.init()

    indPos = new HashMap[Individual, mutSet[ExtendedABoxClause]]()
      with MultiMap[Individual, ExtendedABoxClause]
    indNeg = new HashMap[Individual, mutSet[ExtendedABoxClause]]()
      with MultiMap[Individual, ExtendedABoxClause]
  }

  var turns = 0

  def derive(): Unit = {

    assert(turns<5)

    assert(indPos!=null)

    logger.trace("Beginning inferences")
    logger.trace(s"Defining clauses are ${definingClauses}")

    logger.trace(s"indPos:")
    logger.trace(s"${indPos.mkString("\n")}")
    while(!indPos.isEmpty){

      checkCanceled

      val nextInd = indPos.keys.head
      val next = indPos(nextInd).head
      indPos.removeBinding(nextInd, next)

      logger.debug(s"\nResolving on ${next} for ${nextInd}")

      var derivations = getTBoxResolutionDerivations(next,
        negClauses.toSet, nextInd)
      derivations ++= getABoxResolutionDerivations(next, indNeg.getOrElse(nextInd, Set()).toSet, nextInd)
      derivations.toSeq.sortBy(_.size).foreach{ proceedABox }
    }

    logger.trace(s"indNeg:")
    logger.trace(s"${indNeg.mkString("\n")}")
    while(!indNeg.isEmpty){

      checkCanceled

      val nextInd = indNeg.keys.head
      val next = indNeg(nextInd).head
      indNeg.removeBinding(nextInd, next)

      logger.debug(s"\nResolving on ${next} for ${nextInd}")

      var derivations = getTBoxResolutionDerivations(next, posClauses.toSet, nextInd)
      derivations.toSeq.sortBy(_.size).foreach{ proceedABox }
    }

    logger.trace(s"negClauses:")
    logger.trace(s"${negClauses.mkString("\n")}")
    while(!negClauses.isEmpty) {

      checkCanceled

      val next = negClauses.head
      negClauses.remove(next)

      logger.debug(s"\nResolving on ${next}")
      var derivations = resolutionRule.getDerivations(next,
        tboxAdmissibleFor(next, posClauses.toSet))

      inferenceLogger.sendInference(derivations)
      derivations.flatMap(_.conclusions).toSeq.sortBy(_.literals.size).foreach{ proceedTBox }
    }

    if(!indPos.isEmpty || !indNeg.isEmpty )
      turns+= 1

    if(!indPos.isEmpty || !indNeg.isEmpty)
      derive()
  }


  def setForgetConcept(concept: BaseConcept) = {
    logger.info(s"Forgetting ${concept}")
    this.forgetConcept = concept
    ordering = new ConceptLiteralOrdering(Seq(forgetConcept.name))


  //  definerFactory = new DefinerFactory(ALCFormulaPreparations, ordering, EmptyRoleHierarchy)
  //  subsumptionChecker.setDefinerFactory(definerFactory)



    clauseOrdering = new ConceptClauseOrdering(ordering)
    resultTBox = new TreeSet[ConceptClause]()(clauseOrdering)
    posClauses = new TreeSet[ConceptClause]()(clauseOrdering)
    negClauses = new TreeSet[ConceptClause]()(clauseOrdering)

    // we have to keep the memory for the redundancy checks in subsequent forgetting steps
    // (needed when forgetting with background)
    //allTBox = new TreeSet[ConceptClause]()(clauseOrdering)
    //allABox = new HashMap[Individual, mutSet[ExtendedABoxClause]]
    //  with MultiMap[Individual, ExtendedABoxClause]
  }

  override def _addABoxClause(_clause: ExtendedABoxClause): ExtendedABoxClause = {
    val clause = super._addABoxClause(_clause)
    var result = true
    clause.literals.keys.foreach{ ind =>
      clause.literals(ind).literals.head match {
        case ConceptLiteral(true, b: BaseConcept) if b==forgetConcept => {
          logger.debug(s"+${ind}, ${clause}")
          indPos.addBinding(ind, clause)
          result = false
        }
        case ConceptLiteral(false, b: BaseConcept) if b==forgetConcept => {
          logger.debug(s"-${ind}, ${clause}")
          indNeg.addBinding(ind, clause);
          result=false
        }
        case _ => ;
      }
    }

    if(result && !backgroundABox.contains(clause)){
      logger.trace(s"new result clause!")
      resultABox.add(clause)
    }  else if(result) {
      logger.trace(s"would have been a new result but was redundant ${allABoxSet}")
    }

    clause
  }

  override def _addConceptClause(_clause: ConceptClause): ConceptClause = {
    val clause = super._addConceptClause(_clause)

    // add new clause
    clause.literals.head match {
      case ConceptLiteral(true, b: BaseConcept) if b==forgetConcept =>
        posClauses.add(clause)
        logger.trace(s"positive clauses are now: ${posClauses}")
      case ConceptLiteral(false, b: BaseConcept) if b==forgetConcept => negClauses.add(clause)
        logger.trace(s"negative clauses are now: ${negClauses}")
      case _ if validResultClause(clause) && !backgroundTBox.contains(clause)
        => logger.debug(s"new result clause: ${clause}"); resultTBox.add(clause)
      case _ => ;
    }

    // we don't have the following side-condition anymore, since clauses outside of the signature can be
    // pulled from the background to be processed in subsequent steps.
    //assert(!resultTBox.exists(_.atomicConcepts(forgetConcept.name)), clause+", "+clause.literals.head)

    clause
  }

  override def remove(clause: ConceptClause) = {
    super.remove(clause)
    negClauses.remove(clause)
    posClauses.remove(clause)
  }

  override def remove(clause: ExtendedABoxClause) = {
    super.remove(clause)
    clause.literals.keySet.foreach{ individual =>
      indPos.removeBinding(individual, clause)
      indNeg.removeBinding(individual, clause)
    }
  }
}
