package uk.ac.man.cs.lethe.internal.dl.forgetting.abox

import uk.ac.man.cs.lethe.internal.dl.AbstractMappedReasonerFactory
import uk.ac.man.cs.lethe.internal.dl.forgetting.direct._
import uk.ac.man.cs.lethe.internal.tools.Timeoutable

import scala.collection.mutable.{HashMap, HashSet, MultiMap, TreeSet, Set => mutSet}

//import com.dongxiguo.fastring.Fastring.Implicits._
import com.typesafe.scalalogging.Logger

import uk.ac.man.cs.lethe.internal.dl.datatypes._
import uk.ac.man.cs.lethe.internal.forgetting.Forgetter


class ABoxSingleConceptForgetter(roleHierarchy: RoleHierarchy,
                                 inverseRoles: Boolean = false)
  extends AbstractABoxClauseForgetter[BaseConcept](roleHierarchy, inverseRoles)
    with Timeoutable {

  //override implicit val (logger, formatter, appender) = ZeroLoggerFactory.newLogger(ABoxSingleConceptForgetter)
  //import formatter._
  override val logger = Logger[ABoxSingleConceptForgetter]

  var forgetConcept: BaseConcept = _

  var indPos: MultiMap[Individual, ABoxClause] = _
  var indNeg: MultiMap[Individual, ABoxClause] = _

  var posClauses: TreeSet[ConceptClause] = _
  var negClauses: TreeSet[ConceptClause] = _

  override def init() = {
    super.init()

    indPos = new HashMap[Individual, mutSet[ABoxClause]]() with MultiMap[Individual, ABoxClause]
    indNeg = new HashMap[Individual, mutSet[ABoxClause]]() with MultiMap[Individual, ABoxClause]
  }

  var turns = 0

  def derive(): Unit = {

    assert(turns<5)

   // assert(indPos!=null)

    while(!indPos.isEmpty){
      val nextInd = indPos.keys.head
      val next = indPos(nextInd).head
      indPos.removeBinding(nextInd, next)

      logger.debug(s"\nResolving on ${next} for ${nextInd}")

      var derivations = getTBoxResolutionDerivations(next, negClauses.toSet, nextInd)
      derivations ++= getABoxResolutionDerivations(next, indNeg.getOrElse(nextInd, Set()).toSet, nextInd)
      derivations.toSeq.sortBy(_.size).foreach{ proceedABox }
    }

    while(!indNeg.isEmpty){
      val nextInd = indNeg.keys.head
      val next = indNeg(nextInd).head
      indNeg.removeBinding(nextInd, next)

      logger.debug(s"\nResolving on ${next} for ${nextInd}")

      var derivations = getTBoxResolutionDerivations(next, posClauses.toSet, nextInd)
      derivations.toSeq.sortBy(_.size).foreach{ proceedABox }
    }

    while(!negClauses.isEmpty) {
      val next = negClauses.head
      negClauses.remove(next)

      logger.debug(s"\nResolving on ${next}")
      var derivations = resolutionRule.getDerivations(next,
        posClauses.toSet)
      inferenceLogger.sendInference(derivations)
      derivations.flatMap(_.conclusions).toSeq.sortBy(_.literals.size).foreach{ proceedTBox }
    }

    if(!indPos.isEmpty || !indNeg.isEmpty)
      turns+= 1

    if(!indPos.isEmpty || !indNeg.isEmpty)
      derive() // <--- bitte checken, ob ausreichend!
    //      (neue ABox-Klauseln werden durch role assertion propagation jetzt auch bei TBox
    //       Inferenzen eingefuehert)
  }


  def setForgetConcept(concept: BaseConcept) = {
    logger.info(s"Forgetting ${concept}")
    this.forgetConcept = concept
    ordering = new ConceptLiteralOrdering(Seq(forgetConcept.name))


    definerFactory = new DefinerFactory(ALCFormulaPreparations, ordering, roleHierarchy)
    subsumptionChecker.setDefinerFactory(definerFactory)
    inferenceLogger.notifyDefinerFactory(definerFactory)
    definerFactory.addListener(this)



    clauseOrdering = new ConceptClauseOrdering(ordering)
    resultTBox = new TreeSet[ConceptClause]()(clauseOrdering)
    posClauses = new TreeSet[ConceptClause]()(clauseOrdering)
    negClauses = new TreeSet[ConceptClause]()(clauseOrdering)

    allTBox = new TreeSet[ConceptClause]()(clauseOrdering)
    allABox = new HashMap[Individual, mutSet[ABoxClause]] with MultiMap[Individual, ABoxClause]
  }

  override def _addABoxClause(_clause: ABoxClause): ABoxClause = {
    val clause = super._addABoxClause(_clause)
    var result = true
    clause.literals.keys.foreach{ ind =>
      clause.literals(ind).literals.head match {
        case ConceptLiteral(true, b: BaseConcept) if b==forgetConcept => logger.debug("+"+ind); indPos.addBinding(ind, clause); result=false
        case ConceptLiteral(false, b: BaseConcept) if b==forgetConcept => logger.debug("-"+ind);  indNeg.addBinding(ind, clause); result=false
        case _ => ;
      }
    }

    if(result){
      logger.trace(s"new result clause!")
      resultABox.add(clause)
    }

    clause
  }

  override def _addConceptClause(_clause: ConceptClause): ConceptClause = {
    val clause = super._addConceptClause(_clause)

    // add new clause
    clause.literals.head match {
      case ConceptLiteral(true, b: BaseConcept) if b==forgetConcept => posClauses.add(clause)
      case ConceptLiteral(false, b: BaseConcept) if b==forgetConcept => negClauses.add(clause)
      case _ if validResultClause(clause) => logger.debug(s"new result clause!"); resultTBox.add(clause)
      case _ => ;
    }
    assert(!resultTBox.exists(_.atomicConcepts(forgetConcept.name)), clause+", "+clause.literals.head)

    clause
  }

  override def remove(clause: ConceptClause) = {
    super.remove(clause)
    negClauses.remove(clause)
    posClauses.remove(clause)
  }

  override def remove(clause: ABoxClause) = {
    super.remove(clause)
    clause.literals.keySet.foreach{ individual =>
      indPos.removeBinding(individual, clause)
      indNeg.removeBinding(individual, clause)
    }
  }
}
