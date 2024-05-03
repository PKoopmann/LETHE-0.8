package uk.ac.man.cs.lethe.internal.dl.forgetting.abox

import java.util.Date
import uk.ac.man.cs.lethe.internal.dl.forgetting.direct._
import com.typesafe.scalalogging.Logger
import uk.ac.man.cs.lethe.internal.dl.{AbstractMappedReasonerFactory}

import scala.collection.mutable.{HashMap, HashSet, MultiMap, TreeSet, Set => mutSet}

//import com.dongxiguo.fastring.Fastring.Implicits._


import uk.ac.man.cs.lethe.internal.dl.datatypes._
import uk.ac.man.cs.lethe.internal.forgetting.Forgetter


object ABoxApproximator

class ABoxApproximator(roleHierarchy: RoleHierarchy,
                       _ordering: ConceptLiteralOrdering,
                       reasonerFactory: AbstractMappedReasonerFactory)
  extends AbstractABoxClauseForgetter[BaseConcept](roleHierarchy) {


  //implicit val (logger2, formatter2, appender2) = ZeroLoggerFactory.newLogger(ABoxApproximator)
  //import formatter2._
  val logger2 = Logger[ABoxApproximator]

  var toProcess: TreeSet[ABoxClause] = _

  ordering = _ordering
  clauseOrdering = new ConceptClauseOrdering(ordering)

  override def init() = {
    super.init()

    val mappedReasoner = reasonerFactory.newReasoner(allTBox)
    roleResolutionRule = new RoleResolutionRule(ordering,
      null,
      definerFactory,
      mappedReasoner)
  }

  def derive(): Unit = {

    resultTBox = TreeSet()(clauseOrdering)++allTBox

    // if(toProcess.size>0)
    //   println("Disjunctions to eliminate! "+new Date())

    while(!toProcess.isEmpty){
      val next = toProcess.head
      toProcess.remove(next)

      logger2.trace(s"Processing $next")

      next.literals.keySet.foreach{ ind =>
        val lit = next.literals(ind).literals.head
        lit.concept match{
          case _: BaseConcept =>
            logger2.trace("Base!")
            getTBoxResolutionDerivations(next, allTBox.toSet, ind).foreach{ proceedABox }
            getABoxResolutionDerivations(next, allABox.getOrElse(ind, Set()).toSet, ind).foreach{ proceedABox }
          case _: ExistentialRoleRestriction =>
            logger2.trace("Exis!")
            getABoxRoleResolutionDerivations(next,
              allABox.getOrElse(ind, Set()).toSet,
              allTBox.toSet,
              ind).foreach{ proceedABox }
          case _: UniversalRoleRestriction => {
            // Role propagation with exis-restrictions will trigger further application of role-prop
            logger2.trace("Univ!")
            individualRoles.getOrElse(ind, Set()).flatMap(r =>
              roleAssertionPropagationRule.apply(next, r, lit)).foreach{ proceedABox }
            getTBoxRolePropagationDerivationsEx(next, allTBox.toSet, ind).foreach{ proceedABox }
            //	    getABoxRolePropagationDerivationsWithEx(next, allABox.getOrElse(ind, Set()).toSet, ind).foreach{
            //	      proceedABox }
          }
        }
      }
    }
  }

  override def proceedABox(clause: ABoxClause) = {
    super.proceedABox(clause)
  }


  override def setRoleAssertions(roleAssertions: Set[RoleAssertion]) = {
    super.setRoleAssertions(roleAssertions)

    resultTBox = new TreeSet[ConceptClause]()(clauseOrdering)

    allTBox = new TreeSet[ConceptClause]()(clauseOrdering)
    allABox = new HashMap[Individual, mutSet[ABoxClause]] with MultiMap[Individual, ABoxClause]

    toProcess = new TreeSet[ABoxClause]()(new ABoxClauseOrdering(clauseOrdering))
    _connected = new HashMap[(Individual, Individual), Boolean]()
  }

  override def _addABoxClause(_clause: ABoxClause): ABoxClause = {
    val clause = super._addABoxClause(_clause)

    if(clause.literals.size>1 && unconnectedIndividuals(clause).size>0)
      toProcess.add(clause)
    else
      resultABox.add(clause)

    clause
  }


  def unconnectedIndividuals(clause: ABoxClause): Set[Individual] = {
    clause.literals.keySet.filter(ind1 =>
      !clause.literals.keySet.exists(ind2 => ind2!=ind1 && connected(ind1,ind2)))
  }

  var _connected: HashMap[(Individual, Individual), Boolean] = _

  def connected(ind1: Individual, ind2: Individual): Boolean = {
    _connected.get((ind1, ind2)) match{
      case Some(value) => value
      case None => {
        val processed = HashSet[Individual]()

        def inner(toVisit: List[Individual]): Boolean = {
          toVisit match {
            case Nil => false
            case ind3::rest => {
              if(processed(ind3))
                false
              else if(ind3==ind2)
                true
              else {
                _connected.put((ind1, ind3), true)
                _connected.get((ind3, ind2)) match {
                  case Some(result) => result
                  case None => {
                    processed.add(ind3)
                    inner(individualRoles.getOrElse(ind3, Set()).map(_.individual2).toList ++ rest)
                  }
                }
              }
            }
          }
        }

        val result = inner(List(ind1))

        _connected.put((ind1,ind2), result)

        result
      }
    }
  }

  override def remove(clause: ABoxClause): Unit = {
    super.remove(clause)
    toProcess.remove(clause)
  }

  override def redundant(clause: ABoxClause): Boolean =
    clause.literals.values.exists(redundant) ||
      clause.literals.keySet.exists(ind => allABox.get(ind).exists(_.exists(subsumes(_,clause))))

}
