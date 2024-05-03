package uk.ac.man.cs.lethe.internal.dl.forgetting.direct

import uk.ac.man.cs.lethe.internal.dl.AbstractMappedReasoner

import java.io.File
import java.util.Date

import uk.ac.man.cs.lethe.internal.tools.Timeoutable

import scala.collection.mutable.{HashMap, MultiMap, Set => MutSet}
import scala.concurrent.TimeoutException

//import com.dongxiguo.fastring.Fastring.Implicits._
import com.typesafe.scalalogging.Logger

//import org.semanticweb.HermiT.Reasoner
//import org.semanticweb.owlapi.apibinding.OWLManager
//import org.semanticweb.owlapi.model._


import uk.ac.man.cs.lethe.internal.dl.datatypes._
//import uk.ac.man.cs.lethe.internal.dl.owlapi.OWLExporter
import uk.ac.man.cs.lethe.internal.tools.Iterables


class RoleResolutionRule(ordering: Ordering[ConceptLiteral],
                         baseRole: BaseRole,
                         definerFactory: DefinerFactory,
                         reasoner: AbstractMappedReasoner) extends Rule with Timeoutable {
  //  implicit val (logger, formatter, appender) = ZeroLoggerFactory.newLogger(RoleResolutionRule)
  //  import formatter._

  val logger = Logger[RoleResolutionRule]

  import Iterables._

  override def getDerivations(clause: ConceptClause, clauses: Iterable[ConceptClause]): Set[Derivation] = {
    (if(baseRole==null)
      Some(clause.literals.head.concept)
    else
      getExistentialRestriction(baseRole, clause)) match {
      case None => Set()
      case Some(ExistentialRoleRestriction(role, d1)) => {

        if(!reasoner.isSatisfiable(d1)){
          val newClause = new ConceptClause(clause.literals -
            ConceptLiteral(true,
              ExistentialRoleRestriction(role, d1)),
            ordering)
          logger.trace("new clause is "+newClause)
          return Set(Derivation(
            premisses = Set(clause)+
              new ConceptClause(Set(ConceptLiteral(false,d1)), ordering),
            conclusions = Set(newClause), rule=this
          ))
        }

        var processedSubsets = Set[Set[UniversalRoleRestriction]]()
        var result = Set[Derivation]()

        // candRests: Maps universal restrictions to the clauses in which they occur
        val candRests = new HashMap[UniversalRoleRestriction,
          MutSet[ConceptClause]]() with MultiMap[UniversalRoleRestriction,
          ConceptClause]

        clauses.foreach(cl => candRest(cl, role).foreach(candRests.addBinding(_,cl)))
        // <--POSSIBLE SOURCE OF OPTIMISATION?


        logger.trace(s"candidates: ${clauses.size}")
        logger.trace(s"literal candidates: ${candRests.size}")

        // iterate over subsets of universal restricted definers that are inconsistent with d1

        candRests.keySet.subsets.foreach{ restrictions =>

          checkCanceled

          logger.trace("Checking set "+restrictions)
          if(!processedSubsets.exists(p => p.forall(restrictions))){

            var definers = restrictions.toSet[UniversalRoleRestriction].map(_.filler)+d1
            definers = definers.flatMap(c =>
              definerFactory.getBaseDefiners(c.asInstanceOf[BaseConcept]))

            logger.trace("definers to be checked are "+definers)
            if(!reasoner.isSatisfiable(definers)){
              logger.trace("Got there")
              processedSubsets += restrictions.toSet



              // since there might be several clauses with the same universal role restriction (referred to by candRests)
              // we have to iterate over the different combinations.
              // attach/1 takes as argument a set of sets {{a1,a2},{b},{c1,c2}}, and returns all combinations of
              // taking one element each {{a1,b,c1},{a1,b,c2},{a2,b,c1},{a2,b,c2}}.
              // here we use it to get the clauses that have to be combined to form a conclusion.
              attach(restrictions.toSet[UniversalRoleRestriction].map(candRests(_).toSet)).foreach{ subset =>
                checkCanceled
                val newLiterals = subset.flatMap(_.literals)--restrictions.map(ConceptLiteral(true,_))
                val newClause = new ConceptClause(clause.literals++
                  newLiterals-
                  ConceptLiteral(true,
                    ExistentialRoleRestriction(role, d1)),
                  ordering)
                logger.trace("new clause is "+newClause)
                // logger.trace("new clause: "+newClause.toString)
                // val univ = newClause.literals.collectFirst{
                // 	case ConceptLiteral(true, UniversalRoleRestriction(r, c)) if r==role =>
                // 	  UniversalRoleRestriction(r,c)}
                // univ match {
                // 	case Some(u: UniversalRoleRestriction) => {
                // 	  logger.trace("new candidate!")
                // 	  candidates += newClause
                // 	  candRest = candRest.updated(newClause, u)
                // 	}
                // 	case None => ;
                // }
                // val lastPremise = new
                //   ConceptClause(definers.map(ConceptLiteral(false,_)),
                //     ordering)

                result += Derivation(premisses = subset+clause
                  +new ConceptClause(definers.map(ConceptLiteral(false,_)),ordering), //+lastPremise,
                  conclusions = Set(newClause), rule=this)
              }
            }
          }
        }
        result
      }
    }
  }


  def candRest(clause: ConceptClause, role: Role): Set[UniversalRoleRestriction] = {
    clause.literals.collect{
      case ConceptLiteral(true, u: UniversalRoleRestriction) if u.role==role => u
    }
  }

  def getExistentialRestriction(role: BaseRole, clause: ConceptClause) =
    clause.literals.collectFirst{
      case ConceptLiteral(true, ExistentialRoleRestriction(r, c)) if r==role => ExistentialRoleRestriction(r, c)
      case ConceptLiteral(true, ExistentialRoleRestriction(InverseRole(r), c)) if r==role =>
        ExistentialRoleRestriction(InverseRole(r), c)
    }
}
