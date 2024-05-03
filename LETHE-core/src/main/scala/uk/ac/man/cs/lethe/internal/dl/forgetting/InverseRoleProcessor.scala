package uk.ac.man.cs.lethe.internal.dl.forgetting


import scala.collection.mutable.HashSet

//import com.dongxiguo.fastring.Fastring.Implicits._
import com.typesafe.scalalogging.Logger

import uk.ac.man.cs.lethe.internal.dl.forgetting.direct._
import uk.ac.man.cs.lethe.internal.dl.datatypes._


/**
 * Processes inverse roles with the role inversion rule, as described in the thesis.
 * Precondition: Clause set is fully saturated with respect to derivable universal role restrictions.
 * Postcondition: Clause set can be processed by calculus not supporting inverse roles as normal.
 * (Proved for ALCI, SIF and SIQ, calculus should be able to deal with clauses containing more than
 * one negative definer literal.)
 */
object InverseRoleProcessor {

}


class InverseRoleProcessor(definerFactory: DefinerFactory) {

  //  implicit val (logger, formatter, appender) = ZeroLoggerFactory.newLogger(InverseRoleProcessor)
  //  import formatter._

  val logger = Logger[InverseRoleProcessor]

  def process(inputClauses: Iterable[ConceptClause]): Iterable[ConceptClause] = {

    var newDerivations = Seq[ConceptClause]()

    inputClauses.foreach{ clause =>
      logger.trace(s"process ${clause}")
      clause.literals.foreach{ literal => literal.concept match {
        case UniversalRoleRestriction(role, d: BaseConcept) =>
          newDerivations ++= getConclusions(clause.without(literal), role, d)

        case _: UniversalRoleRestriction => assert(false, "unexpected literal: "+literal)

        case MaxNumberRestriction(0, role, ConceptComplement(d: BaseConcept)) =>
          newDerivations ++= getConclusions(clause.without(literal), role, d)

        case MaxNumberRestriction(0, _, _) => assert(false, "unexpected literal: "+literal)

        case _ => ;
      }}
    }

    newDerivations//.toSet[ConceptClause]
  }



  private def getConclusions(restClause: ConceptClause,
                             role: Role,
                             definer: BaseConcept)
  : Seq[ConceptClause] = {

    //    val newDefiner1 = definerFactory.newDefiner()
    val newDefiner2 = definerFactory.newDefiner()

    val ordering = restClause.ordering



    // val clause1 = new ConceptClause(Set(ConceptLiteral(false, newDefiner1),
    // 					ConceptLiteral(false, newDefiner2)),
    // 				    ordering) // -D1 or -D2
    // // <-- should come first, since defining clause

    val clause2 = restClause._with(ConceptLiteral(false, newDefiner2)) // D1 or C1
    // after resolving on d1: -D2 or C1

    // val clause3 = new ConceptClause(Set(ConceptLiteral(true, newDefiner2),
    // 					ConceptLiteral(true,
    // 						       UniversalRoleRestriction(role,
    // 										definer))),
    // 				    ordering) // D2 or Ar.D
    // Even needed in practice?


    val clause4 = new ConceptClause(Set(ConceptLiteral(true, definer),
      ConceptLiteral(true,
        UniversalRoleRestriction(invert(role),
          newDefiner2))),
      ordering) // D or Ar-.D2

    // logger.trace(s"derived: \n\t${clause1}\n\t${clause2}\n\t${clause3}\n\t${clause4}")
    // Seq(clause1, clause2, clause3, clause4)

    logger.trace(s"derived: \n\t${clause2}\n\t${clause4}")
    Seq(clause2, clause4)
  }


  private def invert(role: Role): Role = role match {
    case role: BaseRole => InverseRole(role)
    case InverseRole(role2: BaseRole) => role2
    case InverseRole(InverseRole(r)) => invert(r)
  }


}
