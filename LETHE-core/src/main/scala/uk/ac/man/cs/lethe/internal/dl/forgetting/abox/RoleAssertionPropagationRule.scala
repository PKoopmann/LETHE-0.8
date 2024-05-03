package uk.ac.man.cs.lethe.internal.dl.forgetting.abox

import com.typesafe.scalalogging.Logger
import uk.ac.man.cs.lethe.internal.dl.datatypes.{RoleAssertion, UniversalRoleRestriction}
import uk.ac.man.cs.lethe.internal.dl.forgetting.abox.ABoxClause
import uk.ac.man.cs.lethe.internal.dl.forgetting.direct.{ConceptClause, ConceptLiteral, RoleHierarchy}

object RoleAssertionPropagationRule

class RoleAssertionPropagationRule(roleHierarchy: RoleHierarchy) {

  //implicit val (logger, formatter, appender) = ZeroLoggerFactory.newLogger(RoleAssertionPropagationRule)
  //import formatter._

  val logger = Logger[RoleAssertionPropagationRule]

  def applyAll(clause: ConceptClause, roleAssertion: RoleAssertion): Set[ABoxClause] = {
    logger.debug(s"checking ${clause} and ${roleAssertion}")
    val lits = clause.literals.filter(_.concept.isInstanceOf[UniversalRoleRestriction])

    var results = lits.flatMap(apply(clause, roleAssertion, _))
    results ++= results.flatMap(applyAll(_, roleAssertion))
    results
    // var results = Set[ABoxClause]()

    // def inner(_clause: ConceptClause, lits: Seq[ConceptLiteral]): Unit = lits.foreach{ lit =>
    //   var newC = apply(clause, roleAssertion, lit)
    //   results ++= newC
    //   inner(clause, lits-lit))
    // }

    // inner(clause, lits)
    // results
  }

  def applyAll(clause: ABoxClause, roleAssertion: RoleAssertion): Set[ABoxClause] = {
    logger.debug("checking "+clause+" and "+roleAssertion)
    clause.literals.get(roleAssertion.individual1).toSet.flatMap{ cla: ConceptClause =>
      applyAll(cla, roleAssertion).map{ cl =>
        logger.debug("clause: "+clause)

        var clauses = (clause.literals - roleAssertion.individual1 - roleAssertion.individual2)
        if(cl.literals.keySet(roleAssertion.individual1))
          clauses += (roleAssertion.individual1 -> cl.literals(roleAssertion.individual1))
        if(clause.literals.keySet(roleAssertion.individual2) && roleAssertion.individual2!=roleAssertion.individual1)
          clauses += (roleAssertion.individual2 ->
            clause.literals(roleAssertion.individual2)._with(cl.literals(roleAssertion.individual2)))
        else
          clauses += (roleAssertion.individual2 ->
            cl.literals(roleAssertion.individual2))

        assert(!clauses.isEmpty)
        new ABoxClause(clauses)
      }
    }
  }

  def apply(clause: ConceptClause, roleAssertion: RoleAssertion, literal: ConceptLiteral): Option[ABoxClause] = {
    literal.concept match {
      case UniversalRoleRestriction(r, d) if roleHierarchy.subsumedBy(roleAssertion.role, r) => {
        val newLiteral = ConceptLiteral(true, d)
        if(roleAssertion.individual1 == roleAssertion.individual2)
          Some(new ABoxClause(Map(roleAssertion.individual1 -> clause.without(literal)._with(newLiteral))))
        else
          Some(new ABoxClause(Map(roleAssertion.individual1 -> clause.without(literal),
            roleAssertion.individual2 -> new ConceptClause(Set(newLiteral),clause.ordering))))

      }
      case _ => None
    }
  }

  // clause.literals.find(_.concept match {
  //   case UniversalRoleRestriction(r, c) if roleHierarchy.subsumedBy(roleAssertion.role, r) => true
  //   case _ => false
  // }) map { l => l match {
  //   case ConceptLiteral(_, UniversalRoleRestriction(_, d)) =>
  // 	new ABoxClause(Map(roleAssertion.individual1 -> clause.without(l),
  // 			   roleAssertion.individual2 ->
  // 			   new ConceptClause(Set(ConceptLiteral(true, d)), clause.ordering)))
  // }}

  def apply(clause: ABoxClause, roleAssertion: RoleAssertion, literal: ConceptLiteral): Option[ABoxClause] =
    clause.literals.get(roleAssertion.individual1).flatMap{
      apply(_, roleAssertion, literal).map { aboxClause =>
        var clauses = clause.literals - roleAssertion.individual1
        if(aboxClause.literals.contains(roleAssertion.individual1)) // quick fix -- valid?
          clauses += roleAssertion.individual1 -> aboxClause.literals(roleAssertion.individual1)
        val ind2 = roleAssertion.individual2
        if(ind2!=roleAssertion.individual1){
          if(clause.literals.contains(ind2))
            clauses += ind2 -> aboxClause.literals(ind2)._with(clause.literals(ind2))
          else
            clauses += ind2 -> aboxClause.literals(ind2)
        }
        new ABoxClause(clauses)
      }
    }



}