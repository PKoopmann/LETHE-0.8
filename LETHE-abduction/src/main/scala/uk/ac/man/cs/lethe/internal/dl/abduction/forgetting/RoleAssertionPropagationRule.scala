package uk.ac.man.cs.lethe.internal.dl.abduction.forgetting

import com.typesafe.scalalogging.Logger
import uk.ac.man.cs.lethe.internal.dl.datatypes.extended.ExtendedABoxClause
import uk.ac.man.cs.lethe.internal.dl.datatypes.{RoleAssertion, UniversalRoleRestriction}
import uk.ac.man.cs.lethe.internal.dl.forgetting.abox.ABoxClause
import uk.ac.man.cs.lethe.internal.dl.forgetting.direct.{ConceptClause, ConceptLiteral, RoleHierarchy}

object RoleAssertionPropagationRule

/**
 * C1 v r(t1,b)     C2 v Ar.D(t2)
 * ------------------------------ ,
 *   (C1 v C2 v D2(b))sigma
 *
 * where sigma is the mgu of t1 and t2.
 *
 */
class RoleAssertionPropagationRule()  {

  val logger = Logger[RoleAssertionPropagationRule]


  def applyAll(clause1: ConceptClause, clause2: ExtendedABoxClause): Set[ExtendedABoxClause] = {
    clause2.roleAssertions.flatMap{ra =>
      applyAll(clause1, ra, clause2.without(ra))
    }
  }

  def applyAll(clause: ConceptClause, roleAssertion: RoleAssertion,
               restClause: ExtendedABoxClause): Set[ExtendedABoxClause] = {
    logger.debug(s"checking ${clause} and ${roleAssertion}")
    val lits = clause.literals.filter(_.concept.isInstanceOf[UniversalRoleRestriction])

    var results = lits.flatMap(apply(clause, roleAssertion, _, restClause))
    results ++= results.flatMap(applyAll(_, roleAssertion, restClause))
    results
  }


  def apply(clause: ConceptClause,
            roleAssertion: RoleAssertion,
            literal: ConceptLiteral,
            restClause: ExtendedABoxClause): Option[ExtendedABoxClause] = {
    literal.concept match {
      case UniversalRoleRestriction(r, d) if roleAssertion.role.equals(r) => {
        val newLiteral = ConceptLiteral(true, d)

        val indMap =
          if (roleAssertion.individual1.equals(roleAssertion.individual2))
            Map(roleAssertion.individual1 -> clause.without(literal)._with(newLiteral))
          else
            Map(roleAssertion.individual1 -> clause.without(literal),
              roleAssertion.individual2 -> new ConceptClause(Set(newLiteral), clause.ordering))

        Some(new ExtendedABoxClause(indMap).combineWith(restClause))
      }
      case _ => None
    }
  }

  def applyAll(clause1: ExtendedABoxClause,
               clause2: ExtendedABoxClause): Set[ExtendedABoxClause] = {
    clause2.roleAssertions.flatMap{ra =>
      applyAll(clause1, ra, clause2.without(ra))
    }
  }

  def applyAll(clause: ExtendedABoxClause,
               roleAssertion: RoleAssertion,
               restClause: ExtendedABoxClause): Set[ExtendedABoxClause] = {
    logger.debug("checking "+clause+" and "+roleAssertion)
    clause.literals.get(roleAssertion.individual1).toSet.flatMap{ cla: ConceptClause =>
      applyAll(cla, roleAssertion, restClause).map{ cl =>
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
        new ExtendedABoxClause(clauses, clause.roleAssertions, clause.negatedRoleAssertions)
          .combineWith(restClause)
      }
    }
  }

}