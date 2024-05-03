package uk.ac.man.cs.lethe.internal.dl.abduction.forgetting

import uk.ac.man.cs.lethe.internal.dl.datatypes.extended.ExtendedABoxClause
import uk.ac.man.cs.lethe.internal.dl.datatypes.{BaseConcept, Concept, Individual, Role, RoleAssertion, UniversalRoleRestriction}
import uk.ac.man.cs.lethe.internal.dl.forgetting.direct.ConceptLiteral

/**
 * C1 v ¬D(a)      C2 v Ar.D(b)
 * ----------------------------
 *      C1 v C2 v ¬r(b,a)
 */

object NegRoleAssertionIntroductionRule {
  def apply(clause1: ExtendedABoxClause,
            definer: BaseConcept,
            ind: Individual,
            clause2: ExtendedABoxClause, r: Role, ind2: Individual) = {

    val combinedClause = clause1.combineWith(clause2)

    new ExtendedABoxClause(
      (combinedClause.literals-ind-ind2)+
        (ind -> combinedClause.literals(ind)
          .without(ConceptLiteral(false, definer))) +
        (ind2 -> combinedClause.literals(ind2)
          .without(ConceptLiteral(true, UniversalRoleRestriction(r,definer)))
          ),
      combinedClause.roleAssertions,
      combinedClause.negatedRoleAssertions + RoleAssertion(r,ind2,ind)
    )
  }


  /**
   * Apply the rule on ¬definer(individual) from clause1
   */
  def apply(clause1: ExtendedABoxClause, clause2: ExtendedABoxClause,
            definer: BaseConcept, individual: Individual): Set[ExtendedABoxClause] =
    clause2.literals.keySet.flatMap { ind2 =>
      clause2.literals(ind2).literals.collect {
        case ConceptLiteral(true, UniversalRoleRestriction(r, d: BaseConcept))
          if d.equals(definer) =>
            apply(clause1, definer, individual, clause2, r, ind2)
    }
  }
}
