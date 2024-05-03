package uk.ac.man.cs.lethe.internal.dl.abduction.forgetting

import uk.ac.man.cs.lethe.internal.dl.datatypes.extended.ExtendedABoxClause
import uk.ac.man.cs.lethe.internal.dl.datatypes.{BaseRole, RoleAssertion}

object RoleAssertionResolutionRule {

  def applyAll(clause1: ExtendedABoxClause, clause2: ExtendedABoxClause, forgetRole: BaseRole) ={
    clause1.roleAssertions
      .filter(_.role.equals(forgetRole))
      .filter(clause2.negatedRoleAssertions)
      .map(apply(clause1, clause2, _))
  }

  def apply(clause1: ExtendedABoxClause, clause2: ExtendedABoxClause, literal: RoleAssertion) = {
    val result = clause1.combineWith(clause1)

    result.roleAssertions-=literal
    result.negatedRoleAssertions-=literal

    result
  }
}
