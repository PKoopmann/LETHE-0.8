package uk.ac.man.cs.lethe.internal.dl.abduction.forgetting

import uk.ac.man.cs.lethe.internal.dl.datatypes
import uk.ac.man.cs.lethe.internal.dl.datatypes.extended.ExtendedABoxClause
import uk.ac.man.cs.lethe.internal.dl.datatypes.{BaseRole, ExistentialRoleRestriction, TopRole}
import uk.ac.man.cs.lethe.internal.dl.forgetting.direct.{ConceptClause, ConceptLiteral}

class RoleMonotonicityRule(forgetRole: BaseRole) {
  def apply(clause: ExtendedABoxClause) = {
    new ExtendedABoxClause(clause.literals.mapValues(cc => new ConceptClause(
      cc.literals.map(_ match {
        case ConceptLiteral(true, ExistentialRoleRestriction(r, d)) if r.equals(forgetRole) =>
          ConceptLiteral(true, ExistentialRoleRestriction(TopRole, d))
        case l: ConceptLiteral => l
    }), cc.ordering)),
      clause.roleAssertions, clause.negatedRoleAssertions)
  }
}
