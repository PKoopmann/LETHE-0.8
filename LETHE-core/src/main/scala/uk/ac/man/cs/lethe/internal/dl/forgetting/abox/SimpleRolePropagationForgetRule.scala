package uk.ac.man.cs.lethe.internal.dl.forgetting.abox

import uk.ac.man.cs.lethe.internal.dl.datatypes.{BaseConcept, BaseRole, ExistentialRoleRestriction, UniversalRoleRestriction}
import uk.ac.man.cs.lethe.internal.dl.forgetting.direct.{ConceptLiteral, DefinerFactory, LiteralBasedRule, RoleHierarchy}

class SimpleRolePropagationForgetRule(forgetRole: BaseRole,
                                      roleHierarchy: RoleHierarchy,
                                      definerFactory: DefinerFactory)
  extends LiteralBasedRule {

  def combineLiterals(l1: ConceptLiteral, l2: ConceptLiteral)
  : Set[Set[ConceptLiteral]] = (l1.concept, l2.concept) match {
    case (ExistentialRoleRestriction(r1: BaseRole, d1: BaseConcept),
    UniversalRoleRestriction(r2: BaseRole, d2: BaseConcept))
      if r1.equals(forgetRole) && r2.equals(forgetRole)
    =>
      Set(Set(ConceptLiteral(true,
        ExistentialRoleRestriction(r1,
          definerFactory.combineDefiners(d1, d2)._1))))
    case _ => Set()
  }
}
