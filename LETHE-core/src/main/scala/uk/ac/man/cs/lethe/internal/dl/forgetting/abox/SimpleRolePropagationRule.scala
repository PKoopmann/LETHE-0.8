package uk.ac.man.cs.lethe.internal.dl.forgetting.abox

import uk.ac.man.cs.lethe.internal.dl.datatypes.{BaseConcept, BaseRole, ExistentialRoleRestriction, Expression, RoleSubsumption, UniversalRoleRestriction}
import uk.ac.man.cs.lethe.internal.dl.forgetting.direct.{ConceptClause, ConceptLiteral, DefinerFactory, ExtendedDerivation, LiteralBasedRule, RoleHierarchy}


class SimpleRolePropagationRule(roleHierarchy: RoleHierarchy,
                                definerFactory: DefinerFactory)
  extends LiteralBasedRule {

  def combineLiterals(l1: ConceptLiteral, l2: ConceptLiteral)
  : Set[Set[ConceptLiteral]] = (l1.concept, l2.concept) match {
    case (UniversalRoleRestriction(r1: BaseRole, d1: BaseConcept),
    UniversalRoleRestriction(r2: BaseRole, d2: BaseConcept)) =>
      roleHierarchy.mostCommonSubRoles(r1,r2).map{ r3 =>
        val combineResult = definerFactory.combineDefiners(d1,d2)

        Set(ConceptLiteral(true,
          UniversalRoleRestriction(r3,
            combineResult._1))) }

    case (UniversalRoleRestriction(r1: BaseRole, d1: BaseConcept),
    ExistentialRoleRestriction(r2: BaseRole, d2: BaseConcept)) if roleHierarchy.isSuperRole(r1, r2) =>
      Set(Set(ConceptLiteral(true,
        ExistentialRoleRestriction(r2,
          definerFactory.combineDefiners(d1, d2)._1))))

    case (_: ExistentialRoleRestriction,
    _: UniversalRoleRestriction) => combineLiterals(l2, l1)
    case _ => Set()
  }

  def fullDerivations(c1: ConceptClause, l1: ConceptLiteral, c2: ConceptClause, l2: ConceptLiteral)
  : Set[ExtendedDerivation] =
    (l1.concept, l2.concept) match {
      case (UniversalRoleRestriction(r1: BaseRole, d1: BaseConcept),
      UniversalRoleRestriction(r2: BaseRole, d2: BaseConcept)) =>
        roleHierarchy.mostCommonSubRoles(r1,r2).map{ r3 =>
          val combineResult = definerFactory.combineDefiners(d1,d2)

          var premises = List[Expression]()

          if(!r1.equals(r3)){
            premises::=RoleSubsumption(r3,r1)
          }

          if(!r2.equals(r3)){
            premises::=RoleSubsumption(r3,r2)
          }

          premises = c1::c2::premises

          val combined = ConceptLiteral(true,
            UniversalRoleRestriction(r3,
              combineResult._1))

          ExtendedDerivation(premises,
            Set(c1.without(l1)
              ._with(c2.without(l2))
              ._with(combined)),
            this.getClass.getSimpleName)
          }

      case (UniversalRoleRestriction(r1: BaseRole, d1: BaseConcept),
      ExistentialRoleRestriction(r2: BaseRole, d2: BaseConcept)) if roleHierarchy.isSuperRole(r1, r2) =>
        val combined = ConceptLiteral(true,
          ExistentialRoleRestriction(r2,
            definerFactory.combineDefiners(d1, d2)._1))

        var premises = List[Expression]()

        if(!r1.equals(r2)){
          premises::=RoleSubsumption(r2,r1)
        }

        premises = c2::c1::premises

        Set(ExtendedDerivation(premises,
          Set(c1.without(l1)
            ._with(c2.without(l2))
            ._with(combined)),
          this.getClass.getSimpleName))

      case (_: ExistentialRoleRestriction,
      _: UniversalRoleRestriction) => fullDerivations(c2,l2, c1,l1)
      case _ => Set()
    }
}
