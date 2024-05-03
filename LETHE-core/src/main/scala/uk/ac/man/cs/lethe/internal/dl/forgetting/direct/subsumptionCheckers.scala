package uk.ac.man.cs.lethe.internal.dl.forgetting.direct

import com.typesafe.scalalogging.Logger
import uk.ac.man.cs.lethe.internal.dl.datatypes._
import uk.ac.man.cs.lethe.internal.dl.datatypes.extended.ExtendedABoxClause
import uk.ac.man.cs.lethe.internal.dl.forgetting.abox.ABoxClause

trait SubsumptionChecker {

  val logger = Logger[SubsumptionChecker]

  def subsumes(definer1: BaseConcept, definer2: BaseConcept): Boolean = {
    definer1==definer2
  }

  def subsumes(role1: Role, role2: Role): Boolean =
    role1==role2

  def subsumes(literal1: ConceptLiteral, literal2: ConceptLiteral): Boolean =
    (literal1, literal2) match {
      case (ConceptLiteral(p1, _), ConceptLiteral(p2, _)) if p1!=p2 => false
      case (_, ConceptLiteral(true, TopConcept)) => true
      case (ConceptLiteral(true, BottomConcept), _) => true
      case (ConceptLiteral(_, b1: BaseConcept),
      ConceptLiteral(_, b2: BaseConcept)) => subsumes(b1, b2)
      case (ConceptLiteral(_, UniversalRoleRestriction(r1: Role, b1: BaseConcept)),
      ConceptLiteral(_, UniversalRoleRestriction(r2: Role, b2: BaseConcept))) if subsumes(r1, r2) =>
        subsumes(b1, b2)
      case (ConceptLiteral(_, ExistentialRoleRestriction(r1: Role, b1: BaseConcept)),
      ConceptLiteral(_, ExistentialRoleRestriction(r2: Role, b2: BaseConcept))) if subsumes(r1, r2)=>
        subsumes(b1, b2)
      case _ => false
    }

  // A u B subsumes A u B u C
  def subsumes(clause1: ConceptClause, clause2: ConceptClause): Boolean = {
    val result = isTautology(clause2) ||
      clause1==clause2 ||
      clause1.literals.forall{ l1 =>
        clause2.literals.exists{ l2 =>
          val result = subsumes(l1, l2)
          if(result) logger.trace(s"${l1} subsumes ${l2}")
          result
        }
      }
    if(result)
      logger.trace(s"${clause1} subsumes ${clause2}")
    result
  }

  def subsumes(clause1: ABoxClause, clause2: ABoxClause): Boolean = {
    isTautology(clause2) ||
      clause1.literals.keySet.forall { ind => clause2.literals.get(ind) match {
        case None => false
        case Some(cl) => subsumes(clause1.literals(ind), cl)
      }}
  }

  def subsumes(clause1: ExtendedABoxClause, clause2: ExtendedABoxClause): Boolean = {
    isTautology(clause2) || (
      clause1.roleAssertions.forall(clause2.roleAssertions) &&
        clause1.negatedRoleAssertions.forall(clause2.negatedRoleAssertions) &&
        clause1.literals.keySet.forall { ind =>
          clause2.literals.get(ind) match {
            case None => false
            case Some(cl) => subsumes(clause1.literals(ind), cl)
          }
        }
      )
  }

  def isTautology(clause: ConceptClause): Boolean =
    clause.literals.exists(l => clause.literals.contains(ConceptLiteral(!l.polarity, l.concept)))

  def isTautology(clause: ABoxClause): Boolean =
    clause.literals.values.exists(isTautology)

  def isTautology(clause: ExtendedABoxClause): Boolean =
    clause.roleAssertions.exists(clause.negatedRoleAssertions) &&
      clause.literals.values.exists(isTautology)


  def condenseClause(clause: ConceptClause): ConceptClause = {
    clause.without(clause.literals.filter{ l1 => clause.literals.exists{ l2 => l1!=l2 && subsumes(l1,l2) } })
  }

  def condenseClause(aboxClause: ABoxClause): ABoxClause =
    new ABoxClause(aboxClause.literals.map{ _ match {
      case (a, cl) => (a, condenseClause(cl))
    }})

  def condenseClause(aboxClause: ExtendedABoxClause): ExtendedABoxClause =
    new ExtendedABoxClause(aboxClause.literals.map{ _ match {
      case (a, cl) => (a, condenseClause(cl))
    }}, aboxClause.roleAssertions, aboxClause.negatedRoleAssertions)

  // def condenseClause(clause: ConceptClause) = { 
  //   clause.without(clause.literals.filter{ _.concept match { 
  //     case UniversalRoleRestriction(r, f: BaseConcept) => clause.literals.exists{ _.concept match { 
  // 	case UniversalRoleRestriction(r2, f2: BaseConcept) if r2==r && f!=f2 =>  subsumes(f, f2)
  // 	case _ => false
  //     }}
  //     case ExistentialRoleRestriction(r, f: BaseConcept) => clause.literals.exists{ _.concept match { 
  // 	case ExistentialRoleRestriction(r2, f2: BaseConcept) if r2==r && f!=f2 =>  subsumes(f, f2)
  // 	case _ => false
  //     }}
  //     case _ => false
  //   }})
  // }


}

class SimpleSubsumptionChecker extends SubsumptionChecker // idea: combine with severeal traits

class CombinedSubsumptionChecker(subsumptionCheckers: Iterable[SubsumptionChecker])
  extends SubsumptionChecker {
  override def subsumes(definer1: BaseConcept, definer2: BaseConcept) = {
    subsumptionCheckers.exists(_.subsumes(definer1, definer2))
  }
}

class SubsumptionCheckerWithMap(var subsumptionMap: Map[BaseConcept, Set[BaseConcept]])
  extends SubsumptionChecker {

  override def subsumes(definer1: BaseConcept, definer2: BaseConcept) =
    subsumptionMap.contains(definer1) && subsumptionMap(definer1)(definer2)

  def addSubsumptions(add: Map[BaseConcept, Set[BaseConcept]]) = {
    add.foreach{ pair =>  {
      val (d, f) = pair
      if(!subsumptionMap.contains(d))
        subsumptionMap = subsumptionMap.updated(d, f)
      else
        subsumptionMap = subsumptionMap.updated(d, subsumptionMap(d)++f)
    }

    }
  }
}


