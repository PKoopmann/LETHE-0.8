package uk.ac.man.cs.lethe.internal.dl.forgetting.direct

import scala.collection.mutable.{ HashMap, Set => MutSet, MultiMap }

//import com.dongxiguo.fastring.Fastring.Implicits._


import uk.ac.man.cs.lethe.internal.dl.datatypes._
import uk.ac.man.cs.lethe.internal.dl.forgetting.Configuration


class TransitivityRule(transitiveRoles: Set[BaseRole], roleHierarchy: RoleHierarchy) extends Rule {

  val transitiveSubRoles = {
    var _transitiveSubRoles = new HashMap[BaseRole, MutSet[BaseRole]]() with MultiMap[BaseRole, BaseRole]

    transitiveRoles.foreach{ transRole => roleHierarchy.getSuperRoles(transRole).foreach{ superRole =>
      _transitiveSubRoles.addBinding(superRole.asInstanceOf[BaseRole], transRole)
    } }

    _transitiveSubRoles
  }

  override def getDerivations(clause: ConceptClause, clauses: Iterable[ConceptClause])
  : Set[Derivation] = {

    def derivationsFor(role: BaseRole, definer: BaseConcept) =
      Set(Derivation(Some(clause),
        Some(new ConceptClause(Set(ConceptLiteral(false, definer),
          ConceptLiteral(true, UniversalRoleRestriction(role, definer))),
          clause.ordering)), rule=this))

    clause.literals.flatMap { _.concept match {
      case UniversalRoleRestriction(r: BaseRole, definer: BaseConcept) => transitiveSubRoles(r).flatMap(r2 => derivationsFor(r2, definer))
      case MaxNumberRestriction(0, r: BaseRole, ConceptComplement(definer: BaseConcept)) => transitiveSubRoles(r).flatMap(r2 => derivationsFor(r2, definer))
      case _ => Set[Derivation]()
    }}
  }
}

object SHQRuleLogger

class NumberRestrictionRule(roleHierarchy: RoleHierarchy,
                            definerFactory: DefinerFactory) extends LiteralBasedRule {
  val rules = Set(new MaxMaxRule(roleHierarchy, definerFactory),
    new MaxMinRule(roleHierarchy, definerFactory),
    new MinMaxRule(roleHierarchy, definerFactory),
    new MinMinRule(roleHierarchy, definerFactory))

  override def combineLiterals(literal1: ConceptLiteral,
                               literal2: ConceptLiteral): Set[Set[ConceptLiteral]] = {
    rules.flatMap(_.combineLiterals(literal1, literal2))
  }
}


class MaxMaxRule(roleHierarchy: RoleHierarchy,
                 definerFactory: DefinerFactory) extends LiteralBasedRule {


  override def combineLiterals(literal1: ConceptLiteral,
                               literal2: ConceptLiteral): Set[Set[ConceptLiteral]] = {

    (literal1.concept, literal2.concept) match {
      case (MaxNumberRestriction(n1, r1: BaseRole, ConceptComplement(d1: BaseConcept)),
      MaxNumberRestriction(n2, r2: BaseRole, ConceptComplement(d2: BaseConcept))) =>
        roleHierarchy.mostCommonSubRoles(r1, r2).map{ r =>
          // println("MaxMax!")
          // println(ConceptLiteral(true,
          // 		 MaxNumberRestriction(n1+n2,
          // 				      r,
          // 				      ConceptComplement(definerFactory.combineDefiners(d1,d2)._1))))
          Set(ConceptLiteral(true,
            MaxNumberRestriction(n1+n2,
              r,
              ConceptComplement(definerFactory.combineDefiners(d1,d2)._1))))
        }
      case _ =>
        Set()
    }
  }
}

class MinMaxRule(roleHierarchy: RoleHierarchy,
                 definerFactory: DefinerFactory) extends LiteralBasedRule {


  override def combineLiterals(literal1: ConceptLiteral,
                               literal2: ConceptLiteral): Set[Set[ConceptLiteral]] = {
    (literal1.concept, literal2.concept) match {
      case (MinNumberRestriction(n1, r1: BaseRole, d1: Concept),
      MaxNumberRestriction(n2, r2: BaseRole, ConceptComplement(d2: Concept)))
        if (n1>n2 && roleHierarchy.subsumedBy(r1, r2)) => {
        // println("MinMax!")
        // println(ConceptLiteral(true,
        // 		       MinNumberRestriction(n1-n2,
        // 					    r2,
        // 					    definerFactory.combine(d1, d2))))
        Set(Set(ConceptLiteral(true,
          MinNumberRestriction(n1-n2,
            r2,
            definerFactory.combine(d1, d2)))))
      }
      case (MinNumberRestriction(n1, r1: BaseRole, d1: Concept),
      MaxNumberRestriction(n2, r2: BaseRole, ConceptComplement(d2: Concept)))
        if (n1<n2 && roleHierarchy.subsumedBy(r2, r1)) =>
        combineLiterals(literal2, literal1)
      case _ => Set()
    }
  }
}

class MinMinRule(roleHierarchy: RoleHierarchy,
                 definerFactory: DefinerFactory) extends LiteralBasedRule {

  import DLHelpers._

  override def combineLiterals(literal1: ConceptLiteral,
                               literal2: ConceptLiteral): Set[Set[ConceptLiteral]] = {
    (literal1.concept, literal2.concept) match {
      case (MinNumberRestriction(n1, r1: BaseRole, d1: Concept),
      MinNumberRestriction(n2, r2: BaseRole, d2: Concept))
      =>
        //	println("MinMin!")
        if(n1>=n2){
          roleHierarchy.mostCommonSuperRoles(r1, r2).flatMap{ r =>
            (1 to n2).map{ i2 =>
              val i1 = n1+n2+1-i2
              // println(ConceptLiteral(true, MinNumberRestriction(i1, r, disjunction(d1, d2)))+", "+
              // 	      ConceptLiteral(true, MinNumberRestriction(i2, r, definerFactory.combine(d1, d2))))
              Set(ConceptLiteral(true, MinNumberRestriction(i1, r, disjunction(d1, d2))),
                ConceptLiteral(true, MinNumberRestriction(i2, r, definerFactory.combine(d1, d2))))
            }
          }
        } else
          combineLiterals(literal2, literal1)
      case _ => Set()
    }
  }
}

class MaxMinRule(roleHierarchy: RoleHierarchy,
                 definerFactory: DefinerFactory) extends LiteralBasedRule {

  import DLHelpers._

  override def combineLiterals(literal1: ConceptLiteral,
                               literal2: ConceptLiteral): Set[Set[ConceptLiteral]] = {
    (literal1.concept, literal2.concept) match {
      case (MaxNumberRestriction(n1, r1: BaseRole, ConceptComplement(d1: Concept)),
      MinNumberRestriction(n2, r2: BaseRole, d2: Concept))
        if(n1>=n2 && roleHierarchy.subsumedBy(r2, r1)) => {
        (1 to n2).toSet[Int].map{ i2 =>
          val i1 = n1-n2-1+i2
          // println("MaxMin!")
          // println(ConceptLiteral(true, MaxNumberRestriction(i1, r1, ConceptComplement(disjunction(d1, d2)))),
          // 	  ConceptLiteral(true, MinNumberRestriction(i2, r1,
          // 						    ConceptComplement(definerFactory.combine(d1, d2)))))
          Set(ConceptLiteral(true, MaxNumberRestriction(i1, r1, ConceptComplement(disjunction(d1, d2)))),
            ConceptLiteral(true, MinNumberRestriction(i2, r1,
              definerFactory.combine(d1, d2))))
        }

      }
      case (MaxNumberRestriction(n1, r1: BaseRole, ConceptComplement(d1: Concept)),
      MinNumberRestriction(n2, r2: BaseRole, d2: Concept))
        if(n2>=n1 && roleHierarchy.subsumedBy(r1, r2)) =>
        combineLiterals(literal2, literal1)
      case _ => Set()
    }
  }
}

object MinEliminationRule extends LiteralBasedRule {
  // implements both min elimination and min resolution rule from paper


  override def derive(clause1: ConceptClause, literal1: ConceptLiteral,
                      clause2: ConceptClause, literal2: ConceptLiteral)
  : Set[ConceptClause] = {

    if(clause2.literals.size>1)
      return Set()

    val rest = (clause1.literals)-literal1

    combineLiterals(literal1, literal2)
      .filter(l => !l.equals(literal1) && !l.equals(literal2))
      .map{ literals =>
        new ConceptClause(rest++literals, clause1.ordering)
      }

  }

  override def combineLiterals(literal1: ConceptLiteral,
                               literal2: ConceptLiteral): Set[Set[ConceptLiteral]] = {
    (literal1.concept, literal2) match {
      case (MinNumberRestriction(n,r,d1: BaseConcept),
      ConceptLiteral(false, d2: BaseConcept)) if d1==d2 => Set(Set())
      case (MinNumberRestriction(n,r, ConceptDisjunction(ds)),
      ConceptLiteral(false, d2: BaseConcept)) if ds(d2) =>
        Set(Set(ConceptLiteral(true, MinNumberRestriction(n,r,DLHelpers.disjunction(ds-d2)))))
      case _ => Set()
    }
  }
}

// class MinMinRule(roleHierarchy: RoleHierarchy,
// 		 definerFactory: DefinerFactory) extends Rule { 

//   import CardinalityHelpers._

//   override def getDerivations(clause: ConceptClause, clauses: Iterable[ConceptClause]): Iterable[Derivation] = { 

//     val partners: Iterable[(MinNumberRestriction, ConceptClause)] = 
//       clauses.flatMap(cl => 
// 	getMins(cl).map(min => (min, cl)))

//     getMins(clause).flatMap{ mnr1 => mnr1 match { 
//       case MinNumberRestriction(_, r1: BaseRole, _) => 
// 	val restClause1 = clause.literals - ConceptLiteral(true, mnr1)
// 	partners.flatMap{ _ match { 
// 	  case (mnr2, clause2) => mnr2 match { 
// 	    case MinNumberRestriction(_, r2: BaseRole, _) => 
// 	      val restClause2 = clause2.literals - ConceptLiteral(true, mnr2)

// 	      roleHierarchy.mostCommonSuperRoles(r1, r2).map{ r3 => 
// 		Derivation(Set(clause, clause2),
// 			   countDown(restClause1++restClause2,
// 				     mnr1,
// 				     mnr2,
// 				     r3,
// 				     clause.ordering))
// 	      }
// 	  }
// 	}}												      
//     }}
//   }

//   def countDown(clausePrefix: Set[ConceptLiteral], 
// 		min1: MinNumberRestriction,
// 		min2: MinNumberRestriction,
// 		role: Role,
// 		ordering: Ordering[ConceptLiteral])
//   : Set[ConceptClause] = (min1, min2) match { 
//     case (MinNumberRestriction(n1, _, d1: BaseConcept), 
// 	  MinNumberRestriction(n2, _, d2: BaseConcept)) => { 
// 	    val filler1 = ConceptDisjunction(Set(d1, d2))
// 	    val (filler2, definition) = definerFactory.combineDefiners(d1, d2)
// 	    (0 to Math.min(n1, n2)).map{ n => 
// 	      new ConceptClause(clausePrefix + 
// 				ConceptLiteral(true, MinNumberRestriction(n1+n2-n,
// 									  role,
// 									  filler1)) +
// 				ConceptLiteral(true, MinNumberRestriction(n+1,
// 									  role,
// 									  filler2)),
// 				ordering)
// 	    }.toSet[ConceptClause] ++ definition
// 	  }
//   }
// }





object CardinalityHelpers {
  def onlyNumberRestrictions(clause: ConceptClause) = {
    new ConceptClause(clause.literals.map(_ match {
      case ConceptLiteral(true, UniversalRoleRestriction(r, c)) =>
        ConceptLiteral(true, MaxNumberRestriction(0, r, ConceptComplement(c)))
      case ConceptLiteral(true, ExistentialRoleRestriction(r, c)) =>
        ConceptLiteral(true, MinNumberRestriction(1, r, c))
      case l => l
    }), clause.ordering)
  }

  def asMax(concept: Concept) = concept match {
    case UniversalRoleRestriction(r, c) => MaxNumberRestriction(0, r, ConceptComplement(c))
    case _ => concept
  }

  def asMin(concept: Concept) = concept match {
    case ExistentialRoleRestriction(r, c) => MinNumberRestriction(1, r, c)
    case _ => concept
  }

  def getMaxs(conceptClause: ConceptClause): Set[MaxNumberRestriction] =
    conceptClause.literals.flatMap(l => asMax(l.concept) match {
      case mnr: MaxNumberRestriction => Some(mnr)
      case _ => Set[MaxNumberRestriction]()
    })

  def getMins(conceptClause: ConceptClause): Set[MinNumberRestriction] =
    conceptClause.literals.flatMap(l => asMin(l.concept) match {
      case mnr: MinNumberRestriction => Some(mnr)
      case _ => Set[MinNumberRestriction]()
    })
}


