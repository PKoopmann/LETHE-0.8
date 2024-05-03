/**
 * Purification of roles
 */

package uk.ac.man.cs.lethe.internal.dl.forgetting

import scala.collection.mutable.{HashMap, HashSet, MultiMap, Set => MutSet}
import uk.ac.man.cs.lethe.internal.dl.datatypes._
import uk.ac.man.cs.lethe.internal.dl.forgetting.direct.RoleHierarchy
import uk.ac.man.cs.lethe.internal.forgetting.Forgetter

object QuickRoleForgetter extends Forgetter[Ontology,String] {
  override def forget(ontology: Ontology, symbols: Set[String]): Ontology = {
    val roleHierarchy = new RoleHierarchy(ontology)

    val quickRoleForgetter = new QuickRoleForgetter(ontology,symbols.map(BaseRole), roleHierarchy)

    quickRoleForgetter.purify()
  }

  override def steps: Int = 0
}

class QuickRoleForgetter(var ontology: Ontology,
                         var roleSymbols: Set[Role],
                         roleHierarchy: RoleHierarchy) {

  val below = new HashMap[Role, Set[Role]]()// with MultiMap[Role, Role]
  val belowL = new HashMap[Role, Set[Role]]()// with MultiMap[Role, Role]  
  val aboveL = new HashMap[Role, Set[Role]]()// with MultiMap[Role, Role]


  val TOP = new BaseRole("___TOP___")
  val BOT = new BaseRole("___BOT___")

  {
    def getBelow(roleSymbol: Role): Set[Role] = {

      val processed = new HashSet[Role]()

      def inner(roleSymbol: Role): Set[Role] = {
        if(processed(roleSymbol))
          return Set(roleSymbol)
        else
          processed.add(roleSymbol)

        if(roleSymbols(roleSymbol)){
          roleHierarchy.getDirectSubRoles(roleSymbol).flatMap(inner)
        } else {
          Set(roleSymbol.asInstanceOf[Role])
        }
      }

      inner(roleSymbol)
    }

    def getBelowL(roleSymbol: Role): Set[Role] = {
      val processed = new HashSet[Role]()

      def inner(roleSymbol: Role): Set[Role] = {
        if(processed(roleSymbol))
          return Set(roleSymbol.asInstanceOf[Role])
        else
          processed.add(roleSymbol)

        if(roleSymbols(roleSymbol)){
          roleHierarchy.getDirectSubRoles(roleSymbol) match {
            case s: Set[Role] if s.isEmpty => Set(roleSymbol.asInstanceOf[Role])
            case s: Set[Role] => s.flatMap(inner)
          }
        } else {
          Set(roleSymbol.asInstanceOf[Role])
        }
      }

      inner(roleSymbol)
    }

    def getAboveL(roleSymbol: Role): Set[Role] = {
      val processed = new HashSet[Role]()

      def inner(roleSymbol: Role): Set[Role] = {
        if(processed(roleSymbol))
          return Set(roleSymbol.asInstanceOf[Role])
        else
          processed.add(roleSymbol)

        if(roleSymbols(roleSymbol.asInstanceOf[Role])){
          roleHierarchy.getDirectSuperRoles(roleSymbol) match {
            case s: Set[Role] if s.isEmpty => Set(roleSymbol.asInstanceOf[Role])
            case s: Set[Role] => s.flatMap(inner)
          }
        } else {
          Set(roleSymbol.asInstanceOf[Role])
        }
      }

      inner(roleSymbol)
    }


    roleSymbols.foreach{ r => below.put(r, getBelow(r))
      belowL.put(r, getBelowL(r))
      aboveL.put(r, getAboveL(r)) }


  }

  def purify() = {

    ontology = DLHelpers.nnf(ontology)

    val purifiable = onlyUniversallyRestricted(ontology, roleSymbols.filter(below(_).isEmpty))

    //    println("purified "+purifiable+" role symbols")

    ontology.tbox.axioms = ontology.tbox.axioms.map(purifyUniversal(_, purifiable))

    ontology
  }

  //  def quickForget() {
  //    def replaceAboveBelowC(concept: Concept): Concept = concept match {
  //       case TopConcept => TopConcept
  //       case BottomConcept => BottomConcept
  //       case _: BaseConcept => concept
  //       case ConceptComplement(_: BaseConcept) => concept
  //       case ConceptConjunction(cs) => ConceptConjunction(cs.map(replaceAboveBelowC))
  //       case ConceptDisjunction(ds) => ConceptDisjunction(ds.map(replaceAboveBelowC))
  //       case ExistentialRoleRestriction(r: Role, c) if roleSymbols(r) => {
  // 	val c2 = replaceAboveBelowC(c)
  // 	ConceptConjunction(aboveL(r).map(ExistentialRoleRestriction(_, c2)))
  //       }
  //       case ExistentialRoleRestriction(r, c) => ExistentialRoleRestriction(r, replaceAboveBelowC(c))
  //       case UniversalRoleRestriction(r: Role, c) if roleSymbols(r) => {
  // 	val c2 = replaceAboveBelowC(c)
  // 	ConceptConjunction(belowL(r).map(UniversalRoleRestriction(_, c2)))
  //       }
  //       case UniversalRoleRestriction(r, c) => UniversalRoleRestriction(r, replaceAboveBelowC(c))
  //     }

  //     def replaceAboveBelow(axiom: Axiom): Axiom = axiom match {
  //       case Subsumption(TopConcept, concept) => Subsumption(TopConcept, replaceAboveBelowC(concept))
  //     }

  //     ontology.tbox.axioms = ontology.tbox.axioms.map(replaceAboveBelow)
  //     val oldSymbols = roleSymbols
  //     roleSymbols = (aboveL.values.flatten.toSet ++ belowL.values.flatten.toSet).filter(roleSymbols)
  //     removed ++= (oldSymbols--roleSymbols)
  // //    println("Quickly forgotten "+(oldSymbols--roleSymbols).size)

  // <---- All this bullshit is not sound!
  //  }


  def purifyUniversal(axiom: Axiom, purifiable: Set[Role]): Axiom = axiom match {
    case Subsumption(TopConcept, c) => Subsumption(TopConcept, purifyUniversal(c, purifiable))
  }

  def purifyUniversal(c: Concept, purifiable: Set[Role]): Concept = c match {
    case TopConcept => TopConcept
    case BottomConcept => BottomConcept
    case b: BaseConcept => b
    case ConceptComplement(_: BaseConcept) => c
    case ConceptComplement(TopConcept) => c
    case ConceptComplement(BottomConcept) => c
    case ConceptDisjunction(ds) => ConceptDisjunction(ds.map(purifyUniversal(_, purifiable)))
    case ConceptConjunction(cs) => ConceptConjunction(cs.map(purifyUniversal(_, purifiable)))
    case ExistentialRoleRestriction(r,c) => ExistentialRoleRestriction(r, purifyUniversal(c, purifiable))
    case UniversalRoleRestriction(r: Role,c) if purifiable(r) => TopConcept
    case UniversalRoleRestriction(r,c) => UniversalRoleRestriction(r, purifyUniversal(c, purifiable))
  }

  def onlyUniversallyRestricted(ontology: Ontology, roleSymbols: Set[_ <: Role]): Set[Role] = {
    val result = new HashSet[Role]()
    roleSymbols.foreach(result.add)

    def check(concept: Concept): Unit = concept match {
      case BottomConcept => ;
      case TopConcept => ;
      case _: BaseConcept => ;
      case ConceptComplement(_: BaseConcept) => ;
      case ConceptComplement(TopConcept) => ;
      case ConceptComplement(BottomConcept) => ;
      case ConceptConjunction(cs) => cs.foreach(check)
      case ConceptDisjunction(ds) => ds.foreach(check)
      case ExistentialRoleRestriction(r: Role, c) => result.remove(r); check(c)
      case UniversalRoleRestriction(_: Role, c) => check(c)
    }

    ontology.tbox.axioms.foreach{ ax =>
      if(result.isEmpty)
        return result.toSet
      ax match {
        case Subsumption(TopConcept, c) => check(c)
        case _ => assert(false)
      }
    }

    ontology.abox.assertions.foreach{ _ match {
      case ConceptAssertion(c, a) => check(c);
      case DisjunctiveConceptAssertion(cas) => cas.foreach(ca => check(ca.concept));
      case RoleAssertion(r: Role, _, _) => result.remove(r)
    } }

    return result.toSet
  }
}
