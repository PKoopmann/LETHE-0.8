package uk.ac.man.cs.lethe.internal.dl.forgetting

import uk.ac.man.cs.lethe.internal.dl.{AbstractMappedReasoner, AbstractMappedReasonerFactory}
import uk.ac.man.cs.lethe.internal.dl.datatypes.{BaseConcept, BaseRole, BottomConcept, Concept, ConceptComplement, ConceptConjunction, ConceptDisjunction, ConceptEquivalence, ExistentialRoleRestriction, MaxNumberRestriction, MinNumberRestriction, Ontology, Role, Subsumption, TBox, TopConcept, UniversalRoleRestriction}
//import uk.ac.man.cs.lethe.internal.dl.owlapi.OWLReasonerFacade
import uk.ac.man.cs.lethe.internal.forgetting.Forgetter
import uk.ac.man.cs.lethe.internal.tools.Timeoutable

/**
 * Forgetting of role names under the assumption that the underlying ontology is saturated wrt. the role propagation rule,
 * and the roles only occur in the TBox. Replaced unsatisfiable existential role restrictions by BOTTOM, and all other
 * occurrences of the role by TOP.
 */
object ExistsEliminator extends Forgetter[Ontology,BaseRole] with Timeoutable {

  var reasonerFactory: AbstractMappedReasonerFactory = _

  def setReasonerFactory(reasonerFactory: AbstractMappedReasonerFactory) =
    this.reasonerFactory=reasonerFactory

  override def forget(ontology: Ontology, symbols: Set[BaseRole]): Ontology = {
    val eliminator = new ExistsEliminator(ontology, reasonerFactory.newReasoner(ontology))

    transferTimeoutInformation(eliminator)

    eliminator.forget(symbols.toSet[Role])
  }


  override def steps: Int = 0
}

class ExistsEliminator(ontology: Ontology, reasoner: AbstractMappedReasoner) extends Timeoutable {

  //val reasoner = new MappedReasoner(ontology)

  def forget(roleNames: Set[Role]): Ontology = {

    val axioms2 = ontology.tbox.axioms.map{checkCanceled; _ match {
      case Subsumption(c1, c2) => Subsumption(forget(roleNames, c1), forget(roleNames, c2))
      case ConceptEquivalence(c1, c2) => ConceptEquivalence(forget(roleNames, c1), forget(roleNames, c2))
      case ax => ax
    }}

    new Ontology(TBox(axioms2), ontology.abox, ontology.rbox)
  }

  def forget(roleNames: Set[Role], concept: Concept): Concept = concept match {
    case TopConcept => TopConcept
    case BottomConcept => BottomConcept
    case c:BaseConcept => c
    case ConceptComplement(concept) => ConceptComplement(forget(roleNames,concept))
    case ConceptConjunction(cs) => ConceptConjunction(cs.map(forget(roleNames, _)))
    case ConceptDisjunction(ds) => ConceptDisjunction(ds.map(forget(roleNames,_)))

    case UniversalRoleRestriction(r,c) if roleNames(r) => TopConcept
    case UniversalRoleRestriction(r,c) =>
      UniversalRoleRestriction(r,forget(roleNames,c))

    case ExistentialRoleRestriction(r,c) if roleNames(r) && !reasoner.isSatisfiable(c) =>
      println("Eliminate: "+ ExistentialRoleRestriction(r,c))
      BottomConcept
    case ExistentialRoleRestriction(r,c) if roleNames(r)  =>
      TopConcept
    case ExistentialRoleRestriction(r,c) =>
      ExistentialRoleRestriction(r,forget(roleNames,c))

    case MaxNumberRestriction(number, role, filler) if roleNames(role) =>
      TopConcept
    case MaxNumberRestriction(number, role, filler) =>
      MaxNumberRestriction(number,role,forget(roleNames, filler))

    case MinNumberRestriction(number, role, filler) if roleNames(role) && !reasoner.isSatisfiable(filler) =>
      BottomConcept
    case MinNumberRestriction(number, role, filler) if roleNames(role) =>
      TopConcept
    case MinNumberRestriction(number,role,filler) =>
      MinNumberRestriction(number,role,forget(roleNames,filler))
  }
}
