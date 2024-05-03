package uk.ac.man.cs.lethe.internal.dl.owlapi

import scala.collection.JavaConverters._
import com.typesafe.scalalogging.Logger
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.io._
import org.semanticweb.owlapi.model._
import uk.ac.man.cs.lethe.internal.dl.datatypes.extended.{ConjunctiveAssertion, ConjunctiveDLStatement, DisjunctiveAssertion, DisjunctiveDLStatement, NegatedRoleAssertion}
import uk.ac.man.cs.lethe.internal.dl.datatypes._

import java.io.File

class OWLExporter {
  //  implicit val (logger, formatter, appender) = ZeroLoggerFactory.newLogger(this)

  val logger = Logger[OWLExporter]

  val manager = OWLManager.createOWLOntologyManager()

  val factory = manager.getOWLDataFactory()

  def exportOntology(ontology: Ontology, file: File) = {
    val manager = OWLManager.createOWLOntologyManager()

    val owlOntology = manager.createOntology()

    ontology.tbox.axioms.foreach(addAxiom(owlOntology, _))
    ontology.rbox.axioms.foreach(addAxiom(owlOntology, _))
    ontology.abox.assertions.foreach(addAxiom(owlOntology, _))

    val format = new RDFXMLOntologyFormat()

    manager.saveOntology(owlOntology, format, IRI.create(file.toURI()))
  }

  def toOwlOntology(ontology: Ontology): OWLOntology = {
    val manager = OWLManager.createOWLOntologyManager()

    val factory = manager.getOWLDataFactory()

    val owlOntology = manager.createOntology()

    ontology.tbox.axioms.foreach(axiom =>
      manager.addAxiom(owlOntology, toOwl(owlOntology, axiom)))
    ontology.rbox.axioms.foreach(addAxiom(owlOntology, _))
    ontology.abox.assertions.foreach(addAxiom(owlOntology, _))


    owlOntology
  }

  def toOwlOntology(axioms: Iterable[OWLAxiom]): OWLOntology = {
    val manager = OWLManager.createOWLOntologyManager()

    val factory = manager.getOWLDataFactory()

    val owlOntology = manager.createOntology()

    axioms.foreach(axiom =>
      manager.addAxiom(owlOntology, axiom))

    owlOntology
  }

  def save(owlOntology: OWLOntology, file: File,
           format: OWLOntologyFormat = new OWLXMLOntologyFormat()) = {
    val manager = OWLManager.createOWLOntologyManager()

    manager.saveOntology(owlOntology, format, IRI.create(file.toURI()))
  }

  def addAxiom(owlOntology: OWLOntology, axiom: Axiom) =
    manager.addAxiom(owlOntology, toOwl(owlOntology, axiom))

  def addAxiom(owlOntology: OWLOntology, axiom: RoleAxiom) =
    manager.addAxiom(owlOntology, toOwl(owlOntology, axiom))

  def addAxiom(owlOntology: OWLOntology, assertion: Assertion) =
    manager.addAxiom(owlOntology, toOwl(owlOntology, assertion))

  def toOwl(owlOntology: OWLOntology, statement: DLStatement): Set[OWLLogicalAxiom] = statement match {
    case ConjunctiveAssertion(cs) => toOwl(owlOntology, ConjunctiveDLStatement(cs.toSet[DLStatement]))
    case DisjunctiveAssertion(ds) => toOwl(owlOntology, DisjunctiveDLStatement(ds.toSet[DLStatement]))
    case NegatedRoleAssertion(r,a,b) => toOwl(owlOntology,
      ConceptAssertion(UniversalRoleRestriction(r, ConceptComplement(NominalSet(Set(b)))), a))
    case ConjunctiveDLStatement(cs) => cs.flatMap(toOwl(owlOntology,_))
    case DisjunctiveDLStatement(ds) =>
      Set(factory.getOWLClassAssertionAxiom(
        factory.getOWLObjectUnionOf(ds.map(toOwlConcept(owlOntology, _)).asJava),
        theIndividual(owlOntology)))
    case a: Assertion => Set(toOwl(owlOntology, a))
    case a: Axiom => Set(toOwl(owlOntology, a))
    case a: RoleAxiom => Set(toOwl(owlOntology, a))
  }

  def theIndividual(owlOntology: OWLOntology) =
    toOwl(owlOntology, Individual("a")) // doesn't really matter which one we pick here

  def toOwlConcept(owlOntology: OWLOntology, statement: DLStatement): OWLClassExpression = statement match {
    case ConceptAssertion(c, a) => toOwl(owlOntology,
      ExistentialRoleRestriction(TopRole, ConceptConjunction(Set(NominalSet(Set(a)), c))))
    case RoleAssertion(r, a, b) => toOwl(owlOntology,
      ExistentialRoleRestriction(TopRole,
        ConceptConjunction(Set(NominalSet(Set(a)),
          ExistentialRoleRestriction(r, NominalSet(Set(b)))))))
    case NegatedRoleAssertion(r,a,b) => toOwl(owlOntology,
      ExistentialRoleRestriction(TopRole,
        ConceptConjunction(Set(NominalSet(Set(a)),
          UniversalRoleRestriction(r, ConceptComplement(NominalSet(Set(b))))))))
    case ConjunctiveAssertion(cs) => toOwlConcept(owlOntology, ConjunctiveDLStatement(cs.toSet[DLStatement]))
    case DisjunctiveAssertion(ds) => toOwlConcept(owlOntology, DisjunctiveDLStatement(ds.toSet[DLStatement]))
    case ConjunctiveDLStatement(cs) => factory.getOWLObjectIntersectionOf(
      cs.map(toOwlConcept(owlOntology,_)).asJava)
    case DisjunctiveDLStatement(ds) => factory.getOWLObjectIntersectionOf(
      ds.map(toOwlConcept(owlOntology,_)).asJava)
    case Subsumption(c,d) => toOwl(owlOntology,
      UniversalRoleRestriction(TopRole, ConceptDisjunction(Set(ConceptComplement(c),d))))
    case ConceptEquivalence(c,d) => factory.getOWLObjectIntersectionOf(
      toOwlConcept(owlOntology, Subsumption(c,d)), toOwlConcept(owlOntology, Subsumption(d,c))
    )
  }

  def toOwl(owlOntology: OWLOntology, axiom: Axiom): OWLLogicalAxiom = axiom match {
    case Subsumption(subsumer, subsumee) =>
      factory.getOWLSubClassOfAxiom(toOwl(owlOntology, subsumer),
        toOwl(owlOntology, subsumee))
    case ConceptEquivalence(concept1, concept2) =>
      factory.getOWLEquivalentClassesAxiom(toOwl(owlOntology, concept1),
        toOwl(owlOntology, concept2))
  }

  def toOwl(owlOntology: OWLOntology, axiom: RoleAxiom): OWLLogicalAxiom = axiom match {
    case RoleSubsumption(r1, r2) => factory.getOWLSubObjectPropertyOfAxiom(toOwl(owlOntology, r1),
      toOwl(owlOntology, r2))
    case TransitiveRoleAxiom(r) => factory.getOWLTransitiveObjectPropertyAxiom(toOwl(owlOntology, r))
    case FunctionalRoleAxiom(r) => factory.getOWLFunctionalObjectPropertyAxiom(toOwl(owlOntology, r))
  }

  def toOwl(owlOntology: OWLOntology, assertion: Assertion): OWLIndividualAxiom = assertion match {
    case ConceptAssertion(c, a) => factory.getOWLClassAssertionAxiom(toOwl(owlOntology, c),
      toOwl(owlOntology, a))
    case RoleAssertion(r, a, b) => factory.getOWLObjectPropertyAssertionAxiom(toOwl(owlOntology, r),
      toOwl(owlOntology, a),
      toOwl(owlOntology, b))
    case DisjunctiveAssertion(ds) => toOwl(owlOntology,
      DisjunctiveDLStatement(ds.toSet[DLStatement])).head.asInstanceOf[OWLIndividualAxiom]
    case NegatedRoleAssertion(r,a,b) => toOwl(owlOntology,
      ConceptAssertion(UniversalRoleRestriction(r, ConceptComplement(NominalSet(Set(b)))), a))
  }

  def toOwl(owlOntology: OWLOntology, concept: Concept): OWLClassExpression = concept match {
    case TopConcept => factory.getOWLThing()
    case BottomConcept => factory.getOWLNothing()
    case BaseConcept(name) => factory.getOWLClass(toIRI(owlOntology, name))
    case ConceptComplement(concept) =>
      factory.getOWLObjectComplementOf(toOwl(owlOntology, concept))
    case ConceptConjunction(conjuncts) if conjuncts.size == 0 => toOwl(owlOntology, TopConcept)
    case ConceptConjunction(conjuncts) if conjuncts.size == 1 => toOwl(owlOntology, conjuncts.head)
    case ConceptConjunction(conjuncts) =>
      factory.getOWLObjectIntersectionOf(conjuncts.map(toOwl(owlOntology, _)).asJava)
    case ConceptDisjunction(disjuncts) if disjuncts.size == 0 => toOwl(owlOntology, BottomConcept)
    case ConceptDisjunction(disjuncts) if disjuncts.size == 1 => toOwl(owlOntology, disjuncts.head)
    case ConceptDisjunction(disjuncts) =>
      assert(disjuncts.size > 1, "invalid disjunction: only contains: " + disjuncts)
      factory.getOWLObjectUnionOf(disjuncts.map(toOwl(owlOntology, _)).asJava)
    case ExistentialRoleRestriction(role, concept) =>
      factory.getOWLObjectSomeValuesFrom(toOwl(owlOntology, role), toOwl(owlOntology, concept))
    case UniversalRoleRestriction(role, concept) =>
      factory.getOWLObjectAllValuesFrom(toOwl(owlOntology, role), toOwl(owlOntology, concept))
    case MinNumberRestriction(n, role, concept) =>
      factory.getOWLObjectMinCardinality(n, toOwl(owlOntology, role), toOwl(owlOntology, concept))
    case MaxNumberRestriction(n, role, concept) =>
      factory.getOWLObjectMaxCardinality(n, toOwl(owlOntology, role), toOwl(owlOntology, concept))
    case EqNumberRestriction(n, role, concept) =>
      factory.getOWLObjectExactCardinality(n, toOwl(owlOntology, role), toOwl(owlOntology, concept))

    case NominalSet(individuals: Set[Individual]) =>
      factory.getOWLObjectOneOf(individuals.map(toOwl(owlOntology, _)).asJava)
  }

  def toOwl(owlOntology: OWLOntology, role: BaseRole) = role match {
    case TopRole => factory.getOWLTopObjectProperty()
    case _ => factory.getOWLObjectProperty(toIRI(owlOntology, role.name))
  }

  def toOwl(owlOntology: OWLOntology, role: Role): OWLObjectPropertyExpression = role match {
    case baseRole: BaseRole => toOwl(owlOntology, baseRole)
    case InverseRole(role: BaseRole) => factory.getOWLObjectInverseOf(toOwl(owlOntology, role))
    case InverseRole(InverseRole(role)) => toOwl(owlOntology, role)
  }

  def toOwl(owlOntology: OWLOntology, individual: Individual): OWLIndividual =
    factory.getOWLNamedIndividual(toIRI(owlOntology, individual.name))

  def toIRI(owlOntology: OWLOntology, name: String): IRI =
//    if(name.contains("#") || name.contains("/"))
      IRI.create(name)
/*    else {
      OWLApiConfiguration.SIMPLIFIED_NAMES
      // val ontIRI = owlOntology.getOntologyID().getOntologyIRI() // <-- needed?
      // IRI.create(ontIRI.getNamespace(), name)
      IRI.create("http://example.com/ns/foo#"+name)
  }*/
}
