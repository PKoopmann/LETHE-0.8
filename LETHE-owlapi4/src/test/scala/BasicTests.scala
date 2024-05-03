import org.junit.Assert._
import org.junit.Test
import org.junit.Before
import org.scalatestplus.junit.AssertionsForJUnit
import uk.ac.man.cs.lethe.internal.tools.formatting.SimpleDLFormatter
//import org.semanticweb.HermiT.model.AtomicConcept
import uk.ac.man.cs.lethe.internal.dl.datatypes.{BaseConcept, BaseRole, CheapSimplifier, ConceptComplement, ConceptConjunction, ConceptDisjunction, ExistentialRoleRestriction, Subsumption, TopConcept, UniversalRoleRestriction}
import uk.ac.man.cs.lethe.internal.dl.forgetting.abox.ABoxForgetter
import uk.ac.man.cs.lethe.internal.dl.forgetting.direct.ALCFormulaPreparations
//import uk.ac.man.cs.lethe.internal.dl.owlapi.{OWLApiConfiguration, OWLApiConverter, OWLParser}
import uk.ac.man.cs.lethe.internal.dl.parsing.DLParser
//import uk.ac.man.cs.lethe.internal.fol.datatypes.Disjunction
//
import java.util
//
class BasicTests extends AssertionsForJUnit {

  @Test
  def simplificationTest(): Unit = {
    val axiom =
      Subsumption(TopConcept, ConceptDisjunction(Set(
        ExistentialRoleRestriction(BaseRole("r"),
          ConceptConjunction(Set(ConceptDisjunction(Set())))),
        UniversalRoleRestriction(BaseRole("r"), TopConcept),
        ConceptComplement(BaseConcept("A")),
        BaseConcept("B")
      ))
      )

    println(axiom)
    val simplified = CheapSimplifier.simplify(axiom)
    println(CheapSimplifier.simplify(axiom))
    assertEquals(simplified, Subsumption(TopConcept, TopConcept))
  }

  @Test
  def forgettingTestI(): Unit = {
    val ontology = DLParser.parse(
      "(Er.E n Ar.E n Es.E n As.E n A) <= B\n" +
        "A <= Ar.(E u C)\n" +
        "A <= (Er.C u Er.E)\n" +
        "C <= E\n" +
        "A <= (Es.C u Es.E)\n" +
        "A <= As.(E u C)"

    )
    val interpolant = ABoxForgetter.forget(ontology, Set("E"))

    println()
    println("Forgetting result: ")
    println(SimpleDLFormatter.format(interpolant))

  }

  @Test
  def forgettingTestH(): Unit = {
    val ontology = DLParser.parse(
      "A <= (Er.E u Er.C)\n" +
        "A <= (Es.E u Es.C)\n" +
        "C <= E \n" +
        "A <= Ar.(E u C)\n" +
        "A <= As.(E u C)\n" +
        "(Er.E n Ar.E n Es.E n As.E n A) <= B"
    )
    val interpolant = ABoxForgetter.forget(ontology, Set("E"))

    println()
    println("Forgetting result: ")
    println(SimpleDLFormatter.format(interpolant))

  }

  @Test
  def forgettingTestG(): Unit = {
    val ontology = DLParser.parse(
      "A <= (Er.E u Er.C)\n" +
        "C <= E \n" +
        "A <= Ar.(E u C)\n" +
        "(Er.E n Ar.E n A) <= B"
    )
    val interpolant = ABoxForgetter.forget(ontology, Set("E"))

    println()
    println("Forgetting result: ")
    println(interpolant)

  }

  @Test
  def forgettingTestF(): Unit = {
    val ontology = DLParser.parse(
      "(A n Ar.E) <= B\n" +
        "A <= Ar.(E u C)\n" +
        "C <= E"
    )
    val interpolant = ABoxForgetter.forget(ontology, Set("E"))

    println()
    println("Forgetting result: ")
    println(interpolant)

    assert(interpolant.tbox.axioms.contains(Subsumption(BaseConcept("A"), BaseConcept("B"))))
  }

  @Test
  def forgettingTestE() = {
    val ontology = DLParser.parse(
      "(A n Ar.E) <= B\n" +
        "A <= Ar.(E u C)\n" +
        "C <= E"
    )
    val interpolant = ABoxForgetter.forget(ontology, Set("E"))

    println()
    println("Forgetting result: ")
    println(interpolant)
  }

  @Test
  def forgettingTestD(): Unit = {
    val ontology = DLParser.parse(
      "(A n Er.C n Ar.C) <= B  \n" +
        "A <= (Er.C n Ar.C)"
    )
    val interpolant = ABoxForgetter.forget(ontology, Set("C"))

    println()
    println("Forgetting result: ")
    println(interpolant)

    assert(interpolant.tbox.axioms(Subsumption(BaseConcept("A"), BaseConcept("B"))))
  }

  @Test
  def forgettingTestC(): Unit = {
    val ontology = DLParser.parse(
      "A <= (Er.B n Ar.C)  \n" +
        "(B n C) <= (Er.B n Ar.C)"
    )
    val interpolant = ABoxForgetter.forget(ontology, Set("B", "C"))

    println()
    println("Forgetting result: ")
    println(interpolant)
  }

  @Test
  def forgettingTestB(): Unit = {
    val ontology = DLParser.parse(
      "A <= Er.(F n Es.TOP)  \n" +
        "(Er.F n Ar.F n A) <= B \n" +
        "A <= Ar.(F n Es.TOP)"
    )
    val interpolant = ABoxForgetter.forget(ontology, Set("F"))

    println()
    println("Forgetting result: ")
    println(interpolant)
  }
}
//
//  @Test
//  def forgettingTestA(): Unit ={
//    val manager = OWLManager.createOWLOntologyManager();
//    val ontology = manager.createOntology();
//    val factory = manager.getOWLDataFactory();
//
//    //"<C> <= <D>",
//    //     "disjoint(<D>, <E>)",
//    //     "<A> <= E<r>.<C>",
//    //     "<A> <= A<r>.<E>",
//    //     "disjoint(<B>, <D>)"
//
//    ontology.addAxiom(factory.getOWLSubClassOfAxiom(
//      factory.getOWLClass(IRI.create("C")),
//      factory.getOWLClass(IRI.create("D"))
//    ))
//    ontology.addAxiom(factory.getOWLDisjointClassesAxiom(
//      factory.getOWLClass(IRI.create("D")),
//      factory.getOWLClass(IRI.create("E"))
//    ))
//    ontology.addAxiom(factory.getOWLSubClassOfAxiom(
//      factory.getOWLClass(IRI.create("A")),
//      factory.getOWLObjectSomeValuesFrom(
//        factory.getOWLObjectProperty(IRI.create("r")),
//        factory.getOWLClass(IRI.create("C"))
//      )
//    ))
//    ontology.addAxiom(factory.getOWLSubClassOfAxiom(
//      factory.getOWLClass(IRI.create("A")),
//      factory.getOWLObjectAllValuesFrom(
//        factory.getOWLObjectProperty(IRI.create("r")),
//        factory.getOWLClass(IRI.create("E"))
//      )
//    ))
//    ontology.addAxiom(factory.getOWLDisjointClassesAxiom(
//      factory.getOWLClass(IRI.create("B")),
//      factory.getOWLClass(IRI.create("D"))
//    ))
//
//    val interpolator = new ShKnowledgeBaseInterpolator
//    interpolator.forgetter.noFiltration=true
//    interpolator.forgetter.clausifyOnce=true
//    interpolator.forgetter.conceptsFirst=true
//
//    val signature = new util.HashSet[OWLEntity]()
//    signature.add(factory.getOWLClass(IRI.create("A")))
//    signature.add(factory.getOWLClass(IRI.create("B")))
//
//    val interpolant = interpolator.uniformInterpolant(ontology, signature)
//
//    println()
//    println()
//
//    println(ALCFormulaPreparations.definerMap)
//    interpolator.forgetter.inferenceLogger.definerFactories.foreach(df =>
//      println(df.knownDefiners)
//    )
//
//    println("Interpolant: ")
//    println(SimpleOWLFormatter.format(interpolant))
//
//  }
//
//  @Test
//  def forgettingTestAminoAcid(): Unit = {
//    val ontologyUrl = "https://raw.githubusercontent.com/TheOntologist/AminoAcidOntology/master/amino-acid.owl"
//
//    val man = OWLManager.createOWLOntologyManager
//
//    var ontology = man.loadOntology(IRI.create(ontologyUrl))
//    val df = man.getOWLDataFactory
//
//    val glutamine = df.getOWLClass("http://www.co-ode.org/ontologies/amino-acid/2006/05/18/amino-acid.owl#Q")
//    val largeAliphatic = df.getOWLClass("http://www.co-ode.org/ontologies/amino-acid/2006/05/18/amino-acid.owl#LargeAliphaticAminoAcid")
//
//    OWLApiConfiguration.SIMPLIFIED_NAMES=true
//
//    val interpolator = new ShKnowledgeBaseInterpolator
//    interpolator.forgetter.noFiltration = true
//    interpolator.forgetter.clausifyOnce = true
//    interpolator.forgetter.conceptsFirst = true
//
//    val signature = new util.HashSet[OWLEntity]()
//    signature.add(glutamine)
//    signature.add(largeAliphatic)
//
//    val interpolant = interpolator.uniformInterpolant(ontology, signature)
//
//
//    println("Interpolant: ")
//    println(SimpleOWLFormatter.format(interpolant))
//  }
//}
