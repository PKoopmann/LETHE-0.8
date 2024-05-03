import uk.ac.man.cs.lethe.internal.dl.owlapi.{OWLApiConverter, OWLApiInterface, OWLReasonerFacade}

import java.util.function.Consumer
import org.semanticweb.owlapi.model.{OWLEntity, OWLOntology}
import uk.ac.man.cs.lethe.internal.dl.datatypes.{DLHelpers, Ontology}
import uk.ac.man.cs.lethe.internal.dl.forgetting.{ConceptAndRoleForgetter, QuickConceptForgetter, QuickRoleForgetter}
import uk.ac.man.cs.lethe.internal.dl.forgetting.abox.ABoxForgetter
import uk.ac.man.cs.lethe.internal.dl.forgetting.direct.AdvancedSymbolOrderings
import uk.ac.man.cs.lethe.internal.dl.interpolation.OntologyInterpolator
import uk.ac.man.cs.lethe.internal.dl.owlapi.OWLReasonerFacade
import uk.ac.man.cs.lethe.internal.forgetting.Forgetter
import uk.ac.man.cs.lethe.internal.tools.formatting.{SimpleDLFormatter, SimpleOWLFormatter}

import scala.collection.JavaConverters._

object SnomedExampleTests {

  def main(args: Array[String]) = {

    List(ABoxForgetter,
      QuickConceptForgetter,
      QuickRoleForgetter,
      ConceptAndRoleForgetter
    ).foreach{ forgetter =>

      println("Testing "+forgetter)

      val interpolator = new OntologyInterpolator(forgetter, delayedExistsElimination = false)

      val tester = new SnomedExampleTest(interpolator)

      tester.allTests

      val interpolator2 = new OntologyInterpolator(forgetter, delayedExistsElimination = true)

      val tester2 = new SnomedExampleTest(interpolator)

      tester2.allTests
    }
  }
}


class SnomedExampleTest(interpolator: OntologyInterpolator) {

  def allTests() = {
    test1
    test2
  }

  def test1(): Unit ={
    val ontologyStream = getClass.getResourceAsStream("/src/test/resources/bottom-example-ontology.owl")
    val sigStream = getClass.getResourceAsStream("/src/test/resources/bottom-example-sig.owl")

    val ontology = OWLApiInterface.getOWLOntologyFromStream(ontologyStream, true)
    val signature = OWLApiInterface.getSignature(sigStream)


    test(ontology,signature)
  }

  def test2(): Unit ={
    val ontologyStream = getClass.getResourceAsStream("/src/test/resources/bottom-example-ontology.owl")
    val sigStream = getClass.getResourceAsStream("/src/test/resources/bottom-example-sig.owl")

    val ontology = OWLApiInterface.getOWLOntologyFromStream(ontologyStream, true)
    val signature = OWLApiInterface.getSignature(ontology).filterNot(OWLApiInterface.getSignature(sigStream))

    test(ontology,signature)
  }

  private def test(ontology: OWLOntology, signature: Set[OWLEntity]) = {

    println("Input Ontology: ")
    println(SimpleOWLFormatter.format(ontology))

    val interpolant = interpolator.uniformInterpolant(ontology, signature)

    println("Interpolant: ")
    println(SimpleOWLFormatter.format(interpolant))

    verify(ontology,interpolant)
  }


  private def testStepwise(ontology: OWLOntology, signature: Set[OWLEntity]) = {

    val originalSignature = OWLApiInterface.getSignature(ontology)

    var currentInt = ontology

    var stillToForget = originalSignature.filterNot(signature)

      while(!stillToForget.isEmpty){

        var symbol = next(ontology, stillToForget)
        stillToForget -= symbol

        val newSignature = originalSignature - symbol

        val interpolant = interpolator.uniformInterpolant(ontology, newSignature)

        verify(ontology, interpolant)
    }
  }

  def next(ontology: OWLOntology, stillToForget: Set[OWLEntity]) = {
    val ontology2 = OWLApiConverter.convert(ontology)
    val name = AdvancedSymbolOrderings.orderByNumOfOccurrences(stillToForget.map(_.toString),ontology2).reverse.head

    toEntity(ontology, name)
  }

  def toEntity(ontology: OWLOntology, name: String) = {
    ontology.getSignature().asScala.find(_.toString.equals(name)).get
  }

  private def verify(ontology: OWLOntology, interpolant: OWLOntology)= {

    val reasoner = OWLReasonerFacade.createReasoner(ontology)



    interpolant.getAxioms().forEach(ax =>
      assert(reasoner.isEntailed(ax), s"unsound: ${SimpleOWLFormatter.format(ax)}"))

    println("All good!")
  }
}
