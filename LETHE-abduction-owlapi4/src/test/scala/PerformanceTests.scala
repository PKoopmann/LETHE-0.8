import org.junit.Test
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.{IRI, OWLAxiom}
import uk.ac.man.cs.lethe.abduction.OWLAbducer

import java.io.File

class PerformanceTests {



  @Test
  def performanceProblem() = {

    val manager = OWLManager.createOWLOntologyManager()
    val factory = manager.getOWLDataFactory()

    val ontologyResource = getClass.getResource("mod-pizza-23-09-21.owl")
    val ontology = manager.loadOntologyFromOntologyDocument(new File(ontologyResource.toURI))

    val prefix = "http://www.co-ode.org/ontologies/pizza/pizza.owl#"

    def getOWLClass(name: String) =
      factory.getOWLClass(IRI.create(prefix + name))

    def getOWLProperty(name: String) =
      factory.getOWLObjectProperty(IRI.create(prefix + name))

    val abducer = new OWLAbducer()
    abducer.setBackgroundOntology(ontology)

    val observationAx1 = factory.getOWLSubClassOfAxiom(
      getOWLClass("AmericanHot"),
      getOWLClass("SpicyPizza"))

    val observation = new java.util.HashSet[OWLAxiom]()
    observation.add(observationAx1)

    val abducibles = ontology.getSignature()
    abducibles.remove(getOWLClass("SpicyPizza"))
    abducibles.remove(getOWLClass("SpicyTopping"))
    abducibles.remove(getOWLProperty("hasSpiciness"))
    abducer.setAbducibles(abducibles)
    val hypothesis = abducer.abduce(observation)

    println(hypothesis)
  }
}
