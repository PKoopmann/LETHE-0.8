import org.junit.Test
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.{IRI, OWLAxiom}
import uk.ac.man.cs.lethe.abduction.OWLAbducer

import java.io.File
import java.util
import scala.io.Source

class DiscoveredBugs {

  //@Test
  // Test does not terminate in reasonable time, as a huge DNF is created
  def performanceBug() = {

    val manager = OWLManager.createOWLOntologyManager()
    val factory = manager.getOWLDataFactory()

    val ontologyResource = getClass.getResource("broken-simplified-pizza.owl")
    val ontology = manager.loadOntologyFromOntologyDocument(new File(ontologyResource.toURI))

    val prefix = "http://www.co-ode.org/ontologies/pizza/pizza.owl#"

    val abducer = new OWLAbducer()
    abducer.setBackgroundOntology(ontology)

    val observationAx1 = factory.getOWLSubClassOfAxiom(factory.getOWLClass(IRI.create(prefix + "Margherita")),
      factory.getOWLClass(IRI.create(prefix + "VegetarianPizza")))

    val observation = new util.HashSet[OWLAxiom]()
    observation.add(observationAx1)

    val abducibles = ontology.getSignature()
    abducibles.remove(factory.getOWLClass(IRI.create(prefix + "VegetarianPizza")))
    abducibles.remove(factory.getOWLObjectProperty(IRI.create(prefix + "hasTopping")))

    abducer.setAbducibles(abducibles)
    val hypothesis = abducer.abduce(observation)

    println(hypothesis)
  }

  @Test
  def bug1() = {
    val manager = OWLManager.createOWLOntologyManager()
    val factory = manager.getOWLDataFactory()

    val ontologyResource = getClass.getResource("broken-simplified-pizza.owl")
    val ontology = manager.loadOntologyFromOntologyDocument(new File(ontologyResource.toURI))

    val prefix = "http://www.co-ode.org/ontologies/pizza#"

    val abducer = new OWLAbducer()
    abducer.setBackgroundOntology(ontology)

    val observationAx1 = factory.getOWLSubClassOfAxiom(factory.getOWLClass(IRI.create(prefix + "SpicyAmerican")),
      factory.getOWLClass(IRI.create(prefix + "SpicyPizza")))

    val observation = new util.HashSet[OWLAxiom]()
    observation.add(observationAx1)

    val abducibles = ontology.getSignature()
    abducibles.remove(factory.getOWLClass(IRI.create(prefix+"SpicyPizza")))

    abducer.setAbducibles(abducibles)
    val hypothesis = abducer.abduce(observation)

    println(hypothesis)
  }


  @Test
  def bug2() = {
    val manager = OWLManager.createOWLOntologyManager()
    val factory = manager.getOWLDataFactory()

    val ontologyResource = getClass.getResource("broken-simplified-pizza.owl")
    val ontology = manager.loadOntologyFromOntologyDocument(new File(ontologyResource.toURI))

    val prefix = "http://www.co-ode.org/ontologies/pizza/pizza.owl#"

    val abducer = new OWLAbducer()
    abducer.setBackgroundOntology(ontology)

    val observationAx1 = factory.getOWLSubClassOfAxiom(factory.getOWLClass(IRI.create(prefix + "SpicyAmerican")),
      factory.getOWLClass(IRI.create(prefix + "SpicyPizza")))

    val observation = new util.HashSet[OWLAxiom]()
    observation.add(observationAx1)

    val abducibles = ontology.getSignature()
    abducibles.remove(factory.getOWLClass(IRI.create(prefix + "SpicyPizza")))
    val hasSpiciness = factory.getOWLObjectProperty(IRI.create("http://www.co-ode.org/ontologies/pizza/pizza.owl#hasSpiciness"))
    abducibles.forEach(c => println("signature: "+c+", "+c.getEntityType))
    assert(abducibles.contains(hasSpiciness))
    abducibles.remove(hasSpiciness)

    abducer.setAbducibles(abducibles)
    val hypothesis = abducer.abduce(observation)

    println(hypothesis)
  }

}
