package uk.ac.man.cs.lethe.internal.dl.owlapi

import java.io.File

import org.semanticweb.HermiT.Reasoner
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.OWLOntology

object OWLReasonerFacade {

  def createReasoner(ontology: OWLOntology) =
    new Reasoner.ReasonerFactory().createReasoner(ontology)

  def isConsistent(ontology: OWLOntology): Boolean = {
    val manager = OWLManager.createOWLOntologyManager()
    val factory = manager.getOWLDataFactory()
    val reasoner = createReasoner(ontology)

    reasoner.isConsistent
  }

  def main(args: Array[String]) = {
    val file = new File(args(0))
    print(file.getName())

    try {
      val owlOnt = OWLApiInterface.getOWLOntology(file)

      try {
        print(" - " + owlOnt.getOntologyID.getOntologyIRI.toString)
      } catch {
        case _: Throwable => print(" - null")
      }

      try {
        if (isConsistent(owlOnt))
          println(" - consistent")
        else
          println(" - inconsistent")
      } catch {
        case t: Throwable => println("- reasoner exception: " + t)
      }
    } catch {
      case t: Throwable => throw (t) //println(" - exception: "+t)
    }
  }
}
