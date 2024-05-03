package uk.ac.man.cs.lethe.internal.klappoExperiments

import scala.collection.JavaConverters._
import java.io.File
import java.util.Date

import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.io.OWLXMLOntologyFormat
import org.semanticweb.owlapi.formats.ManchesterSyntaxDocumentFormat
import org.semanticweb.owlapi.model.{IRI, OWLAxiom, OWLLogicalAxiom, OWLOntology, OWLOntologyManager}
import uk.ac.man.cs.lethe.internal.dl.owlapi.OWLExporter


object ExtractSmallSubOntology { 

  var owlOntologyManager: OWLOntologyManager = _
  
  def main(args: Array[String]) = { 
    owlOntologyManager = OWLManager.createOWLOntologyManager()

    println(new Date())
    println("Loading ontology...")
    val owlOntology = owlOntologyManager.loadOntologyFromOntologyDocument(new File(args(0)))
    println("Loaded")
    println(new Date())

    val subset = subSet(owlOntology, args(1).toInt)

    owlOntologyManager.saveOntology(subset, 
				    new ManchesterSyntaxDocumentFormat(),
				    IRI.create(new File("reducedOnt.owl")))
  }  

  def subSet(ont: OWLOntology, numAxioms: Int) = { 
    var axioms = ont.getLogicalAxioms().iterator.asScala.slice(0,numAxioms).toSet[OWLAxiom]
    axioms = axioms// ++ getAnnotations(ont, axioms.flatMap(_.getClassesInSignature))
    (new OWLExporter).toOwlOntology(ont.getLogicalAxioms().iterator.asScala.slice(0,numAxioms).toSet)
  }


/*  def getAnnotations(ontology: OWLOntology, classes: Iterable[OWLClass]): Iterable[OWLAnnotationAssertionAxiom] = {
    classes.flatMap{ _.getAnnotationAssertionAxioms(ontology)}
  }*/
}

object VersionGenerator { 

  var owlOntologyManager: OWLOntologyManager = _
  
  val outputPrefix = "Version"

  def main(args: Array[String]) = { 
    owlOntologyManager = OWLManager.createOWLOntologyManager()

    println(new Date())
    println("Loading ontology...")
    val owlOntology = owlOntologyManager.loadOntologyFromOntologyDocument(new File(args(0)))
    println("Loaded")
    println(new Date())

    println("Removing non-logical axioms")
    owlOntology.getAxioms().iterator().asScala.foreach{ ax =>
      if(!ax.isInstanceOf[OWLLogicalAxiom])
	owlOntologyManager.removeAxiom(owlOntology, ax)
    }

    generateVersions(owlOntology, 
		     number = 100, 
		     axiomsToRemoveEach = 10000)
  }

  def generateVersions(owlOntology: OWLOntology, 
		       number: Int,
		       axiomsToRemoveEach: Int) = { 
    (1 to number).foreach{ i =>

      removeAxioms(owlOntology, axiomsToRemoveEach)

      val file = new File(outputPrefix+i)
      println(""+new Date()+file)
      
      owlOntologyManager.saveOntology(owlOntology, 
				      new OWLXMLOntologyFormat(),
				      IRI.create(file.toURI))

    }
  }

  def removeAxioms(owlOntology: OWLOntology,
		   number: Int) = { 
    println("Removing "+number+" axioms")
    val toRemove = asScalaSet(owlOntology.getLogicalAxioms).take(number)
    owlOntologyManager.removeAxioms(owlOntology, toRemove.asJava)
    // (1 to number).foreach{ i =>
    //   val axiom = owlOntology.logicalAxioms().iterator.asScala.head
    //   owlOntologyManager.removeAxiom(owlOntology, axiom)
    // }
  }

}
