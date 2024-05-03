package uk.ac.man.cs.lethe.abduction.cmd

import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.parameters.Imports
import org.semanticweb.owlapi.model.{IRI, OWLEntity, OWLOntologyManager}
import uk.ac.man.cs.lethe.abduction.{DLStatementAdapter, OWLAbducer}

import java.io.{File, FileOutputStream, FileWriter}
import scala.collection.JavaConverters.{asScalaSetConverter, setAsJavaSetConverter}
import scala.io.Source

object AbductionCmd {
    def main(args: Array[String]) : Unit = {
      if(args.size!=4 && args.size!=5)
        printInfoAndExit()

      val ontologyFile = args(0)
      val observationFile = args(1)
      val conceptNamesFile = args(2)
      val roleNamesFile = args(3)

      val hypothesisNumber =
        if(args.size==5)
          args(4).toInt
        else
          10

      println("Loading ontology...")
      val manager = OWLManager.createOWLOntologyManager()
      val ontology = manager.loadOntologyFromOntologyDocument(new File(ontologyFile))

      println("Loading observation...")
      val observation = manager.loadOntologyFromOntologyDocument(new File(observationFile))

      println("Loading signatures..")
      val factory = manager.getOWLDataFactory()
      val concepts= Source.fromFile(conceptNamesFile)
        .getLines()
        .map(IRI.create)
        .map(factory.getOWLClass)
      val roles= Source.fromFile(roleNamesFile)
        .getLines()
        .map(IRI.create)
        .map(factory.getOWLObjectProperty)

      val signature = Set[OWLEntity]()++roles++concepts

      val abducibles = ontology.getSignature(Imports.INCLUDED)
        .asScala
        .filterNot(signature)
        .asJava


      println("Performing abduction...")
      val abducer = new OWLAbducer()
      abducer.setBackgroundOntology(ontology)
      abducer.setAbducibles(abducibles)
      val completeHypothesis = abducer.abduce(observation.getAxioms)

      println("Unravelling hypotheses into OWL ontologies...")
      val adapter = new DLStatementAdapter(completeHypothesis,abducer)
      var count = 0
      var done=false
      while(!done && count<hypothesisNumber) {
        val nextHypothesis = adapter.getNextConversion()
        if(nextHypothesis==null)
          done=true
        else {
          val hypOnt = manager.createOntology()
          manager.addAxioms(hypOnt,nextHypothesis)
          manager.saveOntology(hypOnt, new FileOutputStream(new File("hypothesis_"+count+".owl")))
          count+=1
        }
      }
      println("done")

    }

  private def printInfoAndExit() = {
    println("Use as follows: ")
    println("  "+this.getClass.toString+" ONTOLOGY OBSERVATION CONCEPT-NAMES ROLE-NAMES [MAX_HYPOTHESES]")
    println()
    println("Where ONTOLOGY and OBSERVATION are file names of ontologies, and ROLE-NAMES and CONCEPT-NAMES are ")
    println("filenames for signature files, which contain the forbidden role/concept names, one IRI per line.")
    println("The optional parameter sets the maximum number of hypotheses (default is 10).")
    println()
    println("The different hypotheses are then written into files of the form \"hypothesis_i.owl\", where i is")
    println("an integer.")
    System.exit(0)
  }

}
