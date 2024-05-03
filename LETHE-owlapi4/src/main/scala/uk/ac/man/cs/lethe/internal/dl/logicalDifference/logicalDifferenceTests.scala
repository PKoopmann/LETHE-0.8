package uk.ac.man.cs.lethe.internal.dl.logicalDifference

import uk.ac.man.cs.lethe.internal.dl.owlapi.OWLApiInterface

import java.io.File
import java.net.URL
import java.util.{Date, HashSet, Set}
import scala.collection.JavaConversions._
import scala.collection.mutable.HashMap
import scala.util.Random
import org.semanticweb.owlapi.model.{OWLClass, OWLLogicalAxiom, OWLOntology}
import uk.ac.man.cs.lethe.interpolation._


/**
 * Test performance of incremental logical difference changes
 */

object LogicalDifferenceTester { 

  val MIN_NUMBER_TO_REMOVE = 10

  var axiomsInOnt: Set[OWLLogicalAxiom] = new HashSet[OWLLogicalAxiom]()
  var removedAxioms: Set[OWLLogicalAxiom] = new HashSet[OWLLogicalAxiom]()

  var classesInOnt: Set[OWLClass] = new HashSet[OWLClass]()

  def main(args: Array[String]) = { 
    println("Loading ontology...")
    println(new Date())
    val ontology = OWLApiInterface.getOWLOntology(new File(args(0)))
    println(new Date())
    println("Loaded")
    axiomsInOnt = ontology.getLogicalAxioms
    classesInOnt = ontology.getClassesInSignature

    val interpolator = new AlchTBoxInterpolator()

    val logicalDifferences = new IncrementalLogicalDifferences(ontology,
							       interpolator,
							       10)
    println("Initialised interpolator")

    while(true){ 

      println("Selecting axioms to add")
      val toAdd = if(removedAxioms.isEmpty)
	new HashSet[OWLLogicalAxiom]()
      else
	new HashSet(Random.shuffle(removedAxioms.toSeq).take(6))



      println("Selecting axioms to remove")
      println(new Date())
      val axioms = selectNextAxioms(MIN_NUMBER_TO_REMOVE)
      println(new Date())

      val change = OWLOntologyChange(axioms, toAdd)
      println("T: Removing "+change.removeAxioms.size+", adding "+change.addAxioms.size)
      val start = new Date().getTime

      println("Computing diff")
      val diff = logicalDifferences.evaluateChange(change)

      println()
      println()
      println("T: Computation took "+(new Date().getTime-start)+" ms.")
      println("T: "+diff.lostEntailments.size+" lost entailments and "+diff.newEntailments.size+" new entailments")

      println("Applying change")
      logicalDifferences.applyChange(change)

      axiomsInOnt --= axioms
      removedAxioms ++= axioms

    }
  }

  def selectNextAxioms(minNumber: Int): Set[OWLLogicalAxiom] = { 

    var result = new HashSet[OWLLogicalAxiom]()

    while(result.size<minNumber){ 
      val nextClass = classesInOnt.head
      classesInOnt -= nextClass

      result ++= getAxiomsWith(nextClass)
    }

    return result
  }

  def getAxiomsWith(owlClass: OWLClass) = { 
    asScalaSet(axiomsInOnt).collect { 
      case ax: OWLLogicalAxiom if ax.getClassesInSignature.contains(owlClass) => ax
    }
  }
}
