package uk.ac.man.cs.lethe.internal.dl.logicalDifference

import uk.ac.man.cs.lethe.internal.dl.owlapi.{OWLApiConfiguration, OWLApiConverter}

import java.util.{Date, Set}
import scala.collection.JavaConversions._
import scala.collection.mutable.{HashMap, MultiMap, Set => mutSet}
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.parameters.Imports
import org.semanticweb.owlapi.model.{OWLAxiom, OWLEntity, OWLLogicalAxiom, OWLOntology}
import uk.ac.man.cs.lethe.internal.dl.datatypes.{Axiom, DLStatement, Ontology, OntologyFilter}
import uk.ac.man.cs.lethe.internal.dl.owlapi.OWLApiConverter


/**
 * Wrapper for OWL ontologies using some optimisations to allow for
 * faster access of axioms and its translation into the data structures
 * used by LETHE.
 *
 * Currently, ontologies are by default restricted to ALCH. 
 */
class OptimisedOWLOntology(owlOntology: OWLOntology) { 

  OWLApiConfiguration.SIMPLIFIED_NAMES=false

  private val manager = OWLManager.createOWLOntologyManager

  private var internalOntology = OWLApiConverter.convert(owlOntology)
  
  internalOntology = OntologyFilter.restrictToALCH(internalOntology) // assumption by DirectALCForgetter

  // Build map for OWL axioms
//  println("building up map owl axioms..."+new Date())
  private var symbol2OWLAxiom = new HashMap[OWLEntity, mutSet[OWLAxiom]]() with MultiMap[OWLEntity, OWLAxiom]
  owlOntology.getTBoxAxioms(Imports.INCLUDED).forEach{ ax =>
    ax.getSignature.forEach{ c => symbol2OWLAxiom.addBinding(c, ax) }
  }
//  println("done building up map")
//  println(symbol2OWLAxiom)

  // Build map for internal representation of axioms
//  println("building up map internal format..."+new Date())
  private var concept2Axioms = new HashMap[String, mutSet[Axiom]]() with MultiMap[String, Axiom]
  internalOntology.tbox.axioms.foreach{ ax =>
    ax.atomicConcepts.foreach{ c => concept2Axioms.addBinding(c, ax) }
  }
//  println("done building up map")
//  println("concept2Axioms: "+concept2Axioms.mkString("\n"))


  /**
   * Get the actual OWL ontology object.
   *
   * @return The OWL ontology object used in the wrapper. 
   */
  def getOWLOntology(): OWLOntology = owlOntology
  
  /**
   * Get the internal ontology object (using LETHE data structures.)
   */
  def getInternalOntology(): Ontology = internalOntology

  /**
   * Return the signature of the ontology in OWL format.
   *
   * @return The signature of the ontology in OWL format.
   */
  def getOWLSignature = symbol2OWLAxiom.keySet

  /**
   * Return the concept symbols in the ontology in String format.
   *
   * @return The concept symbols occuring in the ontology, in String format.
   */
  def getConceptSymbols = concept2Axioms.keySet

  /**
   * Return the logical OWL axioms in the ontology.
   * 
   * @return the logical OWL axioms in the ontology.
   */
   def getLogicalOWLAxioms = owlOntology.getLogicalAxioms()

  /**
   * Return the owl axioms using the specified symbol.
   *
   * @param symbol The symbol to be contained in the axioms.
   *
   * @return The OWL axioms in the ontology that contain the specified symbol. 
   */
  def getOWLAxioms(symbol: OWLEntity) = symbol2OWLAxiom.getOrElse(symbol, mutSet[OWLAxiom]())

  /**
   * Return the axiom using the specified concept symbol.
   */
  def getAxiomsForConcept(concept: String) = { 

//    println("Concept: "+concept)

//    println("Concept symbols: "+getConceptSymbols)
//    println(concept2Axioms(concept))

    concept2Axioms.getOrElse(concept, mutSet[Axiom]())

  }

  /**
   * Applies a change to the current ontology.
   *
   * @param change The change to be applied. 
   */
  def applyChange(change: OWLOntologyChange): Unit = {
    change.addAxioms.foreach(addAxiom)
    change.removeAxioms.foreach(removeAxiom)
  }

  /**
   * Add an OWL axiom to the current ontology.
   *
   * @param axiom The OWL axiom to be added.
   */
  def addAxiom(axiom: OWLAxiom): Unit = { 
    manager.addAxiom(owlOntology, axiom)

    axiom.getSignature.foreach(symbol2OWLAxiom.addBinding(_, axiom))

    OWLApiConverter.convert(axiom).foreach{ _ match {
      case ax: Axiom => 
      	internalOntology.addStatement(ax)
        ax.atomicConcepts.foreach(concept2Axioms.addBinding(_, ax))
      case d => ; //println("Warning, optimised ontology currently ignores all non tbox clauses: "+d)
    }}
  }

  /**
   * Remove an OWL axiom from the current ontology.
   */
  def removeAxiom(axiom: OWLAxiom): Unit = {
    manager.removeAxiom(owlOntology, axiom)

    axiom.getSignature.foreach(symbol2OWLAxiom.removeBinding(_, axiom))

    OWLApiConverter.convert(axiom).foreach{ _ match { 
      case ax: Axiom =>
        internalOntology.remove(ax)
        ax.atomicConcepts.foreach(concept2Axioms.removeBinding(_, ax))
      case d => ; //println("Warning, optimised ontology currently ignores all non tbox clauses: "+d)
    }}
  }

  def contains(statement: DLStatement): Boolean = statement match {
    case axiom: Axiom => internalOntology.tbox.axioms(axiom)
    case other => assert(false, "not implemented!"); false
  }

}

/**
 * A change applied to an OWLOntology, consisting of logical axioms
 * being removed from the ontology and logical axioms being added to
 * the ontology.
 *
 * @param removeAxioms Axioms to be removed by this change.
 * @param addAxioms Axioms to be added by this change.
 */
case class OWLOntologyChange(removeAxioms: Set[OWLLogicalAxiom],
			     addAxioms: Set[OWLLogicalAxiom])
