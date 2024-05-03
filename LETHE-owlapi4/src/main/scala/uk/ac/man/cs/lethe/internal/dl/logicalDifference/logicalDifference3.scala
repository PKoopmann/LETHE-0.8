package uk.ac.man.cs.lethe.internal.dl.logicalDifference

import org.semanticweb.owlapi.model._
import uk.ac.man.cs.lethe.internal.dl.owlapi.OWLApiInterface
import uk.ac.man.cs.lethe.internal.tools.formatting.SimpleOWLFormatter
import uk.ac.man.cs.lethe.interpolation._

import java.util.Set
import scala.collection.JavaConversions._


/**
 * Class for computing the logical difference between two ontologies.
 * 
 * @param interpolator The Interpolator to be used for computing the logical difference
 */
class LogicalDifferenceComputer2 { 

  /**
   * Computes a set of axioms in the common signature of both ontologies,
   * such that each axiom is entailed by the second ontology, but not of
   * the first. In case the second ontology is cyclic, an approximation
   * level has to be specified. 
   * 
   * @param ontology1 The first ontology.
   * @param ontology2 The second ontology.
   * @param approximationLevel In case the second ontology is cyclic, 
   *  entailments might have to approximated. Higher values allow 
   *  for deeper nested axioms to be checked.
   *
   * @return The logical difference, that is, a set of axioms in the
   *  in the common signature that are entailed by the second ontology,
   *  but not by the first.
   */
  def logicalDifference(ontology1: OWLOntology, 
			ontology2: OWLOntology,
		        approximationLevel: Int)
  : Set[OWLLogicalAxiom] = { 

    val syntDiff = SyntacticalDifferenceComputer.syntacticalDifference(ontology1, ontology2)

    val sig1 = OWLApiInterface.getSignature(ontology1)
    val sig2 = OWLApiInterface.getSignature(syntDiff)
    val commonSignature = sig1.filter(sig2)

    logicalDifference(ontology1, syntDiff, commonSignature, approximationLevel)
  }

  /**
   * Computes a set of axioms in a given signature, such that
   * each axiom is entailed by the second ontology, but not of 
   * the first.
   * 
   * @param ontology1 The first ontology.
   * @param ontology2 The second ontology.
   * @param signature the signature for which the logical difference is to
   *  be computed. 
   * @param approximationLevel In case the second ontology is cyclic, 
   *  entailments might have to approximated. Higher values allow 
   *  for deeper nested axioms to be checked.
   *
   * @return A set of axioms in the specified signature that is entailed
   *   by the second ontology, but not by the first.
   */
  def logicalDifference(ontology1: OWLOntology, 
			ontology2: OWLOntology, 
			signature: Set[OWLEntity],
			approximationLevel: Int)
  : Set[OWLLogicalAxiom] = { 

    val syntDiff = SyntacticalDifferenceComputer.syntacticalDifference(ontology1, ontology2)


    val logicalEntailmentChecker: LogicalEntailmentChecker = 
      LogicalEntailmentCheckerClassical

    val interpolator = new ALCHInterpolatorWithBackground(ontology1)

    interpolator.setSignature(signature)

    println("interpolating")

    val interpolant: Set[OWLLogicalAxiom] = interpolator.interpolate(syntDiff.getAxioms, approximationLevel)

    println("Relative Interpolant: \n"+interpolant.map(SimpleOWLFormatter.format).mkString("\n"))

    println("Now checking entailments!")

    val result = logicalEntailmentChecker.filterNotEntailed(ontology1, interpolant)

    result
  }

}
