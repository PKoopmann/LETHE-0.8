package uk.ac.man.cs.lethe.internal.dl.logicalDifference

import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.{OWLEntity, OWLLogicalAxiom, OWLOntology}
import uk.ac.man.cs.lethe.internal.dl.owlapi.{ModuleExtractor, OWLApiInterface, OWLExporter}
import uk.ac.man.cs.lethe.interpolation._

import java.util.{Date, Set}
import scala.collection.JavaConversions._


/**
 * Evaluate logical differences caused by small changes to the
 * ontology. The object keeps the current version of the ontology, and computes
 * logical differences caused by small changes to the ontology.
 *
 * The main difference to the class LogicalDifferenceComputer is that instances
 * of IncrementalLogicalDifference keep an updated copy of the current ontology,
 * and evaluate logical differences with respect to small changes to be applied on
 * this ontology, whereas LogicalDifferenceComputer always takes into account two
 * complete ontologies.
 */
trait IIncrementalLogicalDifferences { 
  /**
   * Checks for new and lost entailments after an ontology change has been applied.
   *
   * @param change The change on the current ontology to be evaluated.
   * @return An OWLLogicalDifference object containing lost and new entailments that
   *  would occur if the change would be applied on the current ontology.
   */ 
  def evaluateChange(change: OWLOntologyChange): OWLLogicalDifference

  /**
   * Applies a given change to the current ontology.
   *
   * @param change The change to be applied to the current ontology.
   */
  def applyChange(change: OWLOntologyChange): Unit
}


/**
 * IncrementalLogicalDifferences with updated signatures.
 *
 * Logical differences are always computed with respect to the common signature of the
 * current ontology and the resulting ontology after a change. The interpolator
 * is used for computing the logical difference, and the approximation level
 * is used in case the ontology is or becomes cyclic.
 *
 * @param ontology The ontology for which logical differences are computed. Changes to the
 *   ontology should be performed using the applyChange method of this class.
 * @param interpolator The interpolator used for computing the logical difference.
 * @param approximationLevel Level unto which logical differences are approximated in
 *  case one of the ontologies involved becomes cyclic.
 * 
 */
class IncrementalLogicalDifferences(ontology: OptimisedOWLOntology,
				    interpolator: IOWLInterpolator,
				    approximationLevel: Int) 
extends AbstractIncrementalLogicalDifferences(ontology, 
					      interpolator, 
					      approximationLevel) { 

  def this(ontology: OWLOntology,
           interpolator: IOWLInterpolator,
	   approximationLevel: Int) = this(new OptimisedOWLOntology(ontology), 
	   		       	           interpolator,
					   approximationLevel)

  /**
   * Checks for new and lost entailments after an ontology change has been applied.
   * Considers entailments in the common signature of the current ontology and
   * the ontology resulting from the change.
   *
   * @param change The change on the current ontology to be evaluated.
   * @return An OWLLogicalDifference object containing lost and new entailments that
   *  would occur if the change would be applied on the current ontology.
   */ 
  def evaluateChange(change: OWLOntologyChange): OWLLogicalDifference = { 

    var start = new Date().getTime    
//    val axioms = ontology.getLogicalOWLAxioms.filterNot(asScalaSet(change.removeAxioms))
//    println("filtering axioms took "+(new Date().getTime-start))


    // sigNew: signature of added axioms
    start = new Date().getTime
    val sigNew = OWLApiInterface.getSignature(change.addAxioms)
//    println("getting signature new took "+(new Date().getTime-start))
//    println("sigNew: "+sigNew)

    // sigAxioms: signature of old ontology minus elements that would be removed due to the change
    start = new Date().getTime
    val sigAxioms = ontology.getOWLSignature.filter{ s => 
      sigNew.contains(s) ||
      !change.removeAxioms.containsAll(ontology.getOWLAxioms(s))
						  }

//    println("sigAxioms: "+sigAxioms)
//    println("getting signature took "+(new Date().getTime-start))

    start = new Date().getTime
    val sigCommon = sigNew.filter(sigAxioms)

    evaluateChange(change, sigCommon)
  }

  def evaluateChange(change: OWLOntologyChange, signature: Set[OWLEntity])
      : OWLLogicalDifference = {

//    println("sigCommon: "+sigCommon)
//    println("common signature took "+(new Date().getTime-start))

    var start = new Date().getTime
    val moduleAx = ModuleExtractor.getModule(ontology.getOWLOntology, 
    					     asScalaSet(signature).toSet[OWLEntity])
//    println("Extracting module took "+(new Date().getTime-start))
    start = new Date().getTime
    val module = owlExporter.toOwlOntology(moduleAx)
//    println("Converting module took "+(new Date().getTime-start))


    start = new Date().getTime
    val lost = computeLostEntailments(module, change, signature)
//    println("\nComputing lost entailments took "+(new Date().getTime-start))
    start = new Date().getTime
    val newE = computeNewEntailments(module, change, signature)
//    println("\nComputing new entailments took "+(new Date().getTime-start))
	   

    OWLLogicalDifference(lost, newE)
  }
}

/**
 * IncrementalLogicalDifferences with fixed signature.
 *
 * Logical differences are always computed with respect to a specified ontology. The
 * interpolator
 * is used for computing the logical difference, and the approximation level
 * is used in case the ontology is or becomes cyclic.
 *
 * @param ontology The ontology for which logical differences are computed. Changes to the
 *   ontology should be performed using the applyChange method of this class.
 * @param signature Signature in which logical differences are computed.
 * @param interpolator The interpolator used for computing the logical difference.
 * @param approximationLevel Level unto which logical differences are approximated in
 *  case one of the ontologies involved becomes cyclic.
 */
class IncrementalLogicalDifferencesFixedSignature(ontology: OptimisedOWLOntology,
						  signature: Set[OWLEntity],
						  interpolator: IOWLInterpolator,
						  approximationLevel: Int)
extends AbstractIncrementalLogicalDifferences(ontology, interpolator, approximationLevel) { 
  
  def this(ontology: OWLOntology,
	   signature: Set[OWLEntity],
	   interpolator: IOWLInterpolator,	
	   approximationLevel: Int) = this(new OptimisedOWLOntology(ontology),
	   		              signature,
				      interpolator,
				      approximationLevel)

//  println("Computing uniform interpolant for specified signature")

  val interpolant = interpolator.uniformInterpolant(ontology.getOWLOntology, signature)

  /**
   * Checks for new and lost entailments after an ontology change has been applied.
   * Considers only entailments in the specified signature.
   *
   * @param change The change on the current ontology to be evaluated.
   * @return An OWLLogicalDifference object containing lost and new entailments that
   *  would occur if the change would be applied on the current ontology.
   */ 
  def evaluateChange(change: OWLOntologyChange): OWLLogicalDifference = { 
    
    var changesSignature = OWLApiInterface.getSignature(change.removeAxioms)
    changesSignature ++= OWLApiInterface.getSignature(change.addAxioms)

    val relevantSignature = signature.filter(changesSignature)

    val relevantModuleAx = ModuleExtractor.getModule(interpolant, 
						   asScalaSet(relevantSignature).toSet[OWLEntity])

    val relevantModule = owlExporter.toOwlOntology(relevantModuleAx)

    OWLLogicalDifference(computeLostEntailments(relevantModule, change, signature),
			 computeNewEntailments(relevantModule, change, signature))
  }
}




abstract class AbstractIncrementalLogicalDifferences(ontology: OptimisedOWLOntology,
						     interpolator: IOWLInterpolator,
						     approximationLevel: Int) 
extends IIncrementalLogicalDifferences { 

  protected val manager = OWLManager.createOWLOntologyManager
  
  protected val owlExporter = new OWLExporter()


  private val logicalEntailmentChecker: LogicalEntailmentChecker = 
    LogicalEntailmentCheckerClassical

  protected def computeLostEntailments(module: OWLOntology,
				       change: OWLOntologyChange,
				       signature: Set[OWLEntity]): Set[OWLLogicalAxiom] = { 
    var newModuleAxioms = module.getLogicalAxioms().filterNot(asScalaSet(change.removeAxioms))
    newModuleAxioms ++= change.addAxioms
    val newModuleOnt = owlExporter.toOwlOntology(newModuleAxioms)

    val removedAxiomsOnt = owlExporter.toOwlOntology(change.removeAxioms)
 
    val removedAxiomsInt = interpolator.uniformInterpolant(removedAxiomsOnt,
							   signature,
							   approximationLevel)

    logicalEntailmentChecker.filterEntailed(newModuleOnt, removedAxiomsInt.getLogicalAxioms)
  }

  protected def computeNewEntailments(module: OWLOntology,
				      change: OWLOntologyChange,
				      signature: Set[OWLEntity]): Set[OWLLogicalAxiom] = { 

    val newAxiomOnt = owlExporter.toOwlOntology(change.addAxioms)
    
    val interpolantNew = interpolator.uniformInterpolant(newAxiomOnt,
							 signature,
							 approximationLevel)
    
    logicalEntailmentChecker.filterEntailed(module, interpolantNew.getLogicalAxioms)
  }
 

  /**
   * Applies a change to the current ontology.
   *
   * @param change The change to be applied.
   */
  def applyChange(change: OWLOntologyChange): Unit = {
     ontology.applyChange(change)
  }
}



/**
 * The logical difference between two ontology version, consisting
 * of lost entailments, that are entailed by the first ontology but
 * not by the second, and new entailments, that are entailed by the
 * second but not by the first ontology.
 *
 * @param lostEntailments Entailments that are lost after a change
 *  happened.
 * @param newEntailments New entailments that appear after a change
 *  happened.
 */
case class OWLLogicalDifference(lostEntailments: Set[OWLLogicalAxiom],
				newEntailments: Set[OWLLogicalAxiom])
