package uk.ac.man.cs.lethe

import java.util.Set
import java.util.stream.Collectors

import scala.collection.JavaConverters._
import org.semanticweb.owlapi.model._
import uk.ac.man.cs.lethe.internal.dl.datatypes._
import uk.ac.man.cs.lethe.internal.dl.forgetting.ConceptAndRoleForgetter
import uk.ac.man.cs.lethe.internal.dl.forgetting.DirectALCForgetter
import uk.ac.man.cs.lethe.internal.dl.forgetting.SHQForgetter
import uk.ac.man.cs.lethe.internal.dl.forgetting.direct.FixpointApproximator
import uk.ac.man.cs.lethe.internal.dl.interpolation.OntologyInterpolator
import uk.ac.man.cs.lethe.internal.dl.owlapi.OWLApiConfiguration
import uk.ac.man.cs.lethe.internal.dl.owlapi.{OWLApiConverter, OWLExporter}
import uk.ac.man.cs.lethe.internal.forgetting.Forgetter
import uk.ac.man.cs.lethe.internal.tools.{MockProgressBar, ProgressBarAttached, Timeoutable}


package interpolation {

  import uk.ac.man.cs.lethe.internal.dl.HermitMappedReasonerFactory
  import uk.ac.man.cs.lethe.internal.dl.forgetting.DirectALCForgetterRoles
  import uk.ac.man.cs.lethe.internal.dl.forgetting.abox.ABoxForgetter
  import uk.ac.man.cs.lethe.internal.tools.{Cancelable, Timeoutable}

  import scala.collection.convert.ImplicitConversions.`collection AsScalaIterable`

  /**
   * Interface for classes computing uniform interpolants of OWL ontologies.
   */
  trait IOWLInterpolator {
    /**
     * Compute a uniform interpolant of the given ontology over the signature.
     * If the uniform interpolant would contain fixpoint expressions, simulate
     * them using helper concepts.
     *
     * If the input ontology contains cycles, the result might contain
     * auxiliary concepts whose name starts with the name "_D". These are
     * used to represent cyclic relations in the uniform interpolant which
     * could otherwise only be represented using greatest fixpoint operators.
     *
     * @param owlOntology The ontology to be interpolated.
     * @param signature The signature into which the ontology is to be
     * interpolated, i.e. the set of OWL entities the uniform interpolant is
     * allowed to use.
     * @return The uniform interpolant of the owlOntology for signature.
     * An ontology that uses only symbols in the signature and possibly
     * auxiliary concepts, and preserves all entailments of owlOntology
     * that are in the specified signature.
     */
    def uniformInterpolant(owlOntology: OWLOntology,  
    			   signature: Set[OWLEntity]): OWLOntology

  
    /**
     * Compute a uniform interpolant of the given ontology over the signature.
     * If the uniform interpolant would contain fixpoints, approximate the
     * fixpoint expressions up to the specified approximation level. The resulting  
     * ontology will be completely in the desired signature and not use auxiliary
     * concepts.
     * 
     * @param owlOntology The ontology to be interpolated.
     * @param signature The signature into which the ontology is to be
     * interpolated, i.e. the set of OWL entities the uniform interpolant is
     * allowed to use.
     * @param approximationLevel Specifies the level up to which to approximate.
     * Higher values may lead to deeper nested role restrictions, if the
     * input ontology uses cycles, but preserve more entailments of the
     * original ontology.
     * @return An approximation of the uniform interpolant of the
     * owlOntology for signature.
     * An ontology that uses only symbols in the signature and preserves
     * entailments of owlOntology up to a certain nesting.
     */
    def uniformInterpolant(owlOntology: OWLOntology, 
    	  		   signature: Set[OWLEntity], 
			   approximationLevel: Int): OWLOntology 

    val forgetter: Forgetter[Ontology, String] with ProgressBarAttached
  }

  trait ITimeoutableOWLInterpolator extends IOWLInterpolator with Timeoutable

  /**
   * Interpolator for ALCH TBoxes. Supports elimination of concept and role symbols. Axioms which are not
   * in ALCH, as well as ABox axioms, are removed before the computation of the uniform interpolant.
   */
  class AlchTBoxInterpolator extends AbstractTimeoutableOWLInterpolator
  // note: timeout information is sent by abstract class
  { override val forgetter = ConceptAndRoleForgetter

    ConceptAndRoleForgetter.setReasonerFactory(HermitMappedReasonerFactory)
  }

  /**
   * Interpolator for ALCH TBoxes. Supports only elimination of concept symbols. This is the only interpolator
   * class that is currently supported for use with an OWLAbducer.
   */
  class AlchTBoxInterpolatorC extends AbstractTimeoutableOWLInterpolator
  { override val forgetter = DirectALCForgetter }

  /**
   * Interpolator for SH knowledge bases. Supports elimination of concept and non-transitive role symbols.
   * Also applies to ABoxes. Axioms which are not in SH are removed before the computation of the uniform
   * interpolant.
   */  
  class ShKnowledgeBaseInterpolator extends AbstractTimeoutableOWLInterpolator
    // note: timeout information is sent by abstract class
  {
    override val forgetter = ABoxForgetter

    ABoxForgetter.setReasonerFactory(HermitMappedReasonerFactory)
  }

  /**
   * Interpolator for SHQ TBoxes. Supports only elimination of role symbols. Axioms which are not in SHQ, as
   * well as ABox axioms, are removed before the computation of the uniform interpolant.
   *
   * This class does not support time outs yet.
   */
  class ShqTBoxInterpolator extends AbstractOWLInterpolator { override val forgetter = SHQForgetter }

  abstract class AbstractTimeoutableOWLInterpolator extends ITimeoutableOWLInterpolator {

    override val forgetter: Forgetter[Ontology, String] with ProgressBarAttached with Timeoutable

    var useUniversalRoles = false

    def useUniversalRoles(useUniversalRoles: Boolean): Unit =
      this.useUniversalRoles=useUniversalRoles

    override def uniformInterpolant(owlOntology: OWLOntology, symbols: Set[OWLEntity]): OWLOntology = {
      OWLApiConfiguration.SIMPLIFIED_NAMES=false
      val interpolator = new OntologyInterpolator(forgetter,universalRoles = useUniversalRoles)

      startTiming
      transferCancelInformation(forgetter) // also transfers timeout information, if supported by forgetter
      transferCancelInformation(interpolator)

      val scalaSet = asScalaSet(symbols).toSet[OWLEntity]

      val result = interpolator.uniformInterpolant(owlOntology, scalaSet)
      detachCancelInformation(forgetter)
      detachCancelInformation(interpolator)
      result
    }

    override def uniformInterpolant(owlOntology: OWLOntology,
                                    symbols: Set[OWLEntity],
                                    approximateLevel: Int): OWLOntology = {
      OWLApiConfiguration.SIMPLIFIED_NAMES=false
      val interpolator = new OntologyInterpolator(forgetter)

      startTiming
      transferCancelInformation(forgetter) // also transfers timeout information, if supported by forgetter
      transferCancelInformation(interpolator)

      val scalaSet = asScalaSet(symbols).toSet[OWLEntity]

      val result = interpolator.uniformInterpolant(owlOntology, scalaSet, approximateLevel)
      detachCancelInformation(forgetter)
      detachCancelInformation(interpolator)
      result
    }
  }

  abstract class AbstractOWLInterpolator extends IOWLInterpolator { 
    override def uniformInterpolant(owlOntology: OWLOntology, symbols: Set[OWLEntity]): OWLOntology = { 
      OWLApiConfiguration.SIMPLIFIED_NAMES=false
      val interpolator = new OntologyInterpolator(forgetter)

      
      val scalaSet = asScalaSet(symbols).toSet[OWLEntity]

      interpolator.uniformInterpolant(owlOntology, scalaSet)
    }

    override def uniformInterpolant(owlOntology: OWLOntology, 
				    symbols: Set[OWLEntity],
				    approximateLevel: Int): OWLOntology = { 
      OWLApiConfiguration.SIMPLIFIED_NAMES=false
      val interpolator = new OntologyInterpolator(forgetter)

      val scalaSet = asScalaSet(symbols).toSet[OWLEntity]

      interpolator.uniformInterpolant(owlOntology, scalaSet, approximateLevel)
    }
  }

  /**
   * Interface for classes computing uniform interpolants with background
   * knowledge.
   */
  trait IOWLInterpolatorWithBackground { 
    /**
     * Set the signature for which uniform interpolants are computed.
     *
     * @param signature The set of symbols allowed in the uniform interpolants.
     */
    def setSignature(signature: Set[OWLEntity]): Unit

    /**
     * Compute a uniform interpolant with background knowledge of the given set of
     * axioms. The uniform interpolant has the same entailments in the specified signature
     * as the given set of axioms, if added to the background ontology.
     *
     * @param owlAxioms The axioms for which the uniform interpolant is to be computed.
     *
     * @return The uniform interpolant of the given axioms in the signature set to this
     *  interpolator object, taking into account the background knowledge.
     */
    def interpolate(owlAxioms: Set[_ <: OWLAxiom]): Set[OWLLogicalAxiom]

    def interpolate(owlAxioms: Set[_ <: OWLAxiom], approximationLevel: Int): Set[OWLLogicalAxiom]
  }
  
  /**
   * Uniform interpolator with background knowledge for the description logic ALCH.
   * Currently only supports elimination of concept symbols. Axioms which are not in ALCH are
   * ignored.
   *
   * @param owlOntology The ontology of background knowledge to be used.
   */
  class ALCHInterpolatorWithBackground(owlOntology: OWLOntology) extends IOWLInterpolatorWithBackground { 


    private val internalForgetter = DirectALCForgetter
    
    internalForgetter.progressBar = MockProgressBar
    
    private val internalOntology = OWLApiConverter.convert(owlOntology)
    
    private var signature = owlOntology.getSignature()
    
    def setSignature(signature: Set[OWLEntity]) =
      this.signature = signature

    val exporter = new OWLExporter()

    private def interpolateInner(owlAxioms: Set[_ <: OWLAxiom]) = {
      val owlAxiomsScala = asScalaSet(owlAxioms).toSet
      val axioms = owlAxiomsScala.flatMap(OWLApiConverter.convert)
      
      val sig = asScalaSet(signature).toSet
      val toForget = (owlOntology.getSignature().iterator().asScala).toSet ++
        owlAxiomsScala.flatMap(s => s.getSignature().iterator().asScala.toSet) -- sig
      
      val interpolant = internalForgetter.forget(Ontology.buildFrom(axioms), 
						 toForget.map(OWLApiConverter.getName),
						 internalOntology)


      var faulty1 = interpolant.signature -- sig.map(OWLApiConverter.getName)
      faulty1 = faulty1.filterNot(_.toString.startsWith("_D"))

      println("Problem is "+faulty1)

      assert(faulty1.isEmpty)

      OntologyBeautifier.makeNice(interpolant)
      
      interpolant
    }


    override def interpolate(owlAxioms: Set[_ <: OWLAxiom])
    : Set[OWLLogicalAxiom] = { 
      
      val interpolant = interpolateInner(owlAxioms)

      var result = interpolant.tbox.axioms.map(exporter.toOwl(owlOntology, _))
      result ++= interpolant.rbox.axioms.map(exporter.toOwl(owlOntology, _))

      setAsJavaSet[OWLLogicalAxiom](result)
    }
    

    override def interpolate(owlAxioms: Set[_ <: OWLAxiom], approximationLevel: Int)
    : Set[OWLLogicalAxiom] = {
      val interpolant = interpolateInner(owlAxioms)

      var faulty1 = interpolant.signature --asScalaSet(signature).map(OWLApiConverter.getName)

      faulty1 = faulty1.filterNot(_.toString.startsWith("_D"))

      println("Problem is "+faulty1)

      assert(faulty1.isEmpty)


      val approx = FixpointApproximator.approximate(interpolant, approximationLevel)


      val faulty2 = approx.statements.filter{ ax =>
        asScalaSet(signature).exists(s => !ax.signature(s.toString))}




      var result = approx.tbox.axioms.map(exporter.toOwl(owlOntology, _))
      result ++= approx.rbox.axioms.map(exporter.toOwl(owlOntology, _))


      setAsJavaSet[OWLLogicalAxiom](result)

    }

  }

} // package interpolation

package forgetting {

  import uk.ac.man.cs.lethe.internal.dl.HermitMappedReasonerFactory
  import uk.ac.man.cs.lethe.internal.dl.forgetting.abox.ABoxForgetter

  /** 
   * Interface for classes for forgetting or eliminating symbols from
   * OWL ontologies. 
   */
  trait IOWLForgetter {
    /**
     * Forget a specified set of symbols from the ontology.
     * If the result would contain fixpoint expressions, simulate them using
     * auxiliary concepts.
     * 
     * If the input ontology contains cycles, the result might contain
     * auxiliary concepts whose name starts with the name "_D". These are
     * used to represent cyclic relations in the uniform interpolant which
     * could otherwise only be represented using greatest fixpoint operators.
     *
     * @param owlOntology The OWL ontology from which the symbols are to be 
     *  eliminated.
     * @param symbols The symbols to eliminate.
     * @return A new OWL ontology in which the symbols are eliminated. This 
     *  ontology preserves all entailments of the original ontology that make
     *  not use of the eliminated symbols. It is possible that this ontology
     *  contains auxiliary concepts to ensure a finite representation without
     *  fixpoint operators.
     */
    def forget(owlOntology: OWLOntology, 
	       symbols: Set[OWLEntity]): OWLOntology 

    /**
     * Forget the specified set of symbols from the ontology.
     * If the result would contain fixpoint expressions, approximate them
     * up to the specified approximation level. The resulting ontology does
     * not use auxiliary concepts, but may only be an approximation of the 
     * forgetting result.
     *
     * @param owlOntology The OWL ontology from which the symbols are to be 
     *  eliminated.
     * @param symbols The symbols to eliminate.
     * @param approximationLevel Specifies the level up to which to approximate.
     *  Higher values may lead to deeper nested role restrictions, if the
     *  input ontology uses cycles, but preserve more entailments of the
     *  original ontology.
     * @return A new OWL ontology in which the symbols are eliminated. This 
     *  ontology preserves entailments of the original ontology that make
     *  not use of the eliminated symbols. If the input contains cycles, it is
     *  possible that this ontology is only an approximation of the forgetting 
     *  result. 
     */
    def forget(owlOntology: OWLOntology, 
  	       symbols: Set[OWLEntity],
	       approximationLevel: Int): OWLOntology 

    def internalForgetter: Forgetter[Ontology, String]
  }

  trait ITimeoutableOWLForgetter extends IOWLForgetter with Timeoutable

  /**
   * Forgetter for ALCH TBoxes. Supports forgetting of concept and role symbols. Axioms which are not
   * in ALCH, as well as any ABox axioms, are removed from the ontology before forgetting is applied.
   */
  class AlchTBoxForgetter extends AbstractTimeoutableOWLForgetter {
    override val forgetter = ConceptAndRoleForgetter
    ConceptAndRoleForgetter.setReasonerFactory(HermitMappedReasonerFactory)
    forgetter.deactivateProgressBar
  }

  /**
   * Forgetter for SH knowledge bases. Supports forgetting of concept and role
   * symbols. Axioms which are not in SH are removed from the ontology before forgetting is applied.
   */
  class ShKnowledgeBaseForgetter extends AbstractTimeoutableOWLForgetter {
    override val forgetter =  ABoxForgetter
    ABoxForgetter.setReasonerFactory(HermitMappedReasonerFactory)
    forgetter.deactivateProgressBar
  }

  /**
   * Forgetter for SHQ TBoxes. Supports only forgetting of concept symbols. Axioms which are not in
   * SHQ are removed from the ontology before forgetting is applied.
   */
  class ShqTBoxForgetter extends AbstractOWLForgetter { 
    override val forgetter = SHQForgetter 
    forgetter.deactivateProgressBar
  }

  abstract class AbstractTimeoutableOWLForgetter extends AbstractOWLForgetter with ITimeoutableOWLForgetter {

    override val forgetter: Forgetter[Ontology, String] with ProgressBarAttached with Timeoutable

    override def forget(owlOntology: OWLOntology, symbols: Set[OWLEntity]): OWLOntology = {
      startTiming

      transferCancelInformation(forgetter) // also takes care of timeouts, if supported

      val result = super.forget(owlOntology,symbols)

      // in case the same forgetter is used with another instance of this class,
      // we don't want to use the cancel information
      detachCancelInformation(forgetter)
      result
    }

    override def forget(owlOntology: OWLOntology,
                        symbols: Set[OWLEntity],
                       approximationLevel: Int): OWLOntology = {
      startTiming
      transferCancelInformation(forgetter) // also takes care of timeouts, if supported
      val result = super.forget(owlOntology,symbols,approximationLevel)
      detachCancelInformation(forgetter)
      result
    }
  }

  abstract class AbstractOWLForgetter extends IOWLForgetter { 
    protected val forgetter: Forgetter[Ontology, String] with ProgressBarAttached

    override def internalForgetter = forgetter


    override def forget(owlOntology: OWLOntology, symbols: Set[OWLEntity]): OWLOntology = { 
      OWLApiConfiguration.SIMPLIFIED_NAMES=false


      val scalaSet = asScalaSet(symbols).toSet[OWLEntity]
      val ontology = OWLApiConverter.convert(owlOntology)
      val result = forgetter.forget(ontology, scalaSet.map(OWLApiConverter.getName))
    
      val exporter = new OWLExporter()


      exporter.toOwlOntology(result)
    }

    override def forget(owlOntology: OWLOntology, 
 		        symbols: Set[OWLEntity],
		        approximationLevel: Int): OWLOntology = { 
      OWLApiConfiguration.SIMPLIFIED_NAMES=false

      val scalaSet = asScalaSet(symbols).toSet[OWLEntity]
      val ontology = OWLApiConverter.convert(owlOntology)
      var result = forgetter.forget(ontology, scalaSet.map(OWLApiConverter.getName))

      result = FixpointApproximator.approximate(result, approximationLevel)
 
      val exporter = new OWLExporter()


      exporter.toOwlOntology(result)
    }
  }

} // package forgetting

package beautification {

  /**
   * Reformulate a possibly awkward axiom into a nicer form
   */
  class OWLAxiomBeautifier {
    val exporter = new OWLExporter()
    def beautify(axiom: OWLAxiom, ontology: OWLOntology) = {

      val (cExp, oExp) = (CheapSimplifier.expensive, OntologyBeautifier.expensive)

      CheapSimplifier.expensive=true
      OntologyBeautifier.expensive=true
      val result = OWLApiConverter.convert(axiom)
        .map(OntologyBeautifier.nice(_))
        .map(_ match {
          case ax: Axiom => exporter.toOwl(ontology,ax)
          case as: Assertion => exporter.toOwl(ontology,as)
          case ra: RoleAxiom => exporter.toOwl(ontology,ra)
        }).asJava

      CheapSimplifier.expensive = cExp
      OntologyBeautifier.expensive = oExp

      result
    }
  }
}

