package uk.ac.man.cs.lethe.abduction

import org.semanticweb.HermiT.ReasonerFactory
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.parameters.Imports
import org.semanticweb.owlapi.model.{OWLAxiom, OWLEntity, OWLLogicalAxiom, OWLOntology}
import uk.ac.man.cs.lethe.internal.dl.datatypes.Ontology
import uk.ac.man.cs.lethe.internal.dl.filters.{OWLFamilies, OWLOntologyFilters}
import uk.ac.man.cs.lethe.internal.dl.owlapi.{ModuleExtractor, OWLApiConverter}
import uk.ac.man.cs.lethe.internal.tools.{Cancelable, CanceledException}
import uk.ac.man.cs.lethe.internal.tools.formatting.SimpleDLFormatter

import java.util
import scala.collection.JavaConverters._

class ObservationEntailedException(message: String) extends Exception(message)

class OWLAbducer extends Cancelable {

  val innerAbducer = new FullAbducer();

  var optimizeUsingModules = true

  var ontology: OWLOntology = _
  var abducibles: Set[OWLEntity] = _

  var hypothesisSimplifier: HypothesisSimplifier = _

  private var checkEntailed = true

  private var unsupportedAxiomsInOntology = false
  private var unsupportedAxiomsInHypothesis = false

  /**
   * check whether
   * @return
   */
  def getUnsupportedAxiomsEncountered() =
    unsupportedAxiomsInOntology || unsupportedAxiomsInHypothesis

  def setCheckEntailed(value: Boolean) =
    checkEntailed=value

  def setBackgroundOntology(ontology: OWLOntology) = {

    this.ontology=ontology

    OWLApiConverter.clearIgnoredAxioms()

    val converted = OWLApiConverter.convert(ontology)

    innerAbducer.setBackgroundOntology(converted)


    unsupportedAxiomsInOntology=OWLApiConverter.ignoredAxioms || OWLFamilies.withinFragment(converted,OWLFamilies.ALC)

    //hypothesisSimplifier = new HypothesisSimplifier(ontology)
  }

  def simplify(hypothesis: Set[OWLLogicalAxiom]) = {
    val manager = OWLManager.createOWLOntologyManager()
    val copy = manager.createOntology()
    manager.addAxioms(copy, ontology.getAxioms(Imports.INCLUDED))
    hypothesisSimplifier = new HypothesisSimplifier(copy)
    hypothesisSimplifier.simplify(hypothesis)
  }

  def setAbducibles(abducibles: java.util.Set[OWLEntity]) = {
    this.abducibles = abducibles.asScala.toSet
    val converted = this.abducibles.map(OWLApiConverter.getName).toSet
    innerAbducer.setAbducibles(converted)
  }

  def entailed(observation: util.Set[OWLAxiom]): Boolean = {
    val reasoner = new ReasonerFactory().createReasoner(ontology)
    observation.asScala.forall(reasoner.isEntailed(_))
  }

  @throws(classOf[ObservationEntailedException])
  @throws(classOf[CanceledException])
  def abduce(observation: java.util.Set[OWLAxiom]) = {

    uncancel

    if(checkEntailed) {
      if(entailed(observation))
        throw new ObservationEntailedException("Observation is already entailed!")
    }

    val oldOntology = ontology
    if(optimizeUsingModules){
      val signature = observation.asScala.flatMap(_.getSignature().asScala) ++ abducibles
      ontology = ModuleExtractor.getModuleAsOntology(ontology, signature)
    }

    OWLApiConverter.clearIgnoredAxioms()
    val converted = new Ontology()

    observation.forEach(axiom => converted.addStatements(OWLApiConverter.convert(axiom)))

    unsupportedAxiomsInHypothesis = OWLApiConverter.ignoredAxioms || !OWLFamilies.withinFragment(converted,OWLFamilies.ALC)

    val result = innerAbducer.abduce(converted)

    ontology=oldOntology

    println("My result is: ")
    println(SimpleDLFormatter.format(result))

    result
  }

  def formatAbductionResult(observation: java.util.Set[OWLAxiom]) = {
    SimpleDLFormatter.format(abduce(observation))
  }

  override def cancel(): Unit ={
    innerAbducer.cancel()
  }

  override def uncancel = {
    super.uncancel
    innerAbducer.uncancel
  }

  override def isCanceled: Boolean = {
    innerAbducer.isCanceled
  }

}
