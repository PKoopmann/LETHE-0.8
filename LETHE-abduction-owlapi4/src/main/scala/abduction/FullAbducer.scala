package uk.ac.man.cs.lethe.abduction

import com.typesafe.scalalogging.Logger
import uk.ac.man.cs.lethe.internal.dl.abduction.KBNegator
import uk.ac.man.cs.lethe.internal.dl.abduction.filtering.{AbductionSimplifier, StrongRedundancyChecker}
import uk.ac.man.cs.lethe.internal.dl.abduction.forgetting.ExtendedABoxForgetter
import uk.ac.man.cs.lethe.internal.dl.datatypes.extended.DisjunctiveAssertion
import uk.ac.man.cs.lethe.internal.dl.datatypes.{Assertion, ConceptAssertion, DLStatement, Ontology, TBox}
import uk.ac.man.cs.lethe.internal.dl.proofs.InferenceLogger
import uk.ac.man.cs.lethe.internal.tools.{Cancelable, CanceledException}
import uk.ac.man.cs.lethe.internal.tools.formatting.SimpleDLFormatter

import scala.collection.JavaConverters
import scala.concurrent.CancellationException

class FullAbducer extends Cancelable {

  val logger = Logger[FullAbducer]

  var abducibles: Set[String] = _

  var backgroundOntology = new Ontology()

  val forgetter = ExtendedABoxForgetter

  def setBackgroundOntology(ontology: Ontology) = {
    this.backgroundOntology=ontology
  }

  def setAbducibles(signature: Set[String]) =
    this.abducibles=signature

  @throws(classOf[CanceledException])
  def abduce(observation: Ontology): DLStatement = {

    KBNegator.uncancel

    val forgettingResult = forget(observation)

    println("Negating result...")

    val result = KBNegator.negate(forgettingResult)

    println("... negating done.")

    result
    //filter(simplify(forget(observation)))
  }

  def forget(observation: Ontology) = {

    forgetter.uncancel

    val negatedObservation = KBNegator.negate2Assertion(observation)

    val negObservationAsOntology = Ontology.buildFrom(Set(negatedObservation))

    logger.info("Forgetting...")

    val toForget = (backgroundOntology.signature ++ negatedObservation.signature) -- abducibles

    val forgettingResultBeforeFiltering =
      forgetter.forget(negObservationAsOntology, toForget, backgroundOntology)

    println("Forgetting result before filtering:")
    println(SimpleDLFormatter.format(forgettingResultBeforeFiltering))

    //println("Forgetting result:")
    //println(SimpleDLFormatter.format(forgettingResultBeforeFiltering))

    // any TBox results are definitely not needed for the hypotheses - and are the result of pulling definers,
    // which sometimes might pull more than necessary. We can safely delete the TBox from the forgetting result
    forgettingResultBeforeFiltering.tbox=new TBox(Set())

    //val hypothesisBeforeFiltering = KBNegator.negate(forgettingResultBeforeFiltering)

    forgettingResultBeforeFiltering
  }

  private def simplify(forgettingResult:Ontology): Ontology = {
    println("Before simplifying: "+forgettingResult)
    logger.info("Simplifying...")
    //perform simplification steps
    val forgettingResultConjuncts = forgettingResult.abox.assertions;

    val simplifier = new AbductionSimplifier();
    val simplifierOutput = simplifier.simplify(forgettingResultConjuncts);
    val simplifiedForgettingResult = Ontology.buildFrom(JavaConverters.collectionAsScalaIterable(simplifierOutput));

    simplifiedForgettingResult
  }

  private def filter(simplifiedForgettingResult:Ontology) = {
    logger.info("Filtering...")
    val redundancyChecker = new StrongRedundancyChecker();
    val reducedForgettingResult = redundancyChecker.checkRedundancy(backgroundOntology, simplifiedForgettingResult)

    val hypothesis = KBNegator.negate(reducedForgettingResult)

    hypothesis
  }

  override def cancel(): Unit ={
    KBNegator.cancel
    forgetter.cancel
  }

  override def uncancel: Unit = {
    super.uncancel
    KBNegator.uncancel
    forgetter.uncancel
  }

  override def isCanceled: Boolean = {
    forgetter.isCanceled
  }

}
