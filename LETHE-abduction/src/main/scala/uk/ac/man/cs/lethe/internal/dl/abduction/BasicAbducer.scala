package uk.ac.man.cs.lethe.internal.dl.abduction

import uk.ac.man.cs.lethe.internal.dl.abduction.forgetting.ExtendedABoxForgetter
import uk.ac.man.cs.lethe.internal.dl.datatypes.{Ontology, TBox}
import uk.ac.man.cs.lethe.internal.tools.formatting.SimpleDLFormatter

class BasicAbducer {

  var abducibles: Set[String] = _

  var backgroundOntology = new Ontology()

  def setBackgroundOntology(ontology: Ontology) = {
    this.backgroundOntology=ontology
  }

  def setAbducibles(signature: Set[String]) =
    this.abducibles=signature

  def abduce(observation: Ontology) = {
    val negatedObservation = KBNegator.negate2Assertion(observation)

    val negObservationAsOntology = Ontology.buildFrom(Set(negatedObservation))

    val forgetter = ExtendedABoxForgetter

    val toForget = (backgroundOntology.signature ++ negatedObservation.signature) -- abducibles

    val forgettingResult =
      forgetter.forget(negObservationAsOntology, toForget, backgroundOntology)


    // any TBox results are definitely not needed for the forgetting result, and are the result of pulling definers,
    // which sometimes might pull more than necessary. We can thus safely delete the TBox from the forgetting result
    forgettingResult.tbox=new TBox(Set())

    val hypothesis = KBNegator.negate(forgettingResult)

    hypothesis
  }
}
