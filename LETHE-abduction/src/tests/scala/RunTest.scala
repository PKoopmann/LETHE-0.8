//package tests
//
//import java.io.File
//
//import uk.ac.man.cs.lethe.internal.dl.abduction.filtering.FullAbducer
//import org.semanticweb.owlapi.apibinding.OWLManager
//import org.semanticweb.owlapi.model.{IRI, OWLDataFactory}
//import uk.ac.man.cs.lethe.internal.dl.abduction.KBNegator
//import uk.ac.man.cs.lethe.internal.dl.abduction.forgetting.ExtendedABoxForgetter
//import uk.ac.man.cs.lethe.internal.dl.datatypes.{DLStatement, Ontology}
//import uk.ac.man.cs.lethe.internal.dl.filters.OWLOntologyFilters
//import uk.ac.man.cs.lethe.internal.dl.forgetting.direct.ALCFormulaPreparations
//import uk.ac.man.cs.lethe.internal.dl.owlapi.OWLApiConverter
//
//import scala.util.Random
//
//object RunTest {
//
//  def main(args: Array[String]): Unit = {
//
//    ALCFormulaPreparations.reuseDefiners = true
//
//    val man = OWLManager.createOWLOntologyManager()
//    val inputOntology = man.loadOntologyFromOntologyDocument(new File(args(0)))
//    val inputObservation = man.loadOntologyFromOntologyDocument(new File(args(1)))
//    val inputForgettingSymbols = args(2).split(",")
//
//    var background = OWLApiConverter.convert(inputOntology)
//    var observation = OWLApiConverter.convert(inputObservation)
//
//    val df = OWLManager.createOWLOntologyManager().getOWLDataFactory
//    var IRIString = inputOntology.getOntologyID.toString()
//
//    var nonAbducibles = scala.collection.mutable.Set[String]()
//    for(symbol <- inputForgettingSymbols) {
//      nonAbducibles.add(symbol.toString())
//    }
//    println("Input ont: " + background)
//    println("Input obs: " + observation)
//
//
//    val abducibles = (background.signature ++ observation.signature) -- nonAbducibles
//    println("non-abducibles: " + nonAbducibles)
//
//    val abducer = new FullAbducer()
//    abducer.setBackgroundOntology(background)
//    abducer.setAbducibles(abducibles)
//
//    //Forgetting
//    val forgettingResultBeforeSimplification = abducer.forget(observation)
//    val hypothesisBeforeSimplification = KBNegator.negate(forgettingResultBeforeSimplification)
//
//    //Simplification
//    val forgettingResultAfterSimplification = abducer.simplify(forgettingResultBeforeSimplification)
//    val hypothesisAfterSimplification = KBNegator.negate(forgettingResultAfterSimplification)
//
//    //Filtering
//    val hypothesisAfterFiltering = abducer.filter(forgettingResultAfterSimplification)
//
//    println("Forgetting solution: " + forgettingResultBeforeSimplification)
//    println("Observation: " + observation)
//    println("Hypothesis pre-filtering: " + hypothesisBeforeSimplification)
//    println("Hypothesis post-filtering: " + hypothesisAfterFiltering)
//  }
//}
