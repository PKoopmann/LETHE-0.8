//package tests
//
//import java.io.File
//
//import uk.ac.man.cs.lethe.internal.dl.abduction.filtering.FullAbducer
//import org.semanticweb.owlapi.apibinding.OWLManager
//import org.semanticweb.owlapi.model.{OWLDataPropertyAssertionAxiom, OWLEquivalentClassesAxiom}
//import uk.ac.man.cs.lethe.internal.dl.abduction.{BasicAbducer, KBNegator}
//import uk.ac.man.cs.lethe.internal.dl.abduction.forgetting._
//import uk.ac.man.cs.lethe.internal.dl.datatypes.{Assertion, Axiom, DLStatement, InverseRole, NominalSet, Ontology}
//import uk.ac.man.cs.lethe.internal.dl.filters.OWLOntologyFilters
//import uk.ac.man.cs.lethe.internal.dl.forgetting.direct.ALCFormulaPreparations
//import uk.ac.man.cs.lethe.internal.dl.owlapi.{ModuleExtractor, OWLApiConverter}
//import uk.ac.man.cs.lethe.internal.tools.formatting.SimpleDLFormatter
//
//import scala.collection.JavaConverters._
//import scala.util.Random
//
//object AbductionSmokeTestFull {
//
//  def main(args: Array[String]) = {
//    if(args.size==0){
//      println("Usage: ")
//      println("java "+this.getClass()+" ONTOLOGY-FILE")
//      System.exit(0)
//    }
//
//    ALCFormulaPreparations.reuseDefiners=true
//
//    var ontology = OWLManager.createOWLOntologyManager().loadOntologyFromOntologyDocument(new File(args(0)))
//
//    OWLOntologyFilters.restrictToALC(ontology)
//
//    ExtendedABoxForgetter.deactivateProgressBar
//
//    val equivalentClasses = ontology.getLogicalAxioms().asScala.filter(_.isInstanceOf[OWLEquivalentClassesAxiom])
//
//    equivalentClasses.foreach{ ax =>
//      ontology.removeAxiom(ax)
//      ontology.addAxioms(ax.asInstanceOf[OWLEquivalentClassesAxiom].asOWLSubClassOfAxioms())
//    }
//
//    if(ontology.getAxiomCount()>50*1000) {
//      println("Too many axioms: "+ontology.getAxiomCount())
//      System.exit(0)
//    }
//
//    val module = ModuleExtractor.randomGenuineModule(ontology)
//
//    if(module.size<10) {
//      println("Module too small!")
//      System.exit(0)
//    }
//
//    val myModule = Ontology.buildFrom(OWLApiConverter.convert(module))
//
//    val (background, observation, abducibles) = extractObservation(myModule)
//
//    val abducer = new FullAbducer()
//    abducer.setBackgroundOntology(background)
//    abducer.setAbducibles(abducibles.toSet)
//
//    println("Observation:")
//    println(SimpleDLFormatter.format(observation))
//
//    //Forgetting
//    val forgetStart = System.nanoTime()
//    val forgettingResultBeforeSimplification = abducer.forget(observation)
//    val forgetStop = System.nanoTime()
//    val hypothesisBeforeSimplification = KBNegator.negate(forgettingResultBeforeSimplification)
//
//    val hypothesisSize = hypothesisBeforeSimplification.size
//    val hypothesisDisjuncts = hypothesisBeforeSimplification match {
//      case DisjunctiveDLStatement(ds) => ds.size
//      case other => 1
//    }
//    val (countABox, countTBox) = countABoxTBox(hypothesisBeforeSimplification)
//    val fixpoints = hypothesisBeforeSimplification.subConcepts.exists(c=> c._1.isInstanceOf[LeastFixpoint]||c._1.isInstanceOf[GreatestFixpoint])
//    val nominals = hypothesisBeforeSimplification.subConcepts.exists(c => c._1.isInstanceOf[NominalSet])
//    val inverseRoles = hypothesisBeforeSimplification.roles.exists(c => c.isInstanceOf[InverseRole])
//
//    print("observationSize: "+observation.size+" observationTBox: "+observation.tbox.axioms.size
//      +" observationABox: "+observation.abox.assertions.size)
//    println()
//    println("BEFORE SIMPLIFICATION: " + "forgetting time taken: " + (forgetStop - forgetStart) + " hypothesisSize: "+hypothesisSize+" disjuncts: "+hypothesisDisjuncts+" tboxAxioms: "+countTBox+" aboxAssertions: "+countABox+
//      " fixpoints: "+fixpoints+ " nominals: "+nominals+" inverseRoles: "+inverseRoles)
//    println()
//
//    //Simplification
//    val simplificationStart = System.nanoTime()
//    val forgettingResultAfterSimplification = abducer.simplify(forgettingResultBeforeSimplification)
//    val simplificationStop = System.nanoTime()
//
//    val hypothesisAfterSimplification = KBNegator.negate(forgettingResultAfterSimplification)
//
//    val hypothesisSizeSimplified = hypothesisAfterSimplification.size
//    val hypothesisDisjunctsSimplified = hypothesisAfterSimplification match {
//      case DisjunctiveDLStatement(ds) => ds.size
//      case other => 1
//    }
//    val (countABoxSimplified, countTBoxSimplified) = countABoxTBox(hypothesisAfterSimplification)
//    val fixpointsSimplified = hypothesisAfterSimplification.subConcepts.exists(c=> c._1.isInstanceOf[LeastFixpoint]||c._1.isInstanceOf[GreatestFixpoint])
//    val nominalsSimplified = hypothesisAfterSimplification.subConcepts.exists(c => c._1.isInstanceOf[NominalSet])
//    val inverseRolesSimplified = hypothesisAfterSimplification.roles.exists(c => c.isInstanceOf[InverseRole])
//
//    println("AFTER SIMPLIFICATION: " + "simplification time taken: " + (simplificationStop - simplificationStart) + " hypothesisSize: "+hypothesisSizeSimplified+" disjuncts: "+hypothesisDisjunctsSimplified+" tboxAxioms: "+countTBoxSimplified+" aboxAssertions: "+countABoxSimplified+
//      " fixpoints: "+fixpointsSimplified+ " nominals: "+nominalsSimplified+" inverseRoles: "+inverseRolesSimplified)
//    println()
//
//    //Filtering
//    val filterStart = System.nanoTime()
//    val hypothesisAfterFiltering = abducer.filter(forgettingResultAfterSimplification)
//    val filterStop = System.nanoTime()
//
//    val hypothesisSizeFiltered = hypothesisAfterFiltering.size
//    val hypothesisDisjunctsFiltered = hypothesisAfterFiltering match {
//      case DisjunctiveDLStatement(ds) => ds.size
//      case other => 1
//    }
//    val (countABoxFiltered, countTBoxFiltered) = countABoxTBox(hypothesisAfterFiltering)
//    val fixpointsFiltered = hypothesisAfterFiltering.subConcepts.exists(c=> c._1.isInstanceOf[LeastFixpoint]||c._1.isInstanceOf[GreatestFixpoint])
//    val nominalsFiltered = hypothesisAfterFiltering.subConcepts.exists(c => c._1.isInstanceOf[NominalSet])
//    val inverseRolesFiltered = hypothesisAfterFiltering.roles.exists(c => c.isInstanceOf[InverseRole])
//
//    println("AFTER FILTERING: " + "filtering time taken: " + (filterStop - filterStart) + " hypothesisSize: "+hypothesisSizeFiltered+" disjuncts: "+hypothesisDisjunctsFiltered+" tboxAxioms: "+countTBoxFiltered+" aboxAssertions: "+countABoxFiltered+
//      " fixpoints: "+fixpointsFiltered+ " nominals: "+nominalsFiltered+" inverseRoles: "+inverseRolesFiltered)
//  }
//
//  def countABoxTBox(dlStatement: DLStatement): (Int,Int) = dlStatement match {
//    case DisjunctiveDLStatement(ds) => ds.toSeq.map(countABoxTBox).fold((0,0))((a1,a2) => (a1._1+a2._1, a1._2+a2._2))
//    case ConjunctiveDLStatement(ds) => ds.toSeq.map(countABoxTBox).fold((0,0))((a1,a2) => (a1._1+a2._1, a1._2+a2._2))
//    case DisjunctiveAssertion(ds) => ds.toSeq.map(countABoxTBox).fold((0,0))((a1,a2) => (a1._1+a2._1, a1._2+a2._2))
//    case ConjunctiveAssertion(ds) => ds.toSeq.map(countABoxTBox).fold((0,0))((a1,a2) => (a1._1+a2._1, a1._2+a2._2))
//    case _: Assertion => (1, 0)
//    case _: Axiom => (0,1)
//  }
//
//  def extractObservation(ontology: Ontology) = {
//
//    val observationSize = Random.nextInt(4) + 1
//
//    val statementSeq = ontology.statements.toSeq
//
//    val observation = (1 to observationSize).map{ i =>
//      val pos = Random.nextInt(statementSeq.size)
//      statementSeq(pos)
//    }
//
//    observation.foreach(ontology.remove)
//
//    var abducibles = Set[String]()
//
//    //val targetSize = ontology.signature.size*.7
//    val percentAbducible = 100 - (Random.nextInt(9) + 1) //between 1 and 10% nonabducible
//    val targetSize = math.floor(ontology.signature.size*(percentAbducible.toFloat/100))
//    println("numAbducibles: " + targetSize)
//
//    while(abducibles.size<targetSize) {
//      val st = statementSeq(Random.nextInt(statementSeq.size))
//      val sigSeq = st.signature.toSeq
//      if(!sigSeq.isEmpty)
//        abducibles += sigSeq(Random.nextInt(sigSeq.size))
//    }
//
//    var nonAbducibles = ((ontology.signature ++ Ontology.buildFrom(observation).signature) -- abducibles)
//    println("Obs signature, non-abducibles, intersect?: " + Ontology.buildFrom(observation).signature.intersect(nonAbducibles))
//
//    for(st <- observation) {
//      val sigSeq = st.signature.toSeq
//      if(sigSeq.nonEmpty && sigSeq.intersect(nonAbducibles.toSeq).isEmpty) {
//        val symbol = sigSeq(Random.nextInt(sigSeq.size))
//        println("Obs axiom has no non-abducible, removing abducible: " + symbol)
//        abducibles -= symbol
//        nonAbducibles += symbol
//      }
//      else {
//        println("Obs axiom already contains a non-abducible.")
//      }
//    }
//
//    println("Obs signature, non-abducibles, intersect? AFTER: " + Ontology.buildFrom(observation).signature.intersect(nonAbducibles))
//
//
//    (ontology, Ontology.buildFrom(observation), abducibles)
//  }
//
//}
