//package tests
//
//import uk.ac.man.cs.lethe.internal.dl.datatypes.{Assertion, Axiom, DLStatement, Ontology, OntologyFilter}
//import uk.ac.man.cs.lethe.internal.dl.owlapi.{ModuleExtractor, OWLApiConverter, OWLParser}
//
//import scala.util.Random
//import scala.collection.JavaConverters._
//import java.io.File
//
//import org.semanticweb.owlapi.apibinding.OWLManager
//import org.semanticweb.owlapi.model.OWLEquivalentClassesAxiom
//import uk.ac.man.cs.lethe.internal.dl.abduction.BasicAbducer
//import uk.ac.man.cs.lethe.internal.dl.abduction.forgetting.{ConjunctiveAssertion, ConjunctiveDLStatement, DisjunctiveAssertion, DisjunctiveDLStatement, GreatestFixpoint, LeastFixpoint}
//import uk.ac.man.cs.lethe.internal.dl.filters.OWLOntologyFilters
//import uk.ac.man.cs.lethe.internal.dl.forgetting.direct.ALCFormulaPreparations
//import uk.ac.man.cs.lethe.internal.tools.formatting.SimpleDLFormatter
//
//object AbductionSmokeTest {
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
//    val abducer = new BasicAbducer()
//    abducer.setBackgroundOntology(background)
//    abducer.setAbducibles(abducibles.toSet)
//
//    println("Observation:")
//    println(SimpleDLFormatter.format(observation))
//
//    val hypothesis = abducer.abduce(observation)
//
//    println("Hypothesis:")
//    //println(hypothesis)
//    println()
//    println(SimpleDLFormatter.format(hypothesis))
//
//    val hypothesisSize = hypothesis.size
//    val hypothesisDisjuncts = hypothesis match {
//      case DisjunctiveDLStatement(ds) => ds.size
//      case other => 1
//    }
//
//    val (countABox, countTBox) = countABoxTBox(hypothesis)
//
//    val fixpoints = hypothesis.subConcepts.exists(c=> c._1.isInstanceOf[LeastFixpoint]||c._1.isInstanceOf[GreatestFixpoint])
//
//    print("observationSize: "+observation.size+" observationTBox: "+observation.tbox.axioms.size
//      +" observationABox: "+observation.abox.assertions.size)
//
//    println(" hypothesisSize: "+hypothesisSize+" disjuncts: "+hypothesisDisjuncts+" tboxAxioms: "+countTBox+" aboxAssertions: "+countABox+
//      " fixpoints: "+fixpoints)
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
//    val observationSize = 5
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
//    // names from the observation are never in the abducible set
//    //val relevantSignature = (ontology.signature -- observation.flatMap(_.signature))
//    //  .toSeq
//
//    // from the rest, half of the names is
//    //val abducibles = Random.shuffle(relevantSignature).take((relevantSignature.size*.5).toInt)
//
//    var abducibles = Set[String]()
//
//    val targetSize = ontology.signature.size*.7
//
//    while(abducibles.size<targetSize) {
//      val st = statementSeq(Random.nextInt(statementSeq.size))
//      val sigSeq = st.signature.toSeq
//      if(!sigSeq.isEmpty)
//        abducibles += sigSeq(Random.nextInt(sigSeq.size))
//    }
//
//    (ontology, Ontology.buildFrom(observation), abducibles)
//  }
//
//}
