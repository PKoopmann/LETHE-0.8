package uk.ac.man.cs.lethe.internal.dl.logicalDifference

import org.semanticweb.HermiT.Reasoner
import uk.ac.man.cs.lethe.internal.dl.owlapi.{ModuleExtractor, OWLApiInterface, OWLExporter}

import java.util.{HashSet, Set}
import scala.collection.JavaConversions._
import scala.collection.mutable.HashMap
//import org.semanticweb.more.{OWL2ReasonerManager, MOReReasoner}
// import uk.ac.manchester.cs.jfact.JFactFactory

//import uk.ac.manchester.cs.chainsaw.ChainsawReasonerFactory
//import org.semanticweb.owlapi.owllink.{OWLlinkHTTPXMLReasonerFactory, 
//				       OWLlinkReasonerConfiguration,
//				       OWLlinkReasonerIOException}

import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model._
import org.semanticweb.owlapi.reasoner.OWLReasoner
import uk.ac.man.cs.lethe.interpolation._


/**
 * Class for computing the logical difference between two ontologies.
 *
 * @param interpolator The Interpolator to be used for computing the logical difference
 */
class LogicalDifferenceComputer(interpolator: IOWLInterpolator) {

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

    //    println("Computing syntactical diff...")
    //    println(new Date())
    val syntDiff = SyntacticalDifferenceComputer.syntacticalDifference(ontology1, ontology2)
    //    println(new Date())
    //    println(syntDiff.getLogicalAxioms.size+" new axioms")

    val sig1 = OWLApiInterface.getSignature(ontology1)
    val sig2 = OWLApiInterface.getSignature(syntDiff)
    val commonSignature = sig1.filter(sig2)
    //    println("Common signature has "+commonSignature.size+" elements")

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

    val logicalEntailmentChecker: LogicalEntailmentChecker =
      LogicalEntailmentCheckerClassical
    //      LogicalEntailmentCheckerUsingClassification


    var interpolant = interpolator.uniformInterpolant(ontology2, signature, approximationLevel)

    //    println()
    //    println("Extract Module")
    //    println(new Date())
    var ont1Module = ModuleExtractor.getModule(ontology1, asScalaSet(signature).toSet[OWLEntity])
    //    println(new Date())
    //    println("Module contains "+ont1Module.size+" axioms")
    val modOnt = (new OWLExporter()).toOwlOntology(ont1Module)

    val intAx = interpolant.getLogicalAxioms().toSet[OWLLogicalAxiom]

    //    println()
    //    println("Checking for new entailments ("+intAx.size+" axioms)")
    //    println(new Date())

    //val result = intAx.filterNot(logicalEntailmentChecker.filterEntailed(modOnt, intAx))
    val result = logicalEntailmentChecker.filterNotEntailed(modOnt, intAx)

    //    println(new Date())

    val implicitDiff = result.filterNot(asScalaSet(ontology2.getLogicalAxioms))
    //    println(implicitDiff.size+" new entailments are implicit")

    // We detect redundant new axioms that are: 
    // 1. In the input ontology
    // 2. In the interpolant (since we do not check other axioms for entailments)
    // 3. Not in the result
    val redundant = asScalaSet(ontology2.getLogicalAxioms).filter(intAx).filterNot(result)
    //    println(redundant.size+" new axioms are redundant")

    result
  }

}

/**
 * Object for computing the syntactical difference between ontologies, that is, 
 * the set of axioms of the second ontology that are not contained in the first 
 * ontology. 
 */
object SyntacticalDifferenceComputer {
  /**
   * Returns the set of axioms from the second ontology that are not contained in
   * the first ontology.
   *
   * @param ontology1 The first ontology.
   * @param ontology2 The second ontology.
   *
   * @return An OWL ontology consisting of the axioms of the second ontology that
   *   are not contained in the first ontology.
   */
  def syntacticalDifference(ontology1: OWLOntology,
                            ontology2: OWLOntology)
  : OWLOntology = {
    val axioms = ontology2.getLogicalAxioms().filterNot(asScalaSet(ontology1.getLogicalAxioms()))

    (new OWLExporter).toOwlOntology(axioms)
  }
}

/*
 * Abstract class for checking sets of axioms for entailment.
 */

abstract class LogicalEntailmentChecker {
  /**
   * Filters from a given set of axioms all axioms that are entailed by 
   * the given ontology. 
   *
   * @param owlOntology The OWL ontology against which entailments are to be checked.
   * @param axioms The axioms which are to be checked for entailment.	
   *
   * @return The axioms contained in the given set that are entailed by 
   *   the ontology.
   */
  def filterEntailed(owlOntology: OWLOntology, axioms: Set[OWLLogicalAxiom]): Set[OWLLogicalAxiom]

  /**
   * Filters from a given set of axioms all the axioms that are not 
   * entailed by the given ontology.
   *
   * @param owlOntology The ontology against which entailments are being checked.
   * @param axioms The set of axioms that are to be checked for entailment.
   *
   * @return A axioms contained in the given set that are not entailed by the
   *   given ontology.
   */
  def filterNotEntailed(owlOntology: OWLOntology, axioms: Set[OWLLogicalAxiom]) = {
    axioms.filterNot(filterEntailed(owlOntology, axioms))
  }

  private val reasoner = "HERMIT"

  private val onlySupportsAtomicEntailmentList = Seq("MORE+JFACT", "MORE+HERMIT")

  def supportsComplexEntailment = !onlySupportsAtomicEntailmentList.contains(reasoner)

  /**
   * Returns the OWLReasoner to be used for deciding entailments.
   *
   * @param owlOntology Ontology with which the reasoner is to be initialised.
   *
   * @return The reasoner to be used for computing logical differences.
   */
  def getReasoner(owlOntology: OWLOntology) = {

    if(reasoner == "HERMIT"){
      // HermiT
      new Reasoner.ReasonerFactory().createReasoner(owlOntology)
    }
    else if(reasoner=="JFACT") {
      //new JFactFactory().createReasoner(owlOntology)
      throw new AssertionError("Not supported!")
    }
    // } else if(reasoner=="CHAINSAW"){
    //   new ChainsawReasonerFactory().createReasoner(owlOntology)
    // }
    //    } else if(reasoner=="MORE+JFACT"){
    //      val result = new MOReReasoner(owlOntology)
    //      result.setReasoner(OWL2ReasonerManager.JFACT)
    //      result
    //    } else if(reasoner=="MORE+HERMIT"){
    //      new MOReReasoner(owlOntology)
    //    }


    // else if(reasoner == "KONCLUDE") { 
    // // Konclude
    // val url = new URL("http://localhost:8080");
    //   val reasonerConfiguration =
    // 	new OWLlinkReasonerConfiguration(url);
    //   val factory = new OWLlinkHTTPXMLReasonerFactory();    

    //   factory.createNonBufferingReasoner(owlOntology, reasonerConfiguration);	
    // } 
    else
      throw new AssertionError("Unsupported reasoner!")
    //new Reasoner.ReasonerFactory().createReasoner(owlOntology)
  }

}

/**
 * Optimised logical entailment checker that uses classification to speed up
 * the computation process. Works best for reasoners that are optimised for 
 * classification.
 */
object LogicalEntailmentCheckerUsingClassification extends LogicalEntailmentChecker {

  private val manager: OWLOntologyManager = OWLManager.createOWLOntologyManager()
  private val factory: OWLDataFactory = manager.getOWLDataFactory()

  def main(arsg: Array[String]): Unit = {
    val manager = OWLManager.createOWLOntologyManager

    val ont = manager.createOntology

    val factory = manager.getOWLDataFactory

    manager.addAxiom(ont,
      factory.getOWLSubClassOfAxiom(
        factory.getOWLClass(IRI.create("http://test.bla/#A")),
        factory.getOWLClass(IRI.create("http://test.bla/#B"))))

    manager.addAxiom(ont,
      factory.getOWLSubClassOfAxiom(
        factory.getOWLClass(IRI.create("http://test.bla/#B")),
        factory.getOWLClass(IRI.create("http://test.bla/#C"))))

    var axioms = new HashSet[OWLLogicalAxiom]()

    axioms.add(
      factory.getOWLSubClassOfAxiom(
        factory.getOWLClass(IRI.create("http://test.bla/#A")),
        factory.getOWLClass(IRI.create("http://test.bla/#C"))))
    //
    axioms.add(
      factory.getOWLSubClassOfAxiom(
        factory.getOWLClass(IRI.create("http://test.bla/#C")),
        factory.getOWLClass(IRI.create("http://test.bla/#A"))))



    println(filterNotEntailed(ont, axioms))

  }


  def filterEntailed(owlOntology: OWLOntology, axioms: Set[OWLLogicalAxiom]) = {

    addClassAxioms(owlOntology, axioms)

    val reasoner = getReasoner(owlOntology)

    asScalaSet(reasoner.getUnsatisfiableClasses.getEntities).filter{ e =>
      concToAxiom.containsKey(e)
    }.map(concToAxiom)
  }

  def addClassAxioms(owlOntology: OWLOntology, axioms: Set[OWLLogicalAxiom]) = {
    axioms.foreach{ ax => manager.addAxiom(owlOntology, toInclusion(ax)) }
  }

  val concToAxiom = new HashMap[OWLClass, OWLLogicalAxiom]()

  def toInclusion(axiom: OWLLogicalAxiom): OWLLogicalAxiom = {
    val corresClass = newClass()
    concToAxiom.put(corresClass, axiom)

    factory.getOWLSubClassOfAxiom(corresClass, negAsClass(axiom))
  }


  def negAsClass(axiom: OWLLogicalAxiom): OWLClassExpression = axiom match {
    case ax: OWLSubClassOfAxiom => {
      // a subclassOf b
      val a = ax.getSubClass
      val b = ax.getSuperClass
      val negB = factory.getOWLObjectComplementOf(b)
      factory.getOWLObjectIntersectionOf(a, negB)
    }
    case ax: OWLSubObjectPropertyOfAxiom => {
      val a = ax.getSubProperty
      val b = ax.getSuperProperty
      factory.getOWLObjectIntersectionOf(
        factory.getOWLObjectSomeValuesFrom(a, factory.getOWLThing),
        factory.getOWLObjectAllValuesFrom(b, factory.getOWLNothing))
    }
    case _ => assert(false, "Unexpected: "+axiom); null
  }

  var count = 0
  def newClass() = {
    val iri = IRI.create("http://koop.mine.mine/#"+count)
    count += 1
    factory.getOWLClass(iri)
  }
}

/**
 * Simple non-optimised logical entailment checker.
 */
object LogicalEntailmentCheckerClassical extends LogicalEntailmentChecker {

  def main(arsg: Array[String]): Unit = {
    val manager = OWLManager.createOWLOntologyManager

    val ont = manager.createOntology

    val factory = manager.getOWLDataFactory

    manager.addAxiom(ont,
      factory.getOWLSubClassOfAxiom(
        factory.getOWLClass(IRI.create("http://test.bla/#A")),
        factory.getOWLClass(IRI.create("http://test.bla/#B"))))

    manager.addAxiom(ont,
      factory.getOWLSubClassOfAxiom(
        factory.getOWLClass(IRI.create("http://test.bla/#B")),
        factory.getOWLClass(IRI.create("http://test.bla/#C"))))

    var axioms = new HashSet[OWLLogicalAxiom]()

    axioms.add(
      factory.getOWLSubClassOfAxiom(
        factory.getOWLClass(IRI.create("http://test.bla/#A")),
        factory.getOWLClass(IRI.create("http://test.bla/#C"))))
    //
    axioms.add(
      factory.getOWLSubClassOfAxiom(
        factory.getOWLClass(IRI.create("http://test.bla/#C")),
        factory.getOWLClass(IRI.create("http://test.bla/#A"))))



    println(filterNotEntailed(ont, axioms))

  }

  override def filterEntailed(owlOntology: OWLOntology, axioms: Set[OWLLogicalAxiom]) = {
    var reasoner = getReasoner(owlOntology)
    axioms.filter{ a => {
      print(".")
      System.out.flush();
      //      try{
      owlOntology.containsAxiom(a) || reasoner.isEntailed(a)
      /*      } catch {
        case _: OWLlinkReasonerIOException =>
          println("Reestablish Link Connection...")
          Thread.sleep(30000)
          reasoner = getReasoner(owlOntology)
          reasoner.isEntailed(a)
            }*/
    }}
  }

  def existsNotEntailed(owlOntology: OWLOntology, axioms: Set[OWLLogicalAxiom]) = {
    val reasoner = getReasoner(owlOntology)
    val result = axioms.exists{ a => {
      print(".")
      System.out.flush();
      //      try{
      !isEntailed(a, owlOntology, reasoner)
      /*      } catch {
        case _: OWLlinkReasonerIOException =>
          println("Reestablish Link Connection...")
          Thread.sleep(30000)
          reasoner = getReasoner(owlOntology)
          reasoner.isEntailed(a)
            }*/
    }}

    reasoner.dispose()

    result
  }

  def isEntailed(a: OWLLogicalAxiom, o: OWLOntology, reasoner: OWLReasoner): Boolean = {

    if(o.containsAxiom(a))
      return true

    val manager = o.getOWLOntologyManager
    val factory = manager.getOWLDataFactory

    //    var axiomsToCheck: Iterable[OWLLogicalAxiom] = Some(a)

    var axiomsToAdd = new HashSet[OWLLogicalAxiom]()

    var count = 0

    def getAxiomsToCheck(a: OWLLogicalAxiom): Iterable[OWLLogicalAxiom] = a match {
      case a: OWLSubClassOfAxiomShortCut => getAxiomsToCheck(a.asOWLSubClassOfAxiom)
      case a: OWLSubClassOfAxiomSetShortCut =>
        a.asOWLSubClassOfAxioms.flatMap(getAxiomsToCheck)
      case a: OWLSubClassOfAxiom if a.getSubClass.isAnonymous || a.getSuperClass.isAnonymous =>
        count += 1
        val l = factory.getOWLClass(IRI.create("L"+count))
        val r = factory.getOWLClass(IRI.create("R"+count))
        axiomsToAdd.add(factory.getOWLEquivalentClassesAxiom(a.getSubClass, l))
        axiomsToAdd.add(factory.getOWLEquivalentClassesAxiom(a.getSuperClass, r))
        Some(factory.getOWLSubClassOfAxiom(l,r))
      case a: OWLSubClassOfAxiom => Some(a)
      case _ => println("IGNORING: "+a); Some(a)
    }

    val axiomsToCheck: Iterable[OWLLogicalAxiom] = if(supportsComplexEntailment){
      Some(a)
    } else getAxiomsToCheck(a)

    manager.addAxioms(o, axiomsToAdd)

    val result = axiomsToCheck.forall(reasoner.isEntailed)

    manager.removeAxioms(o, axiomsToAdd)

    return result

  }

}
