package uk.ac.man.cs.lethe.internal.dl.owlapi

import java.io.File
import java.util.HashSet
import java.util.stream.Collectors
import scala.collection.JavaConverters._
import scala.util.Random
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model._
import uk.ac.manchester.cs.owlapi.modularity.ModuleType
import uk.ac.manchester.cs.owlapi.modularity.SyntacticLocalityModuleExtractor
import com.typesafe.scalalogging.Logger
import uk.ac.man.cs.lethe.internal.tools.MultiSet
import uk.ac.man.cs.lethe.internal.tools.formatting.SimpleOWLFormatter

import scala.collection.convert.ImplicitConversions.`set asScala`

object ModuleExtractor {
  //  implicit val (logger, formatter, appender) = ZeroLoggerFactory.newLogger(this)

  val logger = Logger(ModuleExtractor.getClass)

  /**
   * Returns a random coherent signature of given minimal size
   */
  def coherentSignature(owlOntology: OWLOntology,
                        minSize: Integer): Set[OWLEntity] = {
    var result = Set[OWLEntity]()

    assert(owlOntology.getLogicalAxioms().size > minSize)

    while (result.size < minSize) {
      result ++= coherentSignature(owlOntology)
      println("Signature size: " + result.size)
    }


    result
  }

  /**
   * Returns any random signature that is "coherent", by which we
   * mean the symbols occur together in a genuine module
   */
  def coherentSignature(owlOntology: OWLOntology): Set[OWLEntity] = {
    randomGenuineModule(owlOntology).flatMap(_.getSignature().iterator().asScala.toSet)
  }

  def randomGenuineModule(owlOntology: OWLOntology): Set[OWLAxiom] = {
    val axioms = owlOntology.getLogicalAxioms().iterator.asScala.toSeq
    val randomAxiom = axioms(Random.nextInt(axioms.size))
    println("Extracting genuine module for "+SimpleOWLFormatter.format(randomAxiom))
    val result = genuineModule(owlOntology, randomAxiom)
    println("Module size: " + result.size)
    result
  }


  /**
   * Preparation for genuine module extraction
   * see Del Vescovo et. al.: The Modular Structure of an Ontology (2011)
   */
  def prepare(owlOntology: OWLOntology, moduleType: ModuleType = ModuleType.STAR) = {
    val manager = OWLManager.createOWLOntologyManager();

    val extractor = new SyntacticLocalityModuleExtractor(manager, owlOntology, moduleType)

    println(owlOntology.getLogicalAxioms().size)

    // remove syntactical tautologies
    val ontSignature = owlOntology.getSignature()
    var restricted = extractor.extractAsOntology(ontSignature, IRI.create(owlOntology.getOntologyID.getDefaultDocumentIRI + "noSynt"))
    println(owlOntology.getLogicalAxioms().size)
    val extractor2 = new SyntacticLocalityModuleExtractor(manager, restricted, moduleType)
    var globalAxioms = extractor2.extract(new HashSet[OWLEntity]())
    println(globalAxioms.size)
    globalAxioms.asScala.foreach(manager.removeAxiom(restricted, _))

    restricted
  }

  def genuineModule(owlOntology: OWLOntology,
                    owlAxiom: OWLAxiom,
                    moduleType: ModuleType = ModuleType.STAR): Set[OWLAxiom] =
    getModule(owlOntology, owlAxiom.getSignature().iterator.asScala.toSet, moduleType).filter(_.isInstanceOf[OWLLogicalAxiom])


  def getModule(ontology: OWLOntology,
                signature: Iterable[OWLEntity],
                moduleType: ModuleType = ModuleType.STAR): Set[OWLAxiom] = {
    val manager = OWLManager.createOWLOntologyManager();

    val extractor = new SyntacticLocalityModuleExtractor(manager, ontology, moduleType)

    extractor.extract(signature.toSet[OWLEntity].asJava).asScala.toSet
  }

  def getModuleAsOntology(ontology: OWLOntology,
                          signature: Iterable[OWLEntity],
                          moduleType: ModuleType = ModuleType.STAR): OWLOntology = {
    new OWLExporter().toOwlOntology(getModule(ontology, signature, moduleType))
  }

  /**
   * Get all genuine modules
   */
  def main(args: Array[String]) = {
    val result = new HashSet[Set[OWLAxiom]]()

    val ontology = OWLApiInterface.getOWLOntology(new File(args(0)))
    val prepared = prepare(ontology)

    countSymbolsInGenuineModules(prepared)

    // prepared.getAxioms.foreach{ axiom =>
    //   val module = genuineModule(prepared, axiom)
    //   println("Axioms: "+module.size+"\t Symbols: "+module.flatMap(_.getSignature).toSet.size)
    // }

  }

  def countSymbolsInGenuineModules(ontology: OWLOntology) = {
    val symbolCount = MultiSet[OWLEntity]()

    val imported = ontology.getImportsClosure.iterator().asScala.toSet[OWLOntology] - ontology

    ontology.getAxioms().forEach { axiom =>
      if (!imported.exists(_.containsAxiom(axiom))) {
        val module = genuineModule(ontology, axiom)

        module.flatMap(_.getSignature().iterator().asScala.toSet).foreach(symbolCount.add)
      }
      //      println("Axioms: "+module.size+"\t Symbols: "+module.flatMap(_.getSignature).toSet.size)
    }

    symbolCount
  }
}
