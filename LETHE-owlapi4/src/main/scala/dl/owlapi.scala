
package uk.ac.man.cs.lethe.internal.dl.owlapi

import java.io.{File, InputStream}
import java.net.URL
import java.util.Date
import com.typesafe.scalalogging.Logger
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model._
import org.semanticweb.owlapi.model.parameters.Imports
import uk.ac.man.cs.lethe.internal.dl.datatypes._
import uk.ac.man.cs.lethe.internal.tools.formatting.SimpleDLFormatter

import scala.collection.JavaConverters._

object OWLApiConfiguration { 
  // represent named objects with full IRIs or just fragments?
  var SIMPLIFIED_NAMES = false
}

object OntologyPrinter {


  def main(args: Array[String]) = { 

    println("Loading "+args(0))
    println(new Date())
    val file = new File(args(0))

    val manager = OWLManager.createOWLOntologyManager();
    
    val ontology = manager.loadOntologyFromOntologyDocument(file)

    println(new Date())
    println("Loaded ontology: " + ontology.toString());
    
    ontology.getAxioms().iterator.asScala.foreach{ axiom => {
      OWLApiConverter.convert(axiom).foreach(ax => println(SimpleDLFormatter.format(ax)))
    }}
    println(new Date())
    
  }
}

object OWLParser {   
//  implicit val (logger, formatter, appender) = ZeroLoggerFactory.newLogger(this)

  val logger = Logger(OWLParser.getClass)

  def parseURL(url: String): Ontology = { 
    val manager = OWLManager.createOWLOntologyManager();
    val iri = IRI.create(url)

    val ontology = manager.loadOntologyFromOntologyDocument(iri);

    logger.info("Loaded ontology: " + ontology.toString());
    
    OWLApiConverter.convert(ontology)
  }

  def parseURL(url: URL): Ontology = { 
    val manager = OWLManager.createOWLOntologyManager();
    val iri = IRI.create(url)

    val ontology = manager.loadOntologyFromOntologyDocument(iri);

    logger.info("Loaded ontology: " + ontology.toString());
    
    OWLApiConverter.convert(ontology)
  }


  def parseFile(file: File): Ontology = { 
    val manager = OWLManager.createOWLOntologyManager();
    
    val ontology = manager.loadOntologyFromOntologyDocument(file)

    logger.info("Loaded ontology: " + ontology.toString());
    
    OWLApiConverter.convert(ontology)
  }

}


object OWLApiConverter { 
//  implicit val (logger, formatter, appender) =
//  ZeroLoggerFactory.newLogger(this)
  val logger = Logger(OWLApiConverter.getClass)

  import OWLApiConfiguration._

  var ignoredAxioms: Boolean = false

  def clearIgnoredAxioms() =
    (ignoredAxioms=false)

  def convert(owlOntology: OWLOntology, excludeImports: Boolean=false): Ontology = {
    val result = new Ontology
    val imports = if(excludeImports) Imports.EXCLUDED else Imports.INCLUDED
    val statements =
      owlOntology.getTBoxAxioms(imports).iterator().asScala.toSet ++
      owlOntology.getABoxAxioms(imports).iterator().asScala ++
      owlOntology.getRBoxAxioms(imports).iterator().asScala

    val statementsNr = statements.size

    val s1 = statements.map(convert)
    s1.flatten.foreach{ st =>
      logger.debug("Converted statement: " + st.toString)
      result.addStatement(st)
    }
    logger.debug("Difference in statements size after conversion: "+(result.statements.size-statementsNr))
    result
  }

  def convert(owlAxioms: Set[OWLAxiom]): Set[DLStatement] = { 
    owlAxioms.map(OWLApiConverter.convert).flatten.map{ st =>
      logger.debug("Converted statement: " + st.toString)
      st
    }
  }

  def convert(owlAxiom: OWLAxiom): Set[DLStatement] = { 
    val result: Set[DLStatement] = 
    try { 
      owlAxiom match { 
	case axiom: OWLSubClassOfAxiom => Set(Subsumption(convert(axiom.getSubClass()), convert(axiom.getSuperClass())))
  case axiom: OWLEquivalentClassesAxiom =>
    var processed = Set[(OWLClassExpression, OWLClassExpression)]()
    for(
      concept1 <- axiom.getClassExpressions().iterator().asScala.toSet[OWLClassExpression];
      concept2 <- axiom.getClassExpressions().iterator().asScala.toSet[OWLClassExpression]
      if concept1 != concept2 && !processed.contains((concept2,concept1))
    )
    // TODO: can optimise loop
      yield {
        processed += ((concept1,concept2));
        new ConceptEquivalence(
          convert(concept1),
          convert(concept2)).asInstanceOf[DLStatement]
      }
	case axiom: OWLDisjointClassesAxiom =>
    for(concept1 <- axiom.getClassExpressions().iterator().asScala.toSet[OWLClassExpression];
        concept2 <- axiom.getClassExpressions().iterator().asScala.toSet[OWLClassExpression]
        if concept1 != concept2)
    // TODO: can optimise loop
      yield (DLHelpers.disjoint(convert(concept1), convert(concept2))).asInstanceOf[DLStatement]
	case axiom: OWLDisjointUnionAxiom => convert(axiom.getOWLDisjointClassesAxiom()) ++ convert(axiom.getOWLEquivalentClassesAxiom())
	case axiom: OWLClassAssertionAxiom => Set(ConceptAssertion(convert(axiom.getClassExpression()), convert(axiom.getIndividual)))
	case axiom: OWLObjectPropertyAssertionAxiom => 

	  Set(RoleAssertion(convert(axiom.getProperty()), 
			    convert(axiom.getSubject()), 
			    convert(axiom.getObject())))
	case axiom: OWLObjectPropertyDomainAxiom => Set(Subsumption(ExistentialRoleRestriction(convert(axiom.getProperty), 
											     TopConcept), 
								  convert(axiom.getDomain)))
	case axiom: OWLObjectPropertyRangeAxiom => Set(Subsumption(TopConcept, 
								 UniversalRoleRestriction(convert(axiom.getProperty), 
											  convert(axiom.getRange))))					    
	case axiom: OWLSubObjectPropertyOfAxiom => Set(RoleSubsumption(convert(axiom.getSubProperty), convert(axiom.getSuperProperty)))
	case axiom: OWLInverseObjectPropertiesAxiom => 
	  axiom.asSubObjectPropertyOfAxioms.asScala.toSet[OWLAxiom].flatMap(convert)
	case axiom: OWLTransitiveObjectPropertyAxiom =>
	  Set(TransitiveRoleAxiom(convert(axiom.getProperty)))
	case axiom: OWLFunctionalObjectPropertyAxiom =>
	  Set(Subsumption(TopConcept, MaxNumberRestriction(1, convert(axiom.getProperty), TopConcept)))
        case axiom: OWLSubClassOfAxiomShortCut =>
          convert(axiom.asOWLSubClassOfAxiom)
	case _ => { 
	  logger.warn("ignored axiom (not supported) " + owlAxiom.toString() )
    ignoredAxioms=true

	  Set()
	}
      }
    } catch { 
      case e: Throwable =>
        logger.warn("ignored axiom because of exception: " + owlAxiom.toString())
        ignoredAxioms=true
        Set()
    }

    if(result.isEmpty){ 

    }

    result
  }


  def convert(owlClass: OWLClassExpression): Concept = owlClass match {
    case c if c.isOWLThing => TopConcept
    case c if c.isOWLNothing => BottomConcept 
    case concept: OWLClass => BaseConcept(getName(concept))
    case complement: OWLObjectComplementOf => ConceptComplement(convert(complement.getOperand()))
    case intersection: OWLObjectIntersectionOf =>
      ConceptConjunction(intersection.getOperands.iterator.asScala.map(convert).toSet[Concept])
    case union: OWLObjectUnionOf =>
      ConceptDisjunction(union.getOperands.iterator.asScala.map(convert).toSet[Concept])
    case restriction: OWLObjectSomeValuesFrom => 
      ExistentialRoleRestriction(convert(restriction.getProperty), convert(restriction.getFiller)) 
    case restriction: OWLObjectAllValuesFrom =>
      UniversalRoleRestriction(convert(restriction.getProperty), convert(restriction.getFiller))
//    case restriction: OWLObjectMinCardinality if restriction.getCardinality==0 => TopConcept
    case restriction: OWLObjectMinCardinality => 
      MinNumberRestriction(restriction.getCardinality, convert(restriction.getProperty), convert(restriction.getFiller))
    case restriction: OWLObjectMaxCardinality =>
      MaxNumberRestriction(restriction.getCardinality, convert(restriction.getProperty), convert(restriction.getFiller))
    case restriction: OWLObjectExactCardinality =>
      ConceptConjunction(Set(MaxNumberRestriction(restriction.getCardinality,
						  convert(restriction.getProperty),
						  convert(restriction.getFiller)),
			     MinNumberRestriction(restriction.getCardinality,
						  convert(restriction.getProperty),
						  convert(restriction.getFiller))))
  }

  def convert(property: OWLPropertyExpression): Role = property match {
    case property: OWLObjectProperty if property.isOWLTopObjectProperty => TopRole
    case property: OWLObjectProperty => BaseRole(getName(property))
    case property: OWLObjectInverseOf => InverseRole(convert(property.getInverse))
  }

  def convert(individual: OWLIndividual): Individual = individual match { 
    case individual: OWLNamedIndividual => Individual(getName(individual))
    case individual: OWLAnonymousIndividual => Individual(individual.getID.getID)
  }

  def getName(owlObject: OWLEntity): String = 
    if(SIMPLIFIED_NAMES)
      owlObject.getIRI.getFragment
    else
      owlObject.getIRI.toString
}


object OWLApiInterface {


  //  implicit val (logger, formatter, appender) = ZeroLoggerFactory.newLogger(this)
//  import formatter._

  val logger = Logger(OWLApiInterface.getClass)

  def getOWLOntology(url:String): OWLOntology = { 

    val manager = OWLManager.createOWLOntologyManager();
    val iri = IRI.create(url)

    val ontology = manager.loadOntologyFromOntologyDocument(iri);

    ontology
  }


  def getOWLOntology(url: URL): OWLOntology = { 
    val manager = OWLManager.createOWLOntologyManager();
    val iri = IRI.create(url)

    val ontology = manager.loadOntologyFromOntologyDocument(iri);

    ontology
    }

  def getOWLOntologyFromStream(source: InputStream, ignoreMissingImports: Boolean = false): OWLOntology = {
    val manager = OWLManager.createOWLOntologyManager();


    if(ignoreMissingImports){
      manager.getOntologyLoaderConfiguration.
         setMissingImportHandlingStrategy(MissingImportHandlingStrategy.SILENT)
    }

    val ontology = manager.loadOntologyFromOntologyDocument(source)

    ontology
  }

  def getOWLOntology(file: File, ignoreMissingImports: Boolean = false): OWLOntology = {
    val manager = OWLManager.createOWLOntologyManager();

    if(ignoreMissingImports){
       manager.getOntologyLoaderConfiguration.
         setMissingImportHandlingStrategy(MissingImportHandlingStrategy.SILENT)
    }

    val ontology = manager.loadOntologyFromOntologyDocument(file)

    ontology
  }


  def getSignature(sigStream: InputStream): Set[OWLEntity] = {
    getSignature(getOWLOntologyFromStream(sigStream))
  }


  def getSignature(ontology: OWLOntology): Set[OWLEntity] = {
    ontology.getSignature().iterator.asScala.filter(s =>
	  s.isInstanceOf[OWLClass]||s.isInstanceOf[OWLObjectProperty])
      .filterNot(_.isTopEntity)
      .filterNot(_.isBottomEntity)
      .toSet
	
    //(ontology.getClassesInSignature++ontology.getObjectPropertiesInSignature).toSet[OWLEntity]
  }

  def getSignature(axioms: Iterable[_<:OWLAxiom]): Set[OWLEntity] = 
    axioms.toSet[OWLAxiom].flatMap(getSignature)

  def getSignature(axiom: OWLAxiom): Set[OWLEntity] = { 
    axiom.getSignature.iterator.asScala.toSet[OWLEntity].filter(s =>
      s.isInstanceOf[OWLClass]||s.isInstanceOf[OWLObjectProperty])
      .filterNot(_.isTopEntity)
      .filterNot(_.isBottomEntity)
  }

  def getConceptSignature(ontology: OWLOntology): Set[OWLClass] = { 
    ontology.getClassesInSignature().iterator.asScala.toSet[OWLClass]
      .filterNot(_.isTopEntity)
      .filterNot(_.isBottomEntity)
  }

  // def classify(ontology: OWLOntology) = {   
  //   logger.info("classifying...")
    
  //   val manager = OWLManager.createOWLOntologyManager()
  //   val reasoner = new Reasoner.ReasonerFactory().createReasoner(ontology)


  //   reasoner.precomputeInferences(InferenceType.CLASS_HIERARCHY);
  //   val gens = List[InferredAxiomGenerator[_ <:OWLAxiom]]();
  //   gens.add(new InferredSubClassAxiomGenerator());
		    
  //   val iog = new InferredOntologyGenerator(reasoner, gens);
  //   iog.fillOntology(manager, ontology);
  //   logger.info("done classifying.")
  // }

}
