package uk.ac.man.cs.lethe.internal.dl.forgetting.direct

//import com.dongxiguo.fastring.Fastring.Implicits._
import com.typesafe.scalalogging.Logger

import uk.ac.man.cs.lethe.internal.dl.datatypes._



object FixpointApproximator {   
  //implicit val (logger, formatter, appender) = ZeroLoggerFactory.newLogger(FixpointApproximator)
  //import formatter._

  val logger = Logger(FixpointApproximator.getClass)


  import ALCFormulaPreparations.isDefiner

  
  
  /**
   * Approximates the given ontology into an ontology
   * without definers. The given nestingLevel determines
   * how often cyclic definers should be replaced.
   *
   * Careful: Changes input arguments
   */
  def approximate(ontology: Ontology,
		  nestingLevel: Int): Ontology = { 

    val definitions = ontology.tbox.axioms.collect{ 
      case Subsumption(definer: BaseConcept, definition) if isDefiner(definer) => (definer, definition)
      case Subsumption(ConceptConjunction(cs), rest) if cs.exists(isDefiner) => { 
	val definer = cs.filter(isDefiner).head.asInstanceOf[BaseConcept]
	val definition = rest match{ 
	  case ConceptDisjunction(ds) => ConceptDisjunction(ds ++ cs.map(ConceptComplement))
	  case concept => ConceptDisjunction(cs.map(ConceptComplement(_).asInstanceOf[Concept])+concept)
	}
	(definer, definition)
      }
    }.toMap
    
    if(definitions.isEmpty)
      logger.info(s"No approximations where neccessary.")

    ontology.tbox.axioms = ontology.tbox.axioms.filterNot(_ match { 
      case Subsumption(definer: BaseConcept, _) => isDefiner(definer)
      case Subsumption(ConceptConjunction(cs), _) => cs.exists(isDefiner)
      case _ => false
    })
    
    (1 to nestingLevel).foreach{ _ => 
      ontology.tbox.axioms = ontology.tbox.axioms.map(applyDefinitions(_, definitions))
      ontology.abox.assertions = ontology.abox.assertions.map(applyDefinitions(_, definitions))
    }
    
    ontology.tbox.axioms = ontology.tbox.axioms.map(definersToTop(_))
    ontology.abox.assertions = ontology.abox.assertions.map(definersToTop(_))

    assert(ontology.signature.map(BaseConcept).filter(isDefiner).isEmpty)

    ontology
  }

  def applyDefinitions[A <: DLStatement](stat: A, definitions: Map[BaseConcept, Concept])
  : A = stat match { 
    case Subsumption(a, b) => Subsumption(applyDefinitions(a, definitions), 
					  applyDefinitions(b, definitions)).asInstanceOf[A]
    case ConceptEquivalence(a,b) => ConceptEquivalence(applyDefinitions(a, definitions), 
						       applyDefinitions(b, definitions)).asInstanceOf[A]
    case ConceptAssertion(c, a) => ConceptAssertion(applyDefinitions(c, definitions), 
						    a).asInstanceOf[A]
    case ra: RoleAssertion => ra.asInstanceOf[A]
  }

  def applyDefinitions(concept: Concept, definitions: Map[BaseConcept, Concept])
  : Concept = concept match { 
    case TopConcept => TopConcept
    case BottomConcept => BottomConcept
    case a: BaseConcept if isDefiner(a) => definitions.getOrElse(a, TopConcept)
    case a: BaseConcept => a
    case ConceptComplement(c) => ConceptComplement(applyDefinitions(c, definitions))
    case ConceptConjunction(cs) => ConceptConjunction(cs.map(applyDefinitions(_, definitions)))
    case ConceptDisjunction(ds) => ConceptDisjunction(ds.map(applyDefinitions(_, definitions)))
    case ExistentialRoleRestriction(r,a) => ExistentialRoleRestriction(r, applyDefinitions(a, definitions))
    case UniversalRoleRestriction(r, a) => UniversalRoleRestriction(r, applyDefinitions(a, definitions))
    case MinNumberRestriction(n,r,a) => MinNumberRestriction(n,r,applyDefinitions(a, definitions))
    case MaxNumberRestriction(n,r,a) => MaxNumberRestriction(n,r,applyDefinitions(a, definitions))
  }

  def definersToTop[A <: DLStatement](stat: A)
  : A = stat match { 
    case Subsumption(a, b) => Subsumption(definersToTop(a), definersToTop(b)).asInstanceOf[A]
    case ConceptEquivalence(a,b) => ConceptEquivalence(definersToTop(a), definersToTop(b)).asInstanceOf[A]
    case ConceptAssertion(c, a) => ConceptAssertion(definersToTop(c), a).asInstanceOf[A]
    case ra: RoleAssertion => ra.asInstanceOf[A]
  }

  def definersToTop(concept: Concept)
  : Concept = concept match { 
    case TopConcept => TopConcept
    case BottomConcept => BottomConcept
    case a: BaseConcept if isDefiner(a) => TopConcept
    case a: BaseConcept => a
    case ConceptComplement(c) => ConceptComplement(definersToTop(c))
    case ConceptConjunction(cs) => ConceptConjunction(cs.map(definersToTop))
    case ConceptDisjunction(ds) => ConceptDisjunction(ds.map(definersToTop))
    case ExistentialRoleRestriction(r,a) => ExistentialRoleRestriction(r, definersToTop(a))
    case UniversalRoleRestriction(r, a) => UniversalRoleRestriction(r, definersToTop(a))
    case MinNumberRestriction(n,r,a) => MinNumberRestriction(n,r,definersToTop(a))
    case MaxNumberRestriction(n,r,a) => MaxNumberRestriction(n,r,definersToTop(a))
  }
}
