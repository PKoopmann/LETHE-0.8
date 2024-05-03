package uk.ac.man.cs.lethe.internal.dl.forgetting

import scala.collection.mutable.{ Set => mutSet }
import scala.collection.mutable.HashSet

import uk.ac.man.cs.lethe.internal.dl.datatypes._

object DLForgettingPreprocessing { 

  // // Filter out occurrences of roles in symbol set
  // def filterRoles(ontology: Ontology, symbols: Set[String]): Ontology = { 
  //   val result = new Ontology()
  //   val filtered = ontology.tbox.axioms.flatMap((a: Axiom) => filter(a, symbols))

  //   result.tbox = TBox(filtered)

  //   result
  // }
  // <---- dangerous old code, unsound!

  // sort axioms into three sets 
  def sortStatements(statements: Iterable[DLStatement], symbols: Set[String])
  : (mutSet[DLStatement], mutSet[DLStatement], mutSet[DLStatement]) = { 
    var base: mutSet[DLStatement] = HashSet()
    var nonBase: mutSet[DLStatement] = HashSet()    
    var both: mutSet[DLStatement] = HashSet()

    statements.foreach{ x =>
      if(x.signature.exists(symbols)) { 
	if(x.signature.exists(!symbols(_)))
	  both += x
	else
	  nonBase += x
      } else
	base += x
	
      
      // val symbols2 = x.signature
      // val difference = symbols2 -- symbols
      // if(difference.isEmpty){ 
      // 	nonBase += x
      // }
      // else if(difference==symbols2){ 
      // 	base += x
      // }
      // else{ 
      // 	both += x
      // }
    }

    (base, nonBase, both)
  }


  def filter(axiom: Axiom, symbols: Set[String]): Option[Axiom] = { 
    val result = axiom match { 
    case Subsumption(a, b) => 
      val (a2, b2) = (filter(a, symbols), filter(b, symbols))
      if(a!=TopConcept && a2==TopConcept)
	None
      else if(b!=TopConcept && b2==TopConcept)
	None
      else 
	Some(Subsumption(a2, b2))

    case ConceptEquivalence(a, b) => 
      val (a2, b2) = (filter(a, symbols), filter(b, symbols))
      if(a!=TopConcept && a2==TopConcept)
	None
      else if(b!=TopConcept && b2==TopConcept)
	None
      else 
	Some(ConceptEquivalence(a2, b2))
    }
    result
  }

  def filter(concept: Concept, symbols: Set[String]): Concept = { 
    val result = concept match { 
      case TopConcept => TopConcept
      case BottomConcept => BottomConcept
      case b: BaseConcept => b
      case ConceptComplement(c) => ConceptComplement(filter(c, symbols))
      case ConceptConjunction(cs) => ConceptConjunction(filter(cs, symbols))
      case ConceptDisjunction(ds) => ConceptDisjunction(filter(ds, symbols))
      case UniversalRoleRestriction(BaseRole(r), f) if symbols.contains(r) => println("filtered: "+concept); TopConcept
      case ExistentialRoleRestriction(BaseRole(r), f) if symbols.contains(r) => println("filtered: "+concept); TopConcept    
      case UniversalRoleRestriction(BaseRole(r), f)  => UniversalRoleRestriction(BaseRole(r), filter(f, symbols))
      case ExistentialRoleRestriction(BaseRole(r), f) => ExistentialRoleRestriction(BaseRole(r), filter(f, symbols))
      case _ => throw new AssertionError("Unexpected concept: "+concept.toString)
    }
    result
  }

  def filter(concepts: Iterable[Concept], symbols: Set[String]): Set[Concept] = 
    concepts.collect{ 
      case UniversalRoleRestriction(BaseRole(r), f) if !symbols.contains(r) => UniversalRoleRestriction(BaseRole(r), filter(f, symbols))
      case ExistentialRoleRestriction(BaseRole(r), f) if !symbols.contains(r) => ExistentialRoleRestriction(BaseRole(r), filter(f, symbols))
      case TopConcept => TopConcept
      case BottomConcept => BottomConcept
      case b: BaseConcept => b
      case ConceptComplement(c) => ConceptComplement(filter(c, symbols))
      case ConceptConjunction(cs) => ConceptConjunction(filter(cs, symbols))
      case ConceptDisjunction(ds) => ConceptDisjunction(filter(ds, symbols))
    }.toSet

}
