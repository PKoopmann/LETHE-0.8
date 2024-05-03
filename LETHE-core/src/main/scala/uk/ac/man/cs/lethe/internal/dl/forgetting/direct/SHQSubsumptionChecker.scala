package uk.ac.man.cs.lethe.internal.dl.forgetting.direct

import scala.collection.mutable.{ HashMap, Set => MutSet, MultiMap }

//import com.dongxiguo.fastring.Fastring.Implicits._
import com.typesafe.scalalogging.Logger

import uk.ac.man.cs.lethe.internal.dl.datatypes._
import uk.ac.man.cs.lethe.internal.dl.forgetting.Configuration



trait SHQSubsumptionChecker extends SubsumptionChecker {
  
  override def subsumes(literal1: ConceptLiteral, literal2: ConceptLiteral) = 
    (literal1.concept, literal2.concept) match { 
      case (MinNumberRestriction(n1, r1: BaseRole, f1), 
	    MinNumberRestriction(n2, r2: BaseRole, f2)) => n1>=n2 && subsumes(r1, r2) && subsumesConcept(f1, f2)
      case (MaxNumberRestriction(n1, r1: BaseRole, ConceptComplement(f1)), 
	    MaxNumberRestriction(n2, r2: BaseRole, ConceptComplement(f2))) => 
	      n1<=n2 && subsumes(r2,r1) && subsumesConcept(f1, f2)
      case _ => super.subsumes(literal1, literal2)
  }

  // these should be concepts under a number restriction, only used in this trait
  final def subsumesConcept(concept1: Concept, concept2: Concept): Boolean = (concept1, concept2) match { 
    case (b1: BaseConcept, b2: BaseConcept) => subsumes(b1,b2)
    case (b1: BaseConcept, ConceptDisjunction(bs2)) => bs2.exists(b2 => 
      subsumes(b1,b2.asInstanceOf[BaseConcept]))
    case (ConceptDisjunction(bs1), b2: BaseConcept) => bs1.forall(b1 => 
      subsumes(b1.asInstanceOf[BaseConcept],b2))
    case (ConceptDisjunction(bs1),
	  ConceptDisjunction(bs2)) => 
	    bs1.forall(b1 => bs2.exists(b2 => 
	    subsumes(b1.asInstanceOf[BaseConcept],
		     b2.asInstanceOf[BaseConcept])))
  }

  override def condenseClause(clause: ConceptClause): ConceptClause = { 
    super.condenseClause(new ConceptClause(clause.literals.map(condenseLiteral), clause.ordering))
  }

  def condenseLiteral(literal: ConceptLiteral): ConceptLiteral = literal.concept match { 
    case MinNumberRestriction(n,r,ConceptDisjunction(ds)) => 
      ConceptLiteral(true, MinNumberRestriction(n,r,ConceptDisjunction(ds.filterNot(d1 => 
	ds.exists(d2 => d1!=d2
		  && subsumes(d2.asInstanceOf[BaseConcept],d1.asInstanceOf[BaseConcept]))))))
    case MaxNumberRestriction(n,r,ConceptDisjunction(ds)) => 
      ConceptLiteral(true, MaxNumberRestriction(n,r,ConceptDisjunction(ds.filterNot(d1 => 
	ds.exists(d2 => d1!=d2 
		  && subsumes(d2.asInstanceOf[BaseConcept],d1.asInstanceOf[BaseConcept]))))))
    case _ => literal
  }
}
