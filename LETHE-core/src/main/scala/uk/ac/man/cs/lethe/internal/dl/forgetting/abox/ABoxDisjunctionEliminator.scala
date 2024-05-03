package uk.ac.man.cs.lethe.internal.dl.forgetting.abox

import uk.ac.man.cs.lethe.internal.dl.datatypes._

import com.typesafe.scalalogging.Logger

/**
 * Replace disjunctive ABox assertions by ABox assertions. Assumption: All individuals
 * occuring in a disjunctive ABox are connected by a role
 */
object ABoxDisjunctionEliminator { 
  def process(statements: Set[DLStatement], roleAssertions: Iterable[RoleAssertion])
  : Set[DLStatement] = { 
    statements.map(process(_, roleAssertions))
  }

  def process(statement: DLStatement, roleAssertions: Iterable[RoleAssertion])
  : DLStatement = statement match { 
    case _: Axiom => statement
    case _: ConceptAssertion => statement
    case DisjunctiveConceptAssertion(cas) if cas.size==1 => cas.head
    case DisjunctiveConceptAssertion(cas) => { 
      val ca1 = cas.head
      val ca2 = cas.tail.head
      val tail = cas.tail.tail
      
      val (i1, i2) = (ca1.individual, ca2.individual)

      getConnection(i1, i2, roleAssertions) match { 
      	case Some(r) => { 
      	  val concept = 
      	    ConceptDisjunction(Set(ca1.concept,
      				   ExistentialRoleRestriction(r, 
      							      ConceptConjunction(Set(NominalSet(Set(i2)),
      										     ca2.concept)))))
      	  val next = DisjunctiveConceptAssertion(tail 
      						 + ConceptAssertion(concept, i1))
      	  process(next, roleAssertions)
      	}
      	case None => { 
      	  getConnection(i2, i1, roleAssertions) match { 
      	    case Some(r) => { 
      	      val concept = 
      		ConceptDisjunction(Set(ca2.concept,
      				       ExistentialRoleRestriction(r,
      								  ConceptConjunction(Set(NominalSet(Set(i1)),
      											 ca1.concept)))))
      	      val next = DisjunctiveConceptAssertion(tail 
      						     + ConceptAssertion(concept, i2))
      	      process(next, roleAssertions)
      	    }
      	    case None => assert(false, "Individuals not connected: "+i1+", "+i2)
      		null
      	  }
      	}
      }	
    }
  }

  def getConnection(i1: Individual, i2: Individual, roleAssertions: Iterable[RoleAssertion])
  : Option[BaseRole] = { 
    roleAssertions.find(ra => ra.individual1==i1 && ra.individual2==i2).map(_.role.asInstanceOf[BaseRole])
  }
}
