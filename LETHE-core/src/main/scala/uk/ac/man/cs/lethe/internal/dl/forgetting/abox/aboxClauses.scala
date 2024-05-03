package uk.ac.man.cs.lethe.internal.dl.forgetting.abox

/**
 * Classes related to basic data types used in direct resolution methods
 */

import uk.ac.man.cs.lethe.internal.dl.forgetting.direct.{ALCFormulaPreparations, ConceptClause, ConceptLiteral, RoleHierarchy, SimpleLiteralOrdering}

import scala.collection.immutable.TreeSet
import scala.collection.mutable.HashSet

//import com.dongxiguo.fastring.Fastring.Implicits._
import com.typesafe.scalalogging.Logger

import uk.ac.man.cs.lethe.internal.dl.datatypes._



object ABoxClause {

  // return union of literals
  def combine(clauses: Iterable[ABoxClause]) = { 
    // assume same ordering for all clauses
    val ord = clauses.head.literals.head._2.ordering
    
    new ABoxClause(clauses.flatMap(_.literals.keySet).map(key => 
      (key, new ConceptClause(clauses.flatMap(_.literalsOf(key)), ord))).toMap)
  }

  def empty = new ABoxClause()
}

class ABoxClause(var literals: Map[Individual, ConceptClause] = Map(), 
		 temp: Boolean=false) 
extends Expression { 

  literals = literals.filterNot(_._2.literals.isEmpty)

  assert(temp || !literals.isEmpty, "empty ABox clause - ontology inconsistent?") 

  def this(individual: Individual, conceptClause: ConceptClause, temp: Boolean) =
    this(Map(individual -> conceptClause), temp)

  def this(individual: Individual, conceptClause: ConceptClause) =
    this(individual, conceptClause, false)


  override def toString = literals.flatMap(_ match { 
    case (individual, clause) => clause.literals.map(_.toString+"("+individual.toString+")")
  }).mkString(" v ")

  override def atomicConcepts = literals.values.flatMap(_.atomicConcepts).toSet
  override def roleSymbols = literals.values.flatMap(_.roleSymbols)toSet
  override def signature = literals.values.flatMap(_.signature).toSet
  override def size = literals.values.map(_.size).sum*2
  override def subConcepts = literals.values.map(_.subConcepts).reduce(_++_)


  /**
   * union of the literal sets of this clause and the other
   */
  def combineWith(other: ABoxClause) = ABoxClause.combine(List(this, other))

  /**
   * replace literals associated to ind with clause
   */
  def replace(ind: Individual, clause: ConceptClause) = new ABoxClause(literals-ind+(ind->clause))


  def literalsOf(ind: Individual): Set[ConceptLiteral] = { 
    literals.get(ind) match { 
      case Some(clause) => clause.literals
      case None => Set()
    }
  }

  def withOrdering(ordering: Ordering[ConceptLiteral]) = 
    new ABoxClause(literals.mapValues(_.withOrdering(ordering)), temp)
}

class ABoxClauseOrdering(clauseOrdering: Ordering[ConceptClause]) extends Ordering[ABoxClause] { 

  val compareTuples = { (pair1: (Individual, ConceptClause), pair2: (Individual, ConceptClause)) =>
    // pair1._1.name.compare(pair2._1.name) match { 
    //   case 0 => clauseOrdering.compare(pair1._2, pair2._2)
    //   case other => other
    // }
    clauseOrdering.compare(pair1._2, pair2._2) match { 
      case 0 => pair1._1.name.compare(pair2._1.name)
      case other => other
    }
  }

  def compare(clause1: ABoxClause, clause2: ABoxClause) = { 
    SeqSorting.compare(clause1.literals.toList.sortWith(compareTuples(_,_)<0),
		       clause2.literals.toList.sortWith(compareTuples(_,_)<0),
		       compareTuples)
  }
}

object SeqSorting { 
  def compare[A](seq1: Seq[A], seq2: Seq[A], lt: (A,A) => Int): Int = (seq1, seq2) match { 
    case (Seq(), Seq()) => 0
    case (Seq(), _) => -1
    case (_, Seq()) => +1
    case (a::as, b::bs) if a==b => compare(as, bs, lt)
    case (a::_, b::_) => lt(a,b)
  }
}

// object ABoxLiteral { 
//   def instantiate(conceptLiteral: ConceptLiteral, individual: Individual) = 
//     ABoxLiteral(conceptLiteral.polarity, conceptLiteral.concept, individual)
// }

// case class ABoxLiteral(polarity: Boolean, concept: Concept, individual: Individual) extends Expression{ 
//   def negate = ABoxLiteral(!polarity, concept, individual)
 
//   override def signature = concept.signature
//   override def atomicConcepts = concept.atomicConcepts
//   override def roleSymbols = concept.roleSymbols
//   override def subConcepts = concept.subConcepts
//   override def size = 3

//   override def toString = 
//     (if(polarity) "+" else "-")+concept.toString+"("+individual.toString+")"

  
// }


object ABoxClausification { 

  def clausify(abox: ABox, 
	       ordering: Ordering[ConceptLiteral] = SimpleLiteralOrdering)
  : (Set[ABoxClause], Set[ConceptClause], Set[RoleAssertion]) = { 
    clausify(abox.assertions, ordering)
  }

  def clausify(assertions: Set[Assertion])
  : (Set[ABoxClause], Set[ConceptClause], Set[RoleAssertion]) = 
    clausify(assertions, SimpleLiteralOrdering)
  
  def clausify(assertions: Set[Assertion], 
	       ordering: Ordering[ConceptLiteral])
  : (Set[ABoxClause], Set[ConceptClause], Set[RoleAssertion]) = { 
    var roleAssertions = new HashSet[RoleAssertion]()
    var conceptClauses = new HashSet[ConceptClause]()
    var aboxClauses = new HashSet[ABoxClause]()

    assertions.foreach{ a => a match { 
      case r: RoleAssertion => roleAssertions.add(r)
      case ConceptAssertion(TopConcept, _) => ;
      case DisjunctiveConceptAssertion(ds) => { 
	val maps = ds.map{ _ match { 
	  case ConceptAssertion(concept, individual) => { 
	    val nnf = ALCFormulaPreparations.nnf(concept)
	    val replaced::definitions = ALCFormulaPreparations.replaceFillers(nnf)
	    definitions.flatMap(clauses(_,ordering)).foreach(conceptClauses.add)
	    val genClauses = clauses(replaced, ordering)
	    (individual -> genClauses)
	  }
	}}
	flatten(maps).foreach{ aboxClauses.add }
      }
      case ConceptAssertion(concept, individual) => { 
	val nnf = ALCFormulaPreparations.nnf(concept)
	val replaced::definitions = ALCFormulaPreparations.replaceFillers(nnf)
	definitions.flatMap(clauses(_,ordering)).foreach(conceptClauses.add)

	clauses(replaced, ordering).foreach{ cl => aboxClauses.add(new ABoxClause(individual, cl)) }
      }
    }}

    (aboxClauses.toSet, conceptClauses.toSet, roleAssertions.toSet)
  }

  def flatten(maps: Set[(Individual, Set[ConceptClause])])
  : Set[ABoxClause] = { 
    if(maps.isEmpty)
      Set()
    else { 
      val (ind, clauses) = maps.head
      if(maps.tail.isEmpty)
	clauses.map(new ABoxClause(ind, _, temp=true))
      else
	clauses.flatMap{ clause =>
	  flatten(maps.tail).map{ aboxClause => new ABoxClause(aboxClause.literals + (ind->clause), temp=true)}
      }
    }
  }

  def clauses(concept: Concept, ordering: Ordering[ConceptLiteral]): Set[ConceptClause] = { 
    ALCFormulaPreparations.cnf(concept) match {
      case ConceptConjunction(cs) => cs.map(clause(_, ordering))
      case c => Set(clause(c, ordering))
    }
  }

  def clause(conc: Concept, ordering: Ordering[ConceptLiteral]): ConceptClause = conc match { 
    case ConceptDisjunction(ds) => 
      new ConceptClause(ds.map(ALCFormulaPreparations.toLiteral(_)), ordering)
    case p => new ConceptClause(Set(ALCFormulaPreparations.toLiteral(p)), ordering)
  }

  def declausify(aboxClauses: Set[ABoxClause], definitionClauses: Set[ConceptClause])
  : Set[DLStatement] = { 
    import uk.ac.man.cs.lethe.internal.dl.forgetting.direct.SimpleDefinerEliminator._
    var definerSubs = groupSubsumptions(definitionClauses.map(toSubsumption))
    definerSubs = definerSubs.filter(s => isDefiner(s.subsumer))
    var result = aboxClauses.map(toDisjunctiveAssertion).toSet[DLStatement]
    var cyclicDefinitions = Set[Subsumption]()

    var cyclicDefiners = Set[Concept]()

    while(!definerSubs.isEmpty){ 
      val next = definerSubs.head
      definerSubs = definerSubs.tail
      next match { 
	// cyclic definer
	case Subsumption(d,c) if c.atomicConcepts.contains(d.toString) && isDefiner(d) => { 
	  assert(d.toString.startsWith("_D"))
	  logger.trace("Cyclic definer: "+d.toString)
	  if(tautologicFixpoint(d,c)){ 
	    result = result.map(expandDefinition(Subsumption(d, TopConcept),_))
	    definerSubs = definerSubs.map(expandDefinition(Subsumption(d, TopConcept),_))
	  } 
	  else if(result.exists(_.atomicConcepts(d.toString))
		  || definerSubs.exists(_.atomicConcepts(d.toString))){ 
		    result += Subsumption(d,c)
		    cyclicDefiners += d
		  }
	}
	case definition => { // check for definers????? (now fixed elsewhere)
	  result = result.map(expandDefinition(definition, _))
	  definerSubs = definerSubs.map(expandDefinition(definition, _))
	}
      }

    }

    result
  }

  def toDisjunctiveAssertion(aboxClause: ABoxClause): Assertion = { 
    assert (!aboxClause.literals.isEmpty, "empty ABox clause - ontology inconsistent?")
    if(aboxClause.literals.size==1) { 
      val ind = aboxClause.literals.keySet.head
      ConceptAssertion(aboxClause.literals(ind).convertBack, ind)
    } else { 
      DisjunctiveConceptAssertion(aboxClause.literals.keySet.map{ ind =>
	ConceptAssertion(aboxClause.literals(ind).convertBack, ind)
      })
    }
  }
}
