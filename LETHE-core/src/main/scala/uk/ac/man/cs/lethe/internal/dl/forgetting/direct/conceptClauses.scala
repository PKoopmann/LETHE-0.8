package uk.ac.man.cs.lethe.internal.dl.forgetting.direct

/**
 * Classes related to basic data types used in direct resolution methods
 */

import scala.collection.immutable.TreeSet

import uk.ac.man.cs.lethe.internal.dl.datatypes._

/***************************************************************************
 *                           Concept clauses                               *
 ***************************************************************************
 */

class ConceptClause(_literals: Iterable[ConceptLiteral], _ordering: Ordering[ConceptLiteral]) 
extends Concept { 

  var isResultCandidate: Boolean = false

  def this(_literals: Iterable[ConceptLiteral]) = this(_literals, SimpleLiteralOrdering)

  val ordering = _ordering
  assert(ordering!=null)

  val literals = TreeSet()(ordering) ++ _literals

  def withOrdering(ordering: Ordering[ConceptLiteral]) = new ConceptClause(literals, ordering)

  def isDerivationCandidate = literals.exists(_.concept match { 
    case _: DerivationCandidate => true
    case _ => false
  }) // derivation candidates are clauses on which we do all derivations

  def convertBack = ConceptDisjunction(literals.map(_.convertBack))

  def signature: Set[String] = literals.flatMap(_.signature)

  override def toString = literals.mkString("[", ", ", "]") + (if(isResultCandidate) "rc" else "")

  var parents:Set[ConceptClause] = Set(this)

  override def equals(that: Any) = that match { 
    case cc: ConceptClause => literals.equals(cc.literals)
    case _ => false
  }

  override def hashCode = literals.hashCode

  def getOrdering = ordering

  def without(literal: ConceptLiteral) = 
    new ConceptClause(literals-literal, ordering)

  def without(literals: Iterable[ConceptLiteral]) = 
    new ConceptClause(this.literals--literals, ordering)

  def _with(literal: ConceptLiteral) = 
    new ConceptClause(this.literals+literal, ordering)

  def _with(clause: ConceptClause) = 
    new ConceptClause(this.literals++clause.literals, ordering)


  override def atomicConcepts: Set[String] = 
    literals.flatMap(_.atomicConcepts)

  override def roleSymbols: Set[String] = 
    literals.flatMap(_.roleSymbols)

  override def size = literals.foldLeft(literals.size)(_+_.size)

  override def subConcepts = literals.map(_.subConcepts).reduce(_++_)+this
}

class ConceptClauseOrdering(ordering: Ordering[ConceptLiteral]) extends Ordering[ConceptClause] { 
  override def compare(clause1: ConceptClause, clause2: ConceptClause): Int = { 

    def compareLists(l1: Iterable[ConceptLiteral], l2: Iterable[ConceptLiteral]): Int = 
      // if(l1.size>l2.size)
      // 	1
      // else if(l1.size<l2.size)
      // 	-1
      // else
	compareLists2(l1,l2)
    
    def compareLists2(l1: Iterable[ConceptLiteral], l2: Iterable[ConceptLiteral]): Int = (l1, l2) match { 
      case (Nil, Nil) => 0
      case (_, Nil) => -1
      case (Nil, _) => 1
      case (a::as, b::bs) =>
	val cmp = ordering.compare(a,b)
	if(cmp!=0)
	  cmp
	else
	  compareLists2(as, bs)
    }

    def hasUniversalRestriction(clause: ConceptClause) = 
      clause.literals.collectFirst{ case ConceptLiteral(_, _: UniversalRoleRestriction) => true } != None

    def onlyDefiners(clause: ConceptClause) = clause.literals.forall(x => isDefiner(x.concept))

   if(onlyDefiners(clause1) && !onlyDefiners(clause2))
      -1
    else if(!onlyDefiners(clause1) && onlyDefiners(clause2))
      1
   //  else if(onlyDefiners(clause1) && onlyDefiners(clause2))
   //   compareLists(clause1.literals.toList, clause2.literals.toList)
   //  // else if(hasUniversalRestriction(clause1) && !hasUniversalRestriction(clause2))
   //  //   -1
   //  // else if(!hasUniversalRestriction(clause1) && hasUniversalRestriction(clause2))
   //  //   1
   //  else

    compareLists(clause1.literals.toList, clause2.literals.toList)
  }

  def isDefiner(c: Concept) = c match { 
    case BaseConcept(n) if n.startsWith("_D") => true
    case _ => false
  }
}


object PrefixConceptClauseOrdering extends Ordering[ConceptClause] { 

  override def compare(clause1: ConceptClause, clause2: ConceptClause): Int = { 

    def inner(l1: List[ConceptLiteral], l2: List[ConceptLiteral]): Int = (l1, l2) match { 
      case (Nil, Nil) => 0
      case (_, Nil) => 1
      case (Nil, _) => -1
      case (a::as, b::bs) =>
	val cmp = a.toString.compare(b.toString)
	if(cmp!=0)
	  cmp
	else
	  inner(as, bs)
    }

    inner(clause1.literals.toList, clause2.literals.toList)
  }
}

/***************************************************************************
 *                             Concept Literals                            *
 ***************************************************************************
 */

case class ConceptLiteral(polarity: Boolean, concept: Concept) extends Concept{ 

  def negate = 
    ConceptLiteral(!polarity, concept)

  def convertBack = this match { 
    case ConceptLiteral(true, concept) => concept
    case ConceptLiteral(false, concept) => ConceptComplement(concept)
  }
  
  override def signature = concept.signature
  override def atomicConcepts = concept.atomicConcepts
  override def roleSymbols = concept.roleSymbols

  override def toString = 
    if(polarity)
      "+"+concept.toString
    else
      "-"+concept.toString

  override def size = 2
  override def subConcepts = concept.subConcepts+this
}


class ConceptLiteralOrdering(nonBaseSymbols: Seq[String]) extends Ordering[ConceptLiteral] { 
  
  def index(symbol: String): Int = { 
    assert(nonBaseSymbols.contains(symbol))

    nonBaseSymbols.indexOf(symbol)
  }

  def index(c: Concept): Int = c match { 
    case BaseConcept(name) => index(name)
    case UniversalRoleRestriction(BaseRole(r), _) => index(r)
    case ExistentialRoleRestriction(BaseRole(r), _) => index(r)
    case _ => assert(false); -1
  }

  def next(c: Concept): Concept = c match {
    case BaseConcept(name) => { 
      if(!nonBaseSymbols.contains(name))
	BaseConcept(name+"X")
      else { 
	val i = index(c)+1
	if(nonBaseSymbols.size<i)
	  BaseConcept(nonBaseSymbols(i))
	else
	  BaseConcept(name+"X")
      }
    }
    case UniversalRoleRestriction(BaseRole(r),c) => UniversalRoleRestriction(BaseRole(r+"x"),c)
    case ExistentialRoleRestriction(BaseRole(r),c) => ExistentialRoleRestriction(BaseRole(r+"x"),c)
    case c => assert(false, c.toString); return null
  }

  override def compare(l1: ConceptLiteral, l2: ConceptLiteral): Int = { 

    def nb(c: Concept): Boolean = c match { 
      case b: BaseConcept =>  nonBaseSymbols.contains(b.name)
      case UniversalRoleRestriction(r: BaseRole, _) => nonBaseSymbols.contains(r.name)
      case ExistentialRoleRestriction(r: BaseRole, _) => nonBaseSymbols.contains(r.name)
      case _ => false
    }
    def d(c: Concept): Boolean = c match { 
      case b: BaseConcept => definer(b.name)
      case _ => false
    }
    def q(c: Concept): Boolean = 
      c.isInstanceOf[UniversalRoleRestriction]

    def e(c: Concept): Boolean = 
      c.isInstanceOf[ExistentialRoleRestriction]

    if(nb(l1.concept) && nb(l2.concept)){ 
      val comp1 = index(l1.concept).compareTo(index(l2.concept))
      if(comp1!=0)
	      return comp1
    }

    (l1, l2) match { 
      case (ConceptLiteral(p1, a1), ConceptLiteral(p2, a2)) if a1==a2 => p1.compareTo(p2)
      case (ConceptLiteral(true, a1), ConceptLiteral(_, a2)) if d(a1) && !d(a2) => -1
      case (ConceptLiteral(_, a1), ConceptLiteral(true, a2)) if !d(a1) && d(a2) => 1
      case (ConceptLiteral(true, a1), ConceptLiteral(false, a2)) if d(a1) && d(a2) => -1
      case (ConceptLiteral(false, a1), ConceptLiteral(true, a2)) if d(a1) && d(a2) => 1
      case (ConceptLiteral(_, a1), ConceptLiteral(_, a2)) if nb(a1) && !nb(a2) => -1
      case (ConceptLiteral(_, a1), ConceptLiteral(_, a2)) if !nb(a1) && nb(a2) => 1
//      case (ConceptLiteral(_, a1), ConceptLiteral(_, a2)) if nb(a1) && nb(a2) => index(a1).compareTo(index(a2))
      case (ConceptLiteral(_, a1), ConceptLiteral(_, a2)) if e(a1) && !e(a2) => -1
      case (ConceptLiteral(_, a1), ConceptLiteral(_, a2)) if !e(a1) && e(a2) => 1
      case (ConceptLiteral(_, a1), ConceptLiteral(_, a2)) if q(a1) && !q(a2) => -1
      case (ConceptLiteral(_, a1), ConceptLiteral(_, a2)) if !q(a1) && q(a2) => 1
      case (ConceptLiteral(_, a1), ConceptLiteral(_, a2)) if a1!=a2 => a1.toString.compareTo(a2.toString)
    }

    // (l1.concept, l2.concept) match { 

    //   case (BaseConcept(n1), BaseConcept(n2)) =>
    // 	if(l1.polarity==true && definer(n1)){ 
    // 	  if(l2.polarity==true && definer(n2))
    // 	    n1.compareTo(n2)
    // 	  else 
    // 	    -1
    // 	} else if(l2.polarity==true && definer(n2))
    // 	  1
    // 	  else if(nonBaseSymbols(n1) && !nonBaseSymbols(n2))
    // 	    -1
    // 	  else if(!nonBaseSymbols(n1) && nonBaseSymbols(n2))
    // 	    1
    // 	  else if(n1.compareTo(n2)!=0)
    // 	    n1.compareTo(n2)
    // 	  else if(l1.polarity==false && l2.polarity==true)
    // 	    -1
    // 	  else if(l1.polarity==true && l2.polarity==false)
    // 	    1
    // 	  else
    // 	    0
    //   case (_: BaseConcept, _) => -1
    //   case (_, _: BaseConcept) => 1
    //   case (_, _) => l1.toString.compareTo(l2.toString)
    // }
  }

  def definer(symbol: String) =
    symbol.startsWith("_D")
}


/**
 * Convenience singleton to make clause creation simpler
 */
object SimpleLiteralOrdering extends ConceptLiteralOrdering(Seq()) 


trait ConceptGenerator { 
  def newConcept(): BaseConcept
  def newConcept(derivationCandidate: Boolean): BaseConcept

}
