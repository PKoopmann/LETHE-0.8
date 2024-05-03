package uk.ac.man.cs.lethe.internal.dl.forgetting.direct

import scala.collection.mutable.HashMap
import uk.ac.man.cs.lethe.internal.dl.datatypes._
import com.typesafe.scalalogging.Logger
import uk.ac.man.cs.lethe.internal.dl.datatypes.extended.{ConjunctiveAssertion, DisjunctiveAssertion, NegatedRoleAssertion}
import uk.ac.man.cs.lethe.internal.dl.forgetting.direct.SymbolOrderings.countOccurrences
import uk.ac.man.cs.lethe.internal.tools.MultiSet

trait SymbolOrdering {
  def order(symbols: Iterable[String], ontology: Ontology): Seq[String]
}

class SymbolOrderingByNumberOfOccurrences extends SymbolOrdering {
  override def order(symbols: Iterable[String], ontology: Ontology) =
    AdvancedSymbolOrderings.orderByNumOfOccurrences(symbols, ontology).reverse
}

class OrderingConceptsFirst(ordering: SymbolOrdering, roleNames: Set[String]) extends SymbolOrdering {
  override def order(symbols: Iterable[String], ontology: Ontology) = {
    ordering.order(
      symbols, ontology
    ).sortWith((a,b) =>
      (!roleNames(a) && roleNames(b))
    )
  }
}

object AdvancedSymbolOrderings {

  def maxByNumOfOccurrences(symbols: Iterable[String], ontology: Ontology): Seq[String] = {
    val pairs = countOccurrences(symbols.toSet, ontology)


    var sorted = pairs.toSeq.sortWith((a, b) => a._2 > b._2)

    sorted.map(_._1)
  }

  def orderByNumOfOccurrences(symbols: Iterable[String], ontology: Ontology): Seq[String] = {
    val pairs = countOccurrences(symbols.toSet, ontology)

    //println(pairs)

    var sorted = pairs.toSeq.sortWith((a, b) => a._2 > b._2)

    sorted.map(_._1)
  }

  object Polarity extends Enumeration {
    class Val() extends super.Val{
      var neg: Polarity = _
    }

    type Polarity = Val
    val Positive: Val = new Val()
    val Negative: Val = new Val()
    val Both: Val = new Val()

    Positive.neg=Negative
    Negative.neg=Positive
    Both.neg=Both
  }
  import Polarity.Polarity

  def countOccurrences(symbols: Set[String], ontology: Ontology) = {
    val countersPos = new MultiSet[String]()
    val countersNeg = new MultiSet[String]()

    def add(symbol: String, polarity: Polarity) = {
      if(symbols(symbol))
        polarity match {
          case Polarity.Positive => countersPos.add(symbol)
          case Polarity.Negative => countersNeg.add(symbol)
          case Polarity.Both => countersPos.add(symbol); countersNeg.add(symbol)
        }
    }

    def countOccurrences(concept: Concept, polarity: Polarity): Unit = {
      concept match {
        case BaseConcept(n) => add(n,polarity)
        case BaseConcept(_) => ;
        case ConceptComplement(c) => countOccurrences(c, polarity.neg)
        case ConceptConjunction(cs) => cs.foreach(countOccurrences(_,polarity))
        case ConceptDisjunction(ds) => ds.foreach(countOccurrences(_,polarity))
        case ExistentialRoleRestriction(r, c) => {
          // we assume role names have only positive polarity here
          r.roleSymbols.foreach(add(_, polarity))
          countOccurrences(c,polarity)
        }
        case UniversalRoleRestriction(r, c) => {
          // we assume role names have only negative polarity here
          r.roleSymbols.foreach(add(_, polarity.neg))
          countOccurrences(c, polarity)
        }
        case MinNumberRestriction(n,r,c) =>
          assert(r.equals(TopRole) || r.signature.size==1)
          if(!r.equals(TopRole)) 
            add(r.signature.head, polarity)
          countOccurrences(c,polarity)
        case MaxNumberRestriction(n,r,c) =>
          assert(r.equals(TopRole) || r.signature.size==1)
          if(!r.equals(TopRole))
            add(r.signature.head, polarity.neg)
          countOccurrences(c,polarity)
        case _ => ;
      }
    }

    def countOccurrencesAxiom(dlStatement: DLStatement): Unit = {
      dlStatement match {
        case Subsumption(c1, c2) => countOccurrences(c1, Polarity.Negative); countOccurrences(c2, Polarity.Positive)
        case ConceptEquivalence(c1, c2) => countOccurrences(c1, Polarity.Both); countOccurrences(c2, Polarity.Both)
        case ConceptAssertion(c, _) => countOccurrences(c, Polarity.Positive)
        case RoleAssertion(r, _, _) if !r.equals(TopRole) => add(r.signature.head, Polarity.Positive)
        case RoleSubsumption(r1, r2) => {
          if(!r1.equals(TopRole))
            add(r1.signature.head, Polarity.Negative)
          if(!r2.equals(TopRole))
            add(r2.signature.head, Polarity.Positive)
        }
        case TransitiveRoleAxiom(BaseRole(r)) =>
          add(r, Polarity.Both)
        case DisjunctiveAssertion(assertions) => assertions.foreach(countOccurrencesAxiom)
        case ConjunctiveAssertion(assertions) => assertions.foreach(countOccurrencesAxiom)
        case NegatedRoleAssertion(r, _, _) if !r.equals(TopRole) => add(r.signature.head, Polarity.Negative)
      }
    }


    ontology.statements.foreach(countOccurrencesAxiom)

    symbols.map{ symbol =>
      (symbol, countersNeg(symbol)*countersPos(symbol))
    }.toMap
  }
}

object SymbolOrderings {
  def orderByNumOfOccurrences(symbols: Iterable[String], ontology: Ontology): Seq[String] = {
    val pairs = countOccurrences(symbols.toSet, ontology)

    var sorted = pairs.toSeq.sortWith((a, b) => a._2 > b._2)

    sorted.map(_._1)
  }

  def countOccurrences(symbols: Set[String], ontology: Ontology) = {
    val counters = new HashMap[String, Int]()

    def increase(symbol: String) =
      if(!counters.contains(symbol))
        counters.put(symbol, 1)
      else
        counters.put(symbol, counters(symbol)+1)


    def countOccurrences(concept: Concept): Unit = concept match {
      case BaseConcept(n) if(symbols(n)) => increase(n)
      case BaseConcept(_) => ;
      case ConceptComplement(c) => countOccurrences(c)
      case ConceptConjunction(cs) => cs.foreach(countOccurrences)
      case ConceptDisjunction(ds) => ds.foreach(countOccurrences)
      case ExistentialRoleRestriction(r, c) => {
        assert(r.equals(TopRole) || r.signature.size==1)
        if(!r.equals(TopRole) && symbols(r.signature.head))
          increase(r.signature.head)
        countOccurrences(c)
      }
      case UniversalRoleRestriction(r, c) => {
        assert(r.equals(TopRole) || r.signature.size==1)
        if(!r.equals(TopRole) && symbols(r.signature.head))
          increase(r.signature.head)
        countOccurrences(c)
      }
      case MinNumberRestriction(n,r,c) =>
        assert(r.equals(TopRole) || r.signature.size==1)
        if(!r.equals(TopRole) && symbols(r.signature.head))
          increase(r.signature.head)
        countOccurrences(c)
      case MaxNumberRestriction(n,r,c) =>
        if(!r.equals(TopRole) && symbols(r.signature.head))
          increase(r.signature.head)
        countOccurrences(c)
      case _ => ;
    }

    ontology.statements.foreach{ _ match {
      case Subsumption(c1, c2) => countOccurrences(c1); countOccurrences(c2)
      case ConceptEquivalence(c1, c2) => countOccurrences(c1); countOccurrences(c2)
      case ConceptAssertion(c, _) => countOccurrences(c)
      case RoleAssertion(r,_,_) if !r.equals(TopRole) => if(symbols(r.signature.head)) increase(r.signature.head)
      case RoleSubsumption(r1, r2) => {
        if(!r1.equals(TopRole) && symbols(r1.signature.head)) increase(r1.signature.head)
        if(!r2.equals(TopRole) && symbols(r2.signature.head)) increase(r2.signature.head)
      }
      case TransitiveRoleAxiom(BaseRole(r)) =>
        if(symbols(r)) increase(r)
    }}

    counters.toSet[(String, Int)]
  }

  def countOccurrences(symbol: String, ontology: Ontology): Int = {

    def countOccurrences(concept: Concept): Int = concept match {
      case BaseConcept(n) if n==symbol => 1
      case ConceptComplement(c) => countOccurrences(c)
      case ConceptConjunction(cs) => cs.map(countOccurrences).fold(0)((x,y) => x+y)
      case ConceptDisjunction(ds) => ds.map(countOccurrences).fold(0)((x,y) => x+y)
      case ExistentialRoleRestriction(r, c) => {
        if(!r.equals(TopRole) && symbol==r.signature.head)
          1+countOccurrences(c)
        countOccurrences(c)
      }
      case UniversalRoleRestriction(r, c) => {
        if(!r.equals(TopRole) && symbol==r.signature.head)
          1+countOccurrences(c)
        countOccurrences(c)
      }
      case _ => 0
    }

    var result = 0
    ontology.statements.foreach{ _ match {
      case Subsumption(c1, c2) => result +=countOccurrences(c1)+countOccurrences(c2)
      case ConceptEquivalence(c1, c2) => result += countOccurrences(c1)+countOccurrences(c2)
      case ConceptAssertion(c,_) => result += countOccurrences(c)
      case RoleAssertion(r,_,_) => if(!r.equals(TopRole) && symbol==r.signature.head) result +=1
      case RoleSubsumption(r1,r2) => {
        if(!r1.equals(TopRole) && symbol==r1.signature.head) result +=1
        if(!r2.equals(TopRole) && symbol==r2.signature.head) result +=1
      }
      case TransitiveRoleAxiom(BaseRole(r)) => {
        if(symbol==r)
          result +=1
      }
    }}

    result
  }
}
