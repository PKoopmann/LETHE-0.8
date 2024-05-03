
package uk.ac.man.cs.lethe.internal.dl.forgetting.direct


import scala.collection.immutable.SortedSet
import scala.collection.mutable.{ HashMap, Map }
import scala.math

import com.typesafe.scalalogging.Logger

import uk.ac.man.cs.lethe.internal.dl.datatypes._

object SubsumptionTreeLogger 

class SubsumptionTree(val rootElement: ConceptLiteral) { 

//  implicit val (logger, formatter, appender) = ZeroLoggerFactory.newLogger(SubsumptionTreeLogger)

  val logger = Logger[SubsumptionTree]

  def this() = this(null)

  var children:Map[ConceptLiteral, SubsumptionTree] = HashMap()

  var size = 0
  
  /**
   * Store clause's literals in internal data structure 
   */
  def add(clause: ConceptClause) = 
      addLiterals(clause.literals)


  def addLiterals(literals: Set[ConceptLiteral]): Unit = { 
    val old = deeperRepresentation

    size += 1

    if(literals.isEmpty)
      ()
    else
      literals.foreach{ literal => addChild(literal, literals-literal) }

    assert(children.values.forall(_.size<=size), "added " + literals.toString + " to " + old +"\ngot "+deeperRepresentation + " ("+clauses.toString+")")

    assert(literals.isEmpty || clauses.contains(literals), "added " + literals.toString + " to " + old +"\ngot "+deeperRepresentation + " ("+clauses.toString+")")
  }

  def addChild(literal: ConceptLiteral, literals: Set[ConceptLiteral]): Unit = { 
    if(!children.contains(literal))
      children.put(literal, new SubsumptionTree(literal))
    children(literal).addLiterals(literals)
  }


  /**
   * check whether the clauses added to this tree subsume the given clause
   */
  def subsume(clause: ConceptClause) =
    subsumeLiterals(clause.literals)

  def subsumeLiterals(literals: SortedSet[ConceptLiteral]): Boolean =
    if(children.isEmpty)
      true
    else if(literals.isEmpty)
      size > children.size
    else children.get(literals.head) match { 
      case Some(tree) => tree.subsumeLiterals(literals.tail)
      case None => false
    }


  /**
   * return all clauses subsumed by the given clause
   */
  def subsumedBy(clause: ConceptClause) = 
    getTails(clause.literals).map{ (literals: Set[ConceptLiteral]) => new ConceptClause(literals++clause.literals, clause.getOrdering) }

  /**
   * return all tails of clauses containing the given literals (that means without the given literals)
   */
  def getTails(literals: SortedSet[ConceptLiteral]): Set[Set[ConceptLiteral]] = { 
    if(literals.isEmpty){ 
      clauses
    } else children.get(literals.head) match { 
      case None => Set()
      case Some(tree) => tree.getTails(literals.tail)
    }
  }

  /**
   * return all clauses represented in this tree and subsets of these
   */
  def clauses: Set[Set[ConceptLiteral]] = 
    if(size==1)
      Set(children.keySet.toSet[ConceptLiteral])
    else {
      var result = children.keySet.flatMap{ literal => 
	children(literal).clauses.map{ clause => 
	  clause + literal
        }
      }.toSet[Set[ConceptLiteral]] 
      if(size>children.size)
	result += Set(rootElement)
      result
    }


  /**
   * Remove this clause and everything that is subsumed by it from
   * the tree.
   * (Assumption: There is at least one such clause)
   */
  def remove(clause: ConceptClause) = { 

    removeLiterals(clause.literals)

  }

  def removeLiterals(literals: Set[ConceptLiteral]): Int = { 
    val old = deeperRepresentation

    var removed = 0
    children.keySet.foreach { key =>
      if(literals.contains(key)) { 
	removed = math.max(removed, children(key).size)
	children.remove(key)
      } else { 
	removed = math.max(removed, children(key).removeLiterals(literals))
      }
    }			   

    size -= removed

    if(size==0)
      children.clear()

    val result = removed

    // val result = try { 
    // if(literals.isEmpty) { 
    //   size-=1
    //   1
    // } else if(literals.size==1) { 
    //   val removed = children(literals.head).size
    //   size -= removed
    //   children.remove(literals.head)
    //   removed
    // }
    // else { 
    //   var removed = 0
    //   literals.foreach{ literal => 
    // 	if(children(literal).size==1) { 
    // 	  removed = math.max(removed, 1)
    // 	  children.remove(literal)
    // 	}
    // 	else
    // 	  removed = math.max(removed, children(literal).removeLiterals(literals-literal))
    //   }
    //   size -= removed
    //   removed
    // }
    // } catch { 
    //   case e => 
    // 	println(deeperRepresentation)
    // 	e.printStackTrace()
    // 	exit(1)
    // }

    assert(!clauses.contains(literals), "removed "+ literals.toString + " from " + deeperRepresentation)
    assert(children.values.forall(_.size<=size), "removed " + literals.toString + " from " + old +"\ngot "+deeperRepresentation + " ("+clauses.toString+")")
    assert(children.isEmpty || size>0, "removed " + literals.toString + " from " + old +"\ngot "+deeperRepresentation + " ("+clauses.toString+")")
    result
  }
  
  def deepRepresentation: String = 
    rootElement.toString + size.toString + " -> " + children.values.map(x => x.rootElement.toString+", " + x.size.toString + " -> " + x.clauses.mkString("[", ", ", "]")).mkString("{", ", ","}")
    
  def deeperRepresentation: String = 
    (if(rootElement==null) "" else rootElement.toString + ", ") + size.toString + " -> " + children.values.map(_.deeperRepresentation).mkString("[", ", ","]")
    
  
  def isEmpty = 
    children.isEmpty
}
