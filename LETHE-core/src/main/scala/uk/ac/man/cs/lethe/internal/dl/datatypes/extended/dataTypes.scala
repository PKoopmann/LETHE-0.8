package uk.ac.man.cs.lethe.internal.dl.datatypes.extended

import uk.ac.man.cs.lethe.internal.dl.datatypes._
import uk.ac.man.cs.lethe.internal.tools.MultiSet

/**
 * Additional ontology datatypes relevant in the context of abduction:
 *  negated role assertions and fixpoint point concepts.
 */

////////////////////
// Boolean ABoxes
////////////////////
case class DisjunctiveAssertion(disjuncts: Set[Assertion]) extends Assertion {
  override def signature: Set[String] = disjuncts.flatMap(_.signature)

  override def atomicConcepts: Set[String] = disjuncts.flatMap(_.atomicConcepts)

  override def roleSymbols: Set[String] = disjuncts.flatMap(_.roleSymbols)

  override def size: Int = disjuncts.toSeq.map(_.size).sum// + disjuncts.size-1

  override def subConcepts: MultiSet[Concept] =
    disjuncts.map(_.subConcepts).foldLeft(MultiSet[Concept])(_++_)
}

case class ConjunctiveAssertion(conjuncts: Set[Assertion]) extends Assertion {
  override def signature: Set[String] = conjuncts.flatMap(_.signature)

  override def atomicConcepts: Set[String] = conjuncts.flatMap(_.atomicConcepts)

  override def roleSymbols: Set[String] = conjuncts.flatMap(_.roleSymbols)

  override def size: Int = conjuncts.toSeq.map(_.size).sum// + conjuncts.size-1

  override def subConcepts: MultiSet[Concept] =
    conjuncts.map(_.subConcepts).foldLeft(MultiSet[Concept])(_++_)
}

case class NegatedRoleAssertion(role: BaseRole,
                                individual1: Individual,
                                individual2: Individual) extends Assertion {
  override def signature: Set[String] = Set(role.name)

  override def atomicConcepts: Set[String] = Set()

  override def roleSymbols: Set[String] = Set(role.name)

  override def size: Int = 3

  override def subConcepts: MultiSet[Concept] = MultiSet()

  override def toString =
    "-"+role.toString+"("+individual1.toString+", "+ individual2.toString+")"
}


///////////////////
// Boolean KBs
///////////////////
case class DisjunctiveDLStatement(statements: Set[DLStatement]) extends DLStatement  {
  override def signature: Set[String] = statements.flatMap(_.signature)
  override def atomicConcepts: Set[String] = statements.flatMap(_.atomicConcepts)
  override def roleSymbols: Set[String] = statements.flatMap(_.roleSymbols)
  override def size: Int = statements.toSeq.map(_.size).sum// + statements.size - 1
  override def subConcepts =
    statements.map(_.subConcepts).foldLeft(MultiSet[Concept])(_++_)
  override def toString =
    statements.mkString("(", " OR ", ")")
}

case class ConjunctiveDLStatement(statements: Set[DLStatement]) extends DLStatement  {
  override def signature: Set[String] = statements.flatMap(_.signature)
  override def atomicConcepts: Set[String] = statements.flatMap(_.atomicConcepts)
  override def roleSymbols: Set[String] = statements.flatMap(_.roleSymbols)
  override def size: Int = statements.toSeq.map(_.size).sum// + statements.size-1
  override def subConcepts =
    statements.map(_.subConcepts).foldLeft(MultiSet[Concept])(_++_)
  override def toString =
    statements.mkString("(", " AND ", ")")
}

//////////////////////
// FIXPOINTS
//////////////////////
class ConceptVariable(override val name: String) extends BaseConcept(name) {
  override def signature = Set()
  override def atomicConcepts = Set()
}

object ConceptVariable {
  var counter = 0
  def fresh = {
    counter += 1
    new ConceptVariable("X"+counter)
  }
}

case class GreatestFixpoint(variable: ConceptVariable, concept: Concept) extends Concept {
  override def signature: Set[String] = concept.signature

  override def atomicConcepts: Set[String] = concept.signature

  override def roleSymbols: Set[String] = concept.signature

  override def size: Int = concept.size +2

  override def subConcepts: MultiSet[Concept] = concept.subConcepts + this

  override def toString() = "GFP."+variable+".("+concept+")"
}

case class LeastFixpoint(variable: ConceptVariable, concept: Concept) extends Concept {
  override def signature: Set[String] = concept.signature

  override def atomicConcepts: Set[String] = concept.signature

  override def roleSymbols: Set[String] = concept.signature

  override def size: Int = concept.size +2

  override def subConcepts: MultiSet[Concept] = concept.subConcepts + this

  override def toString() = "LFP."+variable+".("+concept+")"
}
