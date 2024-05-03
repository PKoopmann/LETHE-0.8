package uk.ac.man.cs.lethe.internal.dl.datatypes

import uk.ac.man.cs.lethe.internal.dl.datatypes.extended.{ConjunctiveDLStatement, DisjunctiveDLStatement, GreatestFixpoint, LeastFixpoint}

class Substitution(replace: Concept, replaceBy: Concept) {

  def apply(statement: DLStatement): DLStatement = statement match {
    case o: Ontology => new Ontology(
      apply(o.tbox).asInstanceOf[TBox],
      apply(o.abox).asInstanceOf[ABox],
      apply(o.rbox).asInstanceOf[RBox])
    case TBox(axioms) => TBox(axioms.map(apply(_).asInstanceOf[Axiom]))
    case r: RBox => r
    case ABox(assertions) => ABox(assertions.map(apply(_).asInstanceOf[Assertion]))
    case DisjunctiveDLStatement(statements) => DisjunctiveDLStatement(statements.map(apply))
    case ConjunctiveDLStatement(statements) => ConjunctiveDLStatement(statements.map(apply))
    case Subsumption(c1, c2) => Subsumption(apply(c1), apply(c2))
    case ConceptEquivalence(c1,c2) => ConceptEquivalence(apply(c1), apply(c2))
    case ConceptAssertion(c,i) => ConceptAssertion(apply(c),i)
    case ra: RoleAssertion => ra
    case r: RoleAxiom => r
    case DisjunctiveConceptAssertion(cas) =>
      DisjunctiveConceptAssertion(cas.map(apply(_).asInstanceOf[ConceptAssertion]))
    case _ => throw new Exception("Not supported yet: "+statement)
  }

  def apply(concept: Concept): Concept =
    if(concept.equals(replace))
      return replaceBy
    else concept match {
      case TopConcept => TopConcept
      case BottomConcept => BottomConcept
      case b: BaseConcept => b
      case ConceptComplement(c) => ConceptComplement(apply(c))
      case ConceptConjunction(cs) => ConceptConjunction(cs.map(apply))
      case ConceptDisjunction(ds) => ConceptDisjunction(ds.map(apply))
      case n: NominalSet => n
      case ExistentialRoleRestriction(r,c) => ExistentialRoleRestriction(r,apply(c))
      case UniversalRoleRestriction(r,c) => UniversalRoleRestriction(r,apply(c))
      case MinNumberRestriction(n,r,c) => MinNumberRestriction(n,r,apply(c))
      case MaxNumberRestriction(n,r,c) => MaxNumberRestriction(n,r,apply(c))
      case EqNumberRestriction(n,r,c) => EqNumberRestriction(n,r,apply(c))
      case LeastFixpoint(v,c) => LeastFixpoint(v,apply(c))
      case GreatestFixpoint(v,c) => GreatestFixpoint(v,apply(c))
    }
}
