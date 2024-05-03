package uk.ac.man.cs.lethe.internal.dl.abduction.forgetting

import com.typesafe.scalalogging.Logger
import uk.ac.man.cs.lethe.internal.dl.datatypes.extended.NegatedRoleAssertion
import uk.ac.man.cs.lethe.internal.dl.datatypes.extended.{DisjunctiveAssertion, ExtendedABoxClause, NegatedRoleAssertion}
import uk.ac.man.cs.lethe.internal.dl.datatypes.{ABox, Assertion, BaseConcept, BaseRole, Concept, ConceptAssertion, ConceptConjunction, ConceptDisjunction, DLStatement, Individual, InverseRole, RoleAssertion, Subsumption, TopConcept, UniversalRoleRestriction}
import uk.ac.man.cs.lethe.internal.dl.forgetting.direct.{ALCFormulaPreparations, ConceptClause, ConceptLiteral, SimpleDefinerEliminator, SimpleLiteralOrdering}

import scala.collection.mutable
import scala.collection.mutable.HashSet


object ExtendedABoxClausification {

  val logger = Logger(ExtendedABoxClausification.getClass)

  def clausify(abox: ABox,
               ordering: Ordering[ConceptLiteral] = SimpleLiteralOrdering)
  : (Set[ExtendedABoxClause], Set[ConceptClause]) = {
    clausify(abox.assertions, ordering)
  }

  def clausify(assertions: Set[Assertion])
  : (Set[ExtendedABoxClause], Set[ConceptClause]) =
    clausify(assertions, SimpleLiteralOrdering)

  def clausify(assertions: Set[Assertion],
               ordering: Ordering[ConceptLiteral])
  : (Set[ExtendedABoxClause], Set[ConceptClause]) = {

    var conceptClauses = new HashSet[ConceptClause]()
    var aboxClauses = new HashSet[ExtendedABoxClause]()

    assertions.foreach{ a=>
      a match {
        case r: RoleAssertion => aboxClauses +=
          ExtendedABoxClause.from(r)
        case nr: NegatedRoleAssertion => aboxClauses += ExtendedABoxClause.from(nr)
        case ConceptAssertion(TopConcept, _) => ;
        case ConceptAssertion(concept, individual) => {
          val nnf = ALCFormulaPreparations.nnf(concept)
          val replaced::definitions = ALCFormulaPreparations.replaceFillers(nnf)
          definitions.flatMap(clauses(_,ordering)).foreach(conceptClauses.add)

          clauses(replaced, ordering).foreach{ cl =>
            aboxClauses.add(new ExtendedABoxClause(Map(individual -> cl),
              Set[RoleAssertion](), Set[RoleAssertion]())) }
        }
        case DisjunctiveAssertion(ds) => {
          var clause = new ExtendedABoxClause()
          var cnfConceptAssertions = Set[(Individual, Set[ConceptClause])]()
          var roleAssertions = Set[RoleAssertion]()
          var negatedRoleAssertions = Set[RoleAssertion]()
          // flatten
          val flattened = ds.flatMap(_ match {
            case DisjunctiveAssertion(ds) => ds
            case other => Set(other)
          })

          flattened.foreach{ _ match {
            case ConceptAssertion(concept, individual) => {
              val nnf = ALCFormulaPreparations.nnf(concept)
              val replaced::definitions = ALCFormulaPreparations.replaceFillers(nnf)

              definitions.flatMap(clauses(_,ordering)).foreach(conceptClauses.add)

              val genClauses = clauses(replaced, ordering)
              cnfConceptAssertions += (individual -> genClauses)
            }
            case ra: RoleAssertion => roleAssertions+=ra
            case NegatedRoleAssertion(r, a, b) => negatedRoleAssertions+=RoleAssertion(r,a,b)
          }}
          flatten(cnfConceptAssertions,
            roleAssertions,negatedRoleAssertions).foreach(aboxClauses.add)
        }
      }}

    (aboxClauses.toSet, conceptClauses.toSet)
  }

  def flatten(maps: Set[(Individual, Set[ConceptClause])], ra: Set[RoleAssertion], nra: Set[RoleAssertion])
  : Set[ExtendedABoxClause] =
    if(maps.isEmpty)
      Set(new ExtendedABoxClause(Map[Individual,ConceptClause](), ra, nra))
    else {
      val (ind, clauses) = maps.head
      clauses.flatMap{ clause =>
        flatten(maps.tail, ra, nra).map{ aboxClause =>
          aboxClause.combineWith(new ExtendedABoxClause(Map((ind->clause)), ra, nra))
          //new ExtendedABoxClause(aboxClause.literals + (ind->clause), ra, nra, temp=true)}
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

}
