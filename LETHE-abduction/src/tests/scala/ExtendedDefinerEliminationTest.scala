//package tests
//
//import uk.ac.man.cs.lethe.internal.dl.abduction.forgetting.{DisjunctiveAssertion, ExtendedABoxClause, ExtendedABoxForgetter, NegatedRoleAssertion}
//import uk.ac.man.cs.lethe.internal.dl.datatypes.{BaseConcept, BaseRole, ConceptAssertion, ConceptComplement, ExistentialRoleRestriction, Individual, NominalSet, Ontology, Subsumption, TopConcept, UniversalRoleRestriction}
//import uk.ac.man.cs.lethe.internal.dl.forgetting.direct.{ConceptClause, ConceptLiteral}
//import uk.ac.man.cs.lethe.internal.tools.formatting.SimpleDLFormatter
//
//object ExtendedDefinerEliminationTest extends LogicalTests {
//
//  def main(args: Array[String]): Unit = {
//    test1()
//    test2()
//  }
//
//  def test2() = {
//    val ontology = new Ontology()
//
//    ontology.addStatements(Set(
//      ConceptAssertion(BaseConcept("A"), Individual("a")),
//      ConceptAssertion(BaseConcept("B"), Individual("b")),
//      Subsumption(BaseConcept("A"),
//        UniversalRoleRestriction(BaseRole("r"),
//          UniversalRoleRestriction(BaseRole("s"),
//          ConceptComplement(BaseConcept("B"))
//      )))
//    ))
//
//    val forgettingResult = ExtendedABoxForgetter.forget(ontology, Set("A", "B"), new Ontology())
//
//    println(SimpleDLFormatter.format(forgettingResult))
//
//    val expected = new Ontology()
//    expected.addStatement(ConceptAssertion(
//      UniversalRoleRestriction(BaseRole("r"),
//        UniversalRoleRestriction(BaseRole("s"),
//        ConceptComplement(NominalSet(Set(Individual("b")))))),
//      Individual("a")
//    ))
//
//    assertEquivalent(forgettingResult, expected)
//  }
//
//  def test1() = {
//    val ontology = new Ontology()
//
//    ontology.addStatements(Set(
//      ConceptAssertion(ExistentialRoleRestriction(BaseRole("t"), BaseConcept("D2")),
//        Individual("c")),
//      DisjunctiveAssertion(Set(
//        NegatedRoleAssertion(BaseRole("r"), Individual("a"), Individual("b")),
//        ConceptAssertion(ConceptComplement(BaseConcept("D1")), Individual("a")),
//        ConceptAssertion(UniversalRoleRestriction(BaseRole("r"), BaseConcept("D2")), Individual("b"))
//      )),
//      Subsumption(BaseConcept("D2"), UniversalRoleRestriction(BaseRole("s"), BaseConcept("D1")))
//    ))
//
//    val forgettingResult = ExtendedABoxForgetter.forget(ontology, Set("D1", "D2", "D3"), new Ontology())
//
//    println(SimpleDLFormatter.format(forgettingResult))
//
//    val expected = new Ontology()
//    expected.addStatement(ConceptAssertion(ExistentialRoleRestriction(BaseRole("t"), TopConcept), Individual("c")))
//
//    assertEquivalent(forgettingResult, expected)
//  }
//}
