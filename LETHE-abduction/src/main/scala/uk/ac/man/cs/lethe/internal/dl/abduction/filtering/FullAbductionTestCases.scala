//package uk.ac.man.cs.lethe.internal.dl.abduction.filtering;
//
//package tests
//
//import uk.ac.man.cs.lethe.internal.dl.abduction.BasicAbducer
//import uk.ac.man.cs.lethe.internal.dl.abduction.forgetting.{ConceptVariable, ConjunctiveAssertion, ConjunctiveDLStatement, DisjunctiveAssertion, DisjunctiveDLStatement, ExtendedABoxForgetter, GreatestFixpoint, LeastFixpoint}
//import uk.ac.man.cs.lethe.internal.dl.datatypes.{BaseConcept, BaseRole, BottomConcept, ConceptAssertion, ConceptComplement, ConceptConjunction, ConceptDisjunction, ConceptEquivalence, DLStatement, ExistentialRoleRestriction, Individual, InverseRole, Ontology, RoleAssertion, Subsumption, TopConcept, TopRole, UniversalRoleRestriction}
//import uk.ac.man.cs.lethe.internal.dl.owlapi.OWLExporter
//import uk.ac.man.cs.lethe.internal.fol.datatypes.Equivalence
//import uk.ac.man.cs.lethe.internal.tools.formatting.{SimpleDLFormatter, SimpleOWLFormatter}
//import uk.ac.man.cs.lethe.logicalDifference.LogicalEntailmentCheckerClassical
//
//import scala.collection.JavaConverters._
//
//object FullAbductionTestCases {
//
//  def main(args: Array[String]) = {
//    /*
//    testSimpleCI()
//    testIgnoreBackground()
//    testTBoxIgnoreBackground()
//    testDisjunctiveConceptIgnoreBackground()
//    testInverseRoles()
//
//    testRoleAssertionIntroduction()
//    testHypothesisOtherIndividual()
//    testConjunctionOfAssertions()
//    testDisjunctionOfAssertions()
//    testRoleAssertionHypothesis()
//    testMixedHypothesis()
//    testRoleForgetting()
//    testRoleAssertionResolution()
//    testUniversalRoleRoleAssertion()
//    testPullingDefiners()
//    testTwoTBoxObservation()
//
//     */
//
//    testDisjunctiveHypothesisRedundant()
//    testDisjunctiveHypothesisRedundantEquivalence()
//    testDisjunctiveTBoxHypothesisRedundant()
//    testDisjunctiveTBoxHypothesisNotRedundant()
//
//    /*
//    testNumberOfDisjunctions()
//    testNumberOfDisjunctionsTBoxABox()
//    testNumberOfDisjunctionsTBoxABox2()
//
//    testInverseRolesFixpoint()
//    testFixpointRedundancy()
//
//     */
//  }
//
//  def testSimpleCI() = {
//    val backgroundOntology = Ontology.buildFrom(Set(
//      Subsumption(BaseConcept("A"), BaseConcept("B")) // A <= B
//    ))
//
//    println("Background: ")
//    println(SimpleDLFormatter.format(backgroundOntology))
//    println()
//
//    val abducibles = Set("A")
//
//    println("Abducibles: ")
//    println(abducibles.mkString("{", ", ", "}"))
//    println()
//
//    val observation = Ontology.buildFrom(Set(
//      ConceptAssertion(BaseConcept("B"), Individual("a")) // B(a)
//    ))
//
//    println("Observation: ")
//    println(SimpleDLFormatter.format(observation))
//    println()
//
//    val abducer = new FullAbducer()
//    abducer.setBackgroundOntology(backgroundOntology)
//    abducer.setAbducibles(abducibles)
//
//    val forgettingResult = abducer.forget(observation)
//    val forgettingResultSimplified = abducer.simplify(forgettingResult)
//    val hypothesis = abducer.filter(forgettingResultSimplified)
//
//    println("Hypothesis:")
//    println(SimpleDLFormatter.format(hypothesis))
//
//    val hypothesisDisjuncts = hypothesis match {
//      case DisjunctiveDLStatement(ds) => ds.size
//      case other => 1
//    }
//
//    println("Hypothesis size: " + hypothesisDisjuncts)
//
//    val correctHypothesis =
//      ConceptAssertion(BaseConcept("A"), Individual("a")) // A(a)
//
//    println("Expected Hypothesis:")
//    println(SimpleDLFormatter.format(correctHypothesis))
//
//    hypothesis match {
//      case DisjunctiveDLStatement(disjuncts) if disjuncts.size==1 =>
//        assertEquivalent(disjuncts.head, correctHypothesis)
//      case other => assert(false, "Unexpected shape of hypothesis: "+hypothesis)
//    }
//  }
//
//  def testIgnoreBackground() = {
//    val backgroundOntology = Ontology.buildFrom(Set(
//      Subsumption(BaseConcept("A"), BaseConcept("B")), // A <= B
//      Subsumption(BaseConcept("B"), BaseConcept("C")) // B <= C
//    ))
//
//    println("Background: ")
//    println(SimpleDLFormatter.format(backgroundOntology))
//    println()
//
//    val abducibles = Set("A", "C")
//
//    println("Abducibles: ")
//    println(abducibles.mkString("{", ", ", "}"))
//    println()
//
//    val observation = Ontology.buildFrom(Set(
//      ConceptAssertion(BaseConcept("B"), Individual("a")) // B(a)
//    ))
//
//    println("Observation: ")
//    println(SimpleDLFormatter.format(observation))
//    println()
//
//    val abducer = new FullAbducer()
//    abducer.setBackgroundOntology(backgroundOntology)
//    abducer.setAbducibles(abducibles)
//
//    val forgettingResult = abducer.forget(observation)
//    val forgettingResultSimplified = abducer.simplify(forgettingResult)
//    val hypothesis = abducer.filter(forgettingResultSimplified)
//
//    println("Hypothesis:")
//    println(SimpleDLFormatter.format(hypothesis))
//
//    val hypothesisDisjuncts = hypothesis match {
//      case DisjunctiveDLStatement(ds) => ds.size
//      case other => 1
//    }
//
//    println("Hypothesis size: " + hypothesisDisjuncts)
//
//    val correctHypothesis =
//      ConceptAssertion(BaseConcept("A"), Individual("a")) // A(a)
//
//    println("Expected Hypothesis:")
//    println(SimpleDLFormatter.format(correctHypothesis))
//
//
//    hypothesis match {
//      case DisjunctiveDLStatement(disjuncts) if disjuncts.size==1 =>
//        assertEquivalent(disjuncts.head, correctHypothesis)
//      case other => assert(false, "Unexpected shape of hypothesis: "+hypothesis)
//    }
//  }
//
//  def testTBoxIgnoreBackground() = {
//    val backgroundOntology = Ontology.buildFrom(Set(
//      Subsumption(BaseConcept("A"), BaseConcept("B")), // A <= B
//      Subsumption(BaseConcept("B"), BaseConcept("C")) // B <= C
//    ))
//
//    println("Background: ")
//    println(SimpleDLFormatter.format(backgroundOntology))
//    println()
//
//    val abducibles = Set("A", "C", "B1")
//
//    println("Abducibles: ")
//    println(abducibles.mkString("{", ", ", "}"))
//    println()
//
//    val observation = Ontology.buildFrom(Set(
//      Subsumption(BaseConcept("B1"), BaseConcept("B")) // B1 <= B
//    ))
//
//    println("Observation: ")
//    println(SimpleDLFormatter.format(observation))
//    println()
//
//    val abducer = new FullAbducer()
//    abducer.setBackgroundOntology(backgroundOntology)
//    abducer.setAbducibles(abducibles)
//
//    val forgettingResult = abducer.forget(observation)
//    val forgettingResultSimplified = abducer.simplify(forgettingResult)
//    val hypothesis = abducer.filter(forgettingResultSimplified)
//
//    println("Hypothesis:")
//    println(SimpleDLFormatter.format(hypothesis))
//
//    val hypothesisDisjuncts = hypothesis match {
//      case DisjunctiveDLStatement(ds) => ds.size
//      case other => 1
//    }
//
//    println("Hypothesis size: " + hypothesisDisjuncts)
//
//    val correctHypothesis =
//      Subsumption(BaseConcept("B1"), BaseConcept("A")) // B1 <= A
//
//    println("Expected Hypothesis:")
//    println(SimpleDLFormatter.format(correctHypothesis))
//
//
//    hypothesis match {
//      case DisjunctiveDLStatement(disjuncts) if disjuncts.size==1 =>
//        assertEquivalent(disjuncts.head, correctHypothesis)
//      case other => assert(false, "Unexpected shape of hypothesis: "+hypothesis)
//    }
//  }
//
//  def testDisjunctiveConceptIgnoreBackground() = {
//    val backgroundOntology = Ontology.buildFrom(Set(
//      Subsumption(BaseConcept("A1"), BaseConcept("B")), // A1 <= B
//      Subsumption(BaseConcept("B"), BaseConcept("C")), // B <= C
//      Subsumption(BaseConcept("A2"), BaseConcept("B")) // A2 <= B
//    ))
//
//    println("Background: ")
//    println(SimpleDLFormatter.format(backgroundOntology))
//    println()
//
//    val abducibles = Set("A1", "A2", "C", "B1")
//
//    println("Abducibles: ")
//    println(abducibles.mkString("{", ", ", "}"))
//    println()
//
//    val observation = Ontology.buildFrom(Set(
//      Subsumption(BaseConcept("B1"), BaseConcept("B")) // B1 <= B
//    ))
//
//    println("Observation: ")
//    println(SimpleDLFormatter.format(observation))
//    println()
//
//    val abducer = new FullAbducer()
//    abducer.setBackgroundOntology(backgroundOntology)
//    abducer.setAbducibles(abducibles)
//
//    val forgettingResult = abducer.forget(observation)
//    val forgettingResultSimplified = abducer.simplify(forgettingResult)
//    val hypothesis = abducer.filter(forgettingResultSimplified)
//
//    println("Hypothesis:")
//    println(SimpleDLFormatter.format(hypothesis))
//
//    val hypothesisDisjuncts = hypothesis match {
//      case DisjunctiveDLStatement(ds) => ds.size
//      case other => 1
//    }
//
//    println("Hypothesis size: " + hypothesisDisjuncts)
//
//    val correctHypothesis =
//      Subsumption(BaseConcept("B1"),
//        ConceptDisjunction(Set(BaseConcept("A1"), BaseConcept("A2")))) // B1 <= A1
//
//    println("Expected Hypothesis:")
//    println(SimpleDLFormatter.format(correctHypothesis))
//
//    hypothesis match {
//      case DisjunctiveDLStatement(disjuncts) if disjuncts.size==1 =>
//        assertEquivalent(disjuncts.head, correctHypothesis)
//      case other => assert(false, "Unexpected shape of hypothesis: "+hypothesis)
//    }
//  }
//
//  def testInverseRoles(): Unit = {
//    val backgroundOntology = Ontology.buildFrom(Set(
//      Subsumption(BaseConcept("A"), UniversalRoleRestriction(BaseRole("r"), BaseConcept("B"))) // A <= Ar.B
//    ))
//
//    println("Background: ")
//    println(SimpleDLFormatter.format(backgroundOntology))
//    println()
//
//    val abducibles = Set("A", "r")
//
//    println("Abducibles: ")
//    println(abducibles.mkString("{", ", ", "}"))
//    println()
//
//    val observation = Ontology.buildFrom(Set(
//      ConceptAssertion(BaseConcept("B"), Individual("a")) // B(a)
//    ))
//
//    println("Observation: ")
//    println(SimpleDLFormatter.format(observation))
//    println()
//
//    val abducer = new FullAbducer()
//    abducer.setBackgroundOntology(backgroundOntology)
//    abducer.setAbducibles(abducibles)
//
//    val forgettingResult = abducer.forget(observation)
//    val forgettingResultSimplified = abducer.simplify(forgettingResult)
//    val hypothesis = abducer.filter(forgettingResultSimplified)
//
//    println("Hypothesis:")
//    println(SimpleDLFormatter.format(hypothesis))
//
//    val hypothesisDisjuncts = hypothesis match {
//      case DisjunctiveDLStatement(ds) => ds.size
//      case other => 1
//    }
//
//    println("Hypothesis size: " + hypothesisDisjuncts)
//
//    val correctHypothesis =
//      ConceptAssertion(ExistentialRoleRestriction(InverseRole(BaseRole("r")), BaseConcept("A")),
//        Individual("a")) // (Er^-.A)(a))
//
//    println("Expected Hypothesis:")
//    println(SimpleDLFormatter.format(correctHypothesis))
//
//    hypothesis match {
//      case DisjunctiveDLStatement(disjuncts) if disjuncts.size==1 =>
//        assertEquivalent(disjuncts.head, correctHypothesis)
//      case other => assert(false, "Unexpected shape of hypothesis: "+hypothesis)
//    }
//  }
//
//  def testInverseRolesFixpoint(): Unit = {
//    val backgroundOntology = Ontology.buildFrom(Set(
//      Subsumption(BaseConcept("A"), UniversalRoleRestriction(BaseRole("r"), BaseConcept("B"))), // A <= Ar.B
//      Subsumption(BaseConcept("B"), UniversalRoleRestriction(BaseRole("r"), BaseConcept("B")))  // B <= Ar.B
//    ))
//
//    println("Background: ")
//    println(SimpleDLFormatter.format(backgroundOntology))
//    println()
//
//    val abducibles = Set("A", "r")
//
//    println("Abducibles: ")
//    println(abducibles.mkString("{", ", ", "}"))
//    println()
//
//    val observation = Ontology.buildFrom(Set(
//      ConceptAssertion(BaseConcept("B"), Individual("a")) // B(a)
//    ))
//
//    println("Observation: ")
//    println(SimpleDLFormatter.format(observation))
//    println()
//
//    val abducer = new FullAbducer()
//    abducer.setBackgroundOntology(backgroundOntology)
//    abducer.setAbducibles(abducibles)
//
//    val forgettingResult = abducer.forget(observation)
//    val forgettingResultSimplified = abducer.simplify(forgettingResult)
//    val hypothesis = abducer.filter(forgettingResultSimplified)
//
//    println("Hypothesis:")
//    println(SimpleDLFormatter.format(hypothesis))
//
//    val hypothesisDisjuncts = hypothesis match {
//      case DisjunctiveDLStatement(ds) => ds.size
//      case other => 1
//    }
//
//    println("Hypothesis size: " + hypothesisDisjuncts)
//
//    val x = new ConceptVariable("X")
//
//    val correctHypothesis =
//      ConceptAssertion(
//        LeastFixpoint(x, ConceptDisjunction(
//          Set(ExistentialRoleRestriction(InverseRole(BaseRole("r")), BaseConcept("A")),
//            ExistentialRoleRestriction(InverseRole(BaseRole("r")), x)))),
//        Individual("a")) // (Er^-.A)(a))
//
//    println("Expected Hypothesis:")
//    println(SimpleDLFormatter.format(correctHypothesis))
//
//    // we can't do reasoning with fixpoints, so we can only do a syntactical check here
//    /*
//    hypothesis match {
//      case DisjunctiveDLStatement(disjuncts) if disjuncts.size==1 =>
//        disjuncts.head match {
//          case ConceptAssertion(
//          LeastFixpoint(y, ConceptDisjunction(disjuncts)),
//          Individual("a")
//          ) =>
//            assert(disjuncts.contains(ExistentialRoleRestriction(InverseRole(BaseRole("r")), BaseConcept("A"))))
//            assert(disjuncts.contains(ExistentialRoleRestriction(InverseRole(BaseRole("r")), y)))
////          case other => assert(false, "expected different disjunct as "+other)
//        }
//      case other => assert(false, "Unexpected shape of hypothesis: "+hypothesis)
//    }
//     */
//  }
//
//  def testFixpointRedundancy(): Unit = {
//    val backgroundOntology = Ontology.buildFrom(Set(
//      Subsumption(BaseConcept("A"), UniversalRoleRestriction(BaseRole("r"), BaseConcept("B"))), // A <= Ar.B
//      Subsumption(BaseConcept("B"), UniversalRoleRestriction(BaseRole("r"), BaseConcept("B"))),  // B <= Ar.B
//      Subsumption(BaseConcept("D"), BaseConcept("E")),
//      Subsumption(BaseConcept("F"), BaseConcept("E")),
//      Subsumption(BaseConcept("F"), BaseConcept("D")),
//    ))
//
//    println("Background: ")
//    println(SimpleDLFormatter.format(backgroundOntology))
//    println()
//
//    val abducibles = Set("A", "D", "F", "r")
//
//    println("Abducibles: ")
//    println(abducibles.mkString("{", ", ", "}"))
//    println()
//
//    val observation = Ontology.buildFrom(Set(
//      ConceptAssertion(BaseConcept("B"), Individual("a")), // B(a)
//      ConceptAssertion(BaseConcept("E"), Individual("b"))
//    ))
//
//    println("Observation: ")
//    println(SimpleDLFormatter.format(observation))
//    println()
//
//    val abducer = new FullAbducer()
//    abducer.setBackgroundOntology(backgroundOntology)
//    abducer.setAbducibles(abducibles)
//
//    val forgettingResult = abducer.forget(observation)
//    val forgettingResultSimplified = abducer.simplify(forgettingResult)
//    val hypothesis = abducer.filter(forgettingResultSimplified)
//
//    println("Hypothesis:")
//    println(SimpleDLFormatter.format(hypothesis))
//
//    val hypothesisDisjuncts = hypothesis match {
//      case DisjunctiveDLStatement(ds) => ds.size
//      case other => 1
//    }
//
//    println("Hypothesis size: " + hypothesisDisjuncts)
//
//    val x = new ConceptVariable("X")
//
//  }
//
//  def testRoleIntroduction(): Unit ={
//    val backgroundOntology = Ontology.buildFrom(Set(
//      Subsumption(BaseConcept("A1"), BaseConcept("B")), // A1 <= B
//      Subsumption(BaseConcept("B"), BaseConcept("C")), // B <= C
//      Subsumption(BaseConcept("A2"), BaseConcept("B")) // A2 <= B
//    ))
//
//    println("Background: ")
//    println(SimpleDLFormatter.format(backgroundOntology))
//    println()
//
//    val abducibles = Set("A1", "A2", "C", "B1")
//
//    println("Abducibles: ")
//    println(abducibles.mkString("{", ", ", "}"))
//    println()
//
//    val observation = Ontology.buildFrom(Set(
//      Subsumption(BaseConcept("B1"), BaseConcept("B")) // B1 <= B
//    ))
//
//    println("Observation: ")
//    println(SimpleDLFormatter.format(observation))
//    println()
//
//    val abducer = new FullAbducer()
//    abducer.setBackgroundOntology(backgroundOntology)
//    abducer.setAbducibles(abducibles)
//
//    val forgettingResult = abducer.forget(observation)
//    val forgettingResultSimplified = abducer.simplify(forgettingResult)
//    val hypothesis = abducer.filter(forgettingResultSimplified)
//
//    println("Hypothesis:")
//    println(SimpleDLFormatter.format(hypothesis))
//
//    val hypothesisDisjuncts = hypothesis match {
//      case DisjunctiveDLStatement(ds) => ds.size
//      case other => 1
//    }
//
//    println("Hypothesis size: " + hypothesisDisjuncts)
//
//    val correctHypothesis =
//      Subsumption(BaseConcept("B1"),
//        ConceptDisjunction(Set(BaseConcept("A1"), BaseConcept("A2")))) // B1 <= A1 u A2
//
//    println("Expected Hypothesis:")
//    println(SimpleDLFormatter.format(correctHypothesis))
//
//    hypothesis match {
//      case DisjunctiveDLStatement(disjuncts) if disjuncts.size==1 =>
//        assertEquivalent(disjuncts.head, correctHypothesis)
//      case other => assert(false, "Unexpected shape of hypothesis: "+hypothesis)
//    }
//  }
//
//  def testRoleAssertionIntroduction(): Unit = {
//    val backgroundOntology = Ontology.buildFrom(Set(
//      Subsumption(BaseConcept("A"), UniversalRoleRestriction(BaseRole("r"), BaseConcept("B"))), // A <= Ar.B
//      ConceptAssertion(BaseConcept("A"), Individual("a"))                                       // A(a)
//    ))
//
//    println("Background: ")
//    println(SimpleDLFormatter.format(backgroundOntology))
//    println()
//
//    val abducibles = Set("r")
//
//    println("Abducibles: ")
//    println(abducibles.mkString("{", ", ", "}"))
//    println()
//
//    val observation = Ontology.buildFrom(Set(
//      ConceptAssertion(BaseConcept("B"), Individual("b")) // B(a)
//    ))
//
//    println("Observation: ")
//    println(SimpleDLFormatter.format(observation))
//    println()
//
//    val abducer = new FullAbducer()
//    abducer.setBackgroundOntology(backgroundOntology)
//    abducer.setAbducibles(abducibles)
//
//    val forgettingResult = abducer.forget(observation)
//    val forgettingResultSimplified = abducer.simplify(forgettingResult)
//    val hypothesis = abducer.filter(forgettingResultSimplified)
//
//    println("Hypothesis:")
//    println(SimpleDLFormatter.format(hypothesis))
//
//    val hypothesisDisjuncts = hypothesis match {
//      case DisjunctiveDLStatement(ds) => ds.size
//      case other => 1
//    }
//
//    println("Hypothesis size: " + hypothesisDisjuncts)
//
//    val correctHypothesis =
//      RoleAssertion(BaseRole("r"), Individual("a"), Individual("b")) // r(a,b)
//
//    println("Expected Hypothesis:")
//    println(SimpleDLFormatter.format(correctHypothesis))
//
//    hypothesis match {
//      case DisjunctiveDLStatement(disjuncts) if disjuncts.size==1 =>
//        assertEquivalent(disjuncts.head, correctHypothesis)
//      case other => assert(false, "Unexpected shape of hypothesis: "+hypothesis)
//    }
//  }
//
//
//  def testHypothesisOtherIndividual(): Unit = {
//    val backgroundOntology = Ontology.buildFrom(Set(
//      Subsumption(BaseConcept("A"), UniversalRoleRestriction(BaseRole("r"), BaseConcept("B"))), // A <= Ar.B
//      RoleAssertion(BaseRole("r"), Individual("b"), Individual("a"))                                       // r(b,a)
//    ))
//
//    println("Background: ")
//    println(SimpleDLFormatter.format(backgroundOntology))
//    println()
//
//    val abducibles = Set("A")
//
//    println("Abducibles: ")
//    println(abducibles.mkString("{", ", ", "}"))
//    println()
//
//    val observation = Ontology.buildFrom(Set(
//      ConceptAssertion(BaseConcept("B"), Individual("a")) // B(a)
//    ))
//
//    println("Observation: ")
//    println(SimpleDLFormatter.format(observation))
//    println()
//
//    val abducer = new FullAbducer()
//    abducer.setBackgroundOntology(backgroundOntology)
//    abducer.setAbducibles(abducibles)
//
//    val forgettingResult = abducer.forget(observation)
//    val forgettingResultSimplified = abducer.simplify(forgettingResult)
//    val hypothesis = abducer.filter(forgettingResultSimplified)
//
//    println("Hypothesis:")
//    println(SimpleDLFormatter.format(hypothesis))
//
//    val hypothesisDisjuncts = hypothesis match {
//      case DisjunctiveDLStatement(ds) => ds.size
//      case other => 1
//    }
//
//    println("Hypothesis size: " + hypothesisDisjuncts)
//
//    val correctHypothesis =
//      ConceptAssertion(BaseConcept("A"), Individual("b")) // A(b)
//
//    println("Expected Hypothesis:")
//    println(SimpleDLFormatter.format(correctHypothesis))
//
//    hypothesis match {
//      case DisjunctiveDLStatement(disjuncts) if disjuncts.size==1 =>
//        assertEquivalent(disjuncts.head, correctHypothesis)
//      case other => assert(false, "Unexpected shape of hypothesis: "+hypothesis)
//    }
//  }
//
//
//  def testConjunctionOfAssertions(): Unit = {
//    val backgroundOntology = Ontology.buildFrom(Set(
//      Subsumption(BaseConcept("A"), BaseConcept("C")), // A <= B
//      Subsumption(BaseConcept("B"), BaseConcept("D"))  // B <= D
//    ))
//
//    println("Background: ")
//    println(SimpleDLFormatter.format(backgroundOntology))
//    println()
//
//    val abducibles = Set("A", "B")
//
//    println("Abducibles: ")
//    println(abducibles.mkString("{", ", ", "}"))
//    println()
//
//    val observation = Ontology.buildFrom(Set(
//      ConceptAssertion(BaseConcept("C"), Individual("a")), // B(a)
//      ConceptAssertion(BaseConcept("D"), Individual("b"))
//    ))
//
//    println("Observation: ")
//    println(SimpleDLFormatter.format(observation))
//    println()
//
//    val abducer = new FullAbducer()
//    abducer.setBackgroundOntology(backgroundOntology)
//    abducer.setAbducibles(abducibles)
//
//    val forgettingResult = abducer.forget(observation)
//    val forgettingResultSimplified = abducer.simplify(forgettingResult)
//    val hypothesis = abducer.filter(forgettingResultSimplified)
//
//    println("Hypothesis:")
//    println(SimpleDLFormatter.format(hypothesis))
//
//    val hypothesisDisjuncts = hypothesis match {
//      case DisjunctiveDLStatement(ds) => ds.size
//      case other => 1
//    }
//
//    println("Hypothesis size: " + hypothesisDisjuncts)
//
//    val correctHypothesis =
//      ConjunctiveAssertion(Set(
//        ConceptAssertion(BaseConcept("A"), Individual("a")),
//        ConceptAssertion(BaseConcept("B"), Individual("b"))
//      ))
//
//    println("Expected Hypothesis:")
//    println(SimpleDLFormatter.format(correctHypothesis))
//
//    hypothesis match {
//      case DisjunctiveDLStatement(disjuncts) if disjuncts.size==1 =>
//        assertEquivalent(disjuncts.head, correctHypothesis)
//      case other => assert(false, "Unexpected shape of hypothesis: "+hypothesis)
//    }
//  }
//
//
//  def testDisjunctionOfAssertions(): Unit = {
//    val backgroundOntology = Ontology.buildFrom(Set(
//      Subsumption(BaseConcept("A"), BaseConcept("C")), // A <= B
//      Subsumption(BaseConcept("B"), BaseConcept("D"))  // B <= D
//    ))
//
//    println("Background: ")
//    println(SimpleDLFormatter.format(backgroundOntology))
//    println()
//
//    val abducibles = Set("A", "B")
//
//    println("Abducibles: ")
//    println(abducibles.mkString("{", ", ", "}"))
//    println()
//
//    val observation = Ontology.buildFrom(Set(
//      ConceptAssertion(
//        ConceptDisjunction(Set(BaseConcept("C"), BaseConcept("D"))),
//        Individual("a")), // (C or D)(a)
//    ))
//
//    println("Observation: ")
//    println(SimpleDLFormatter.format(observation))
//    println()
//
//    val abducer = new FullAbducer()
//    abducer.setBackgroundOntology(backgroundOntology)
//    abducer.setAbducibles(abducibles)
//
//    val forgettingResult = abducer.forget(observation)
//    val forgettingResultSimplified = abducer.simplify(forgettingResult)
//    val hypothesis = abducer.filter(forgettingResultSimplified)
//
//    println("Hypothesis:")
//    println(SimpleDLFormatter.format(hypothesis))
//
//    val hypothesisDisjuncts = hypothesis match {
//      case DisjunctiveDLStatement(ds) => ds.size
//      case other => 1
//    }
//
//    println("Hypothesis size: " + hypothesisDisjuncts)
//
//    val correctHypothesis1 = ConceptAssertion(BaseConcept("A"), Individual("a"))
//    val correctHypothesis2 = ConceptAssertion(BaseConcept("B"), Individual("a"))
//
//    println("Expected Hypotheses (disjunctive):")
//    println(SimpleDLFormatter.format(correctHypothesis1))
//    println(SimpleDLFormatter.format(correctHypothesis2))
//
//    hypothesis match {
//      case DisjunctiveDLStatement(disjuncts) if disjuncts.size==2 =>
//        assert(disjuncts.exists(equivalent(_, correctHypothesis1)))
//        assert(disjuncts.exists(equivalent(_, correctHypothesis2)))
//      case other => assert(false, "Unexpected shape of hypothesis: "+hypothesis)
//    }
//  }
//
//  def testRoleAssertionHypothesis(): Unit = {
//    val backgroundOntology = Ontology.buildFrom(Set(
//      Subsumption(ExistentialRoleRestriction(BaseRole("r"), BaseConcept("B")),
//        BaseConcept("A")),  // Er.B <= A
//      ConceptAssertion(BaseConcept("B"), Individual("b"))
//    ))
//
//    println("Background: ")
//    println(SimpleDLFormatter.format(backgroundOntology))
//    println()
//
//    val abducibles = Set("r")
//
//    println("Abducibles: ")
//    println(abducibles.mkString("{", ", ", "}"))
//    println()
//
//    val observation = Ontology.buildFrom(Set(
//      ConceptAssertion(BaseConcept("A"), Individual("a")) // A(a)
//    ))
//
//    println("Observation: ")
//    println(SimpleDLFormatter.format(observation))
//    println()
//
//    val abducer = new FullAbducer()
//    abducer.setBackgroundOntology(backgroundOntology)
//    abducer.setAbducibles(abducibles)
//
//    val forgettingResult = abducer.forget(observation)
//    val forgettingResultSimplified = abducer.simplify(forgettingResult)
//    val hypothesis = abducer.filter(forgettingResultSimplified)
//
//    println("Hypothesis:")
//    println(SimpleDLFormatter.format(hypothesis))
//
//    val hypothesisDisjuncts = hypothesis match {
//      case DisjunctiveDLStatement(ds) => ds.size
//      case other => 1
//    }
//
//    println("Hypothesis size: " + hypothesisDisjuncts)
//
//    val correctHypothesis = RoleAssertion(BaseRole("r"), Individual("a"), Individual("b")) // r(a,b)
//
//    println("Expected Hypothesis:")
//    println(SimpleDLFormatter.format(correctHypothesis))
//
//    hypothesis match {
//      case DisjunctiveDLStatement(disjuncts) if disjuncts.size==1 =>
//        assertEquivalent(disjuncts.head, correctHypothesis)
//      case other => assert(false, "Unexpected shape of hypothesis: "+hypothesis)
//    }
//  }
//
//
//  def testMixedHypothesis(): Unit = {
//    val backgroundOntology = Ontology.buildFrom(Set(
//      Subsumption(ExistentialRoleRestriction(BaseRole("r"), BaseConcept("B")),
//        BaseConcept("A")),  // Er.B <= A
//      Subsumption(BaseConcept("C"), BaseConcept("D")),
//      ConceptAssertion(BaseConcept("B"), Individual("b")) // B(b)
//    ))
//
//    println("Background: ")
//    println(SimpleDLFormatter.format(backgroundOntology))
//    println()
//
//    val abducibles = Set("r", "C")
//
//    println("Abducibles: ")
//    println(abducibles.mkString("{", ", ", "}"))
//    println()
//
//    val observation = Ontology.buildFrom(Set(
//      ConceptAssertion(BaseConcept("A"), Individual("a")), // A(a)
//      ConceptAssertion(BaseConcept("D"), Individual("d"))
//    ))
//
//    println("Observation: ")
//    println(SimpleDLFormatter.format(observation))
//    println()
//
//    val abducer = new FullAbducer()
//    abducer.setBackgroundOntology(backgroundOntology)
//    abducer.setAbducibles(abducibles)
//
//    val forgettingResult = abducer.forget(observation)
//    val forgettingResultSimplified = abducer.simplify(forgettingResult)
//    val hypothesis = abducer.filter(forgettingResultSimplified)
//
//    println("Hypothesis:")
//    println(SimpleDLFormatter.format(hypothesis))
//
//    val hypothesisDisjuncts = hypothesis match {
//      case DisjunctiveDLStatement(ds) => ds.size
//      case other => 1
//    }
//
//    println("Hypothesis size: " + hypothesisDisjuncts)
//
//    val correctHypothesis = ConjunctiveAssertion(Set(
//      RoleAssertion(BaseRole("r"), Individual("a"), Individual("b")),
//      ConceptAssertion(BaseConcept("C"), Individual("d"))
//    ))
//
//    println("Expected Hypothesis:")
//    println(SimpleDLFormatter.format(correctHypothesis))
//
//    hypothesis match {
//      case DisjunctiveDLStatement(disjuncts) if disjuncts.size==1 =>
//        assertEquivalent(disjuncts.head, correctHypothesis)
//      case other => assert(false, "Unexpected shape of hypothesis: "+hypothesis)
//    }
//  }
//
//  def testRoleForgetting(): Unit = {
//    val backgroundOntology = Ontology.buildFrom(Set(
//      Subsumption(BaseConcept("B"), ExistentialRoleRestriction(BaseRole("r"), BaseConcept("B")))))
//
//    println("Background: ")
//    println(SimpleDLFormatter.format(backgroundOntology))
//    println()
//
//    val abducibles = Set("B")
//
//    println("Abducibles: ")
//    println(abducibles.mkString("{", ", ", "}"))
//    println()
//
//    val observation = Ontology.buildFrom(Set(
//      ConceptAssertion(ExistentialRoleRestriction(BaseRole("r"), BaseConcept("B")), Individual("a"))
//    ))
//
//    println("Observation: ")
//    println(SimpleDLFormatter.format(observation))
//    println()
//
//    val abducer = new FullAbducer()
//    abducer.setBackgroundOntology(backgroundOntology)
//    abducer.setAbducibles(abducibles)
//
//    val forgettingResult = abducer.forget(observation)
//    val forgettingResultSimplified = abducer.simplify(forgettingResult)
//    val hypothesis = abducer.filter(forgettingResultSimplified)
//
//    println("Hypothesis:")
//    println(SimpleDLFormatter.format(hypothesis))
//
//    val hypothesisDisjuncts = hypothesis match {
//      case DisjunctiveDLStatement(ds) => ds.size
//      case other => 1
//    }
//
//    println("Hypothesis size: " + hypothesisDisjuncts)
//
//    val correctHypothesis = ConjunctiveAssertion(Set(
//      ConceptAssertion(BaseConcept("B"), Individual("a"))
//    ))
//
//    println("Expected Hypothesis:")
//    println(SimpleDLFormatter.format(correctHypothesis))
//
//    hypothesis match {
//      case DisjunctiveDLStatement(disjuncts) if disjuncts.size==1 =>
//        assertEquivalent(disjuncts.head, correctHypothesis)
//      case other => assert(false, "Unexpected shape of hypothesis: "+hypothesis)
//    }
//  }
//
//  def testRoleAssertionResolution() = {
//    val backgroundOntology = Ontology.buildFrom(Set(
//      RoleAssertion(BaseRole("r"), Individual("a"), Individual("b"))))
//
//    println("Background: ")
//    println(SimpleDLFormatter.format(backgroundOntology))
//    println()
//
//    val abducibles = Set("")
//
//    println("Abducibles: ")
//    println(abducibles.mkString("{", ", ", "}"))
//    println()
//
//    val observation = Ontology.buildFrom(Set(
//      RoleAssertion(BaseRole("r"), Individual("a"), Individual("b"))
//    ))
//
//    println("Observation: ")
//    println(SimpleDLFormatter.format(observation))
//    println()
//
//    val abducer = new FullAbducer()
//    abducer.setBackgroundOntology(backgroundOntology)
//    abducer.setAbducibles(abducibles)
//
//    val forgettingResult = abducer.forget(observation)
//    val forgettingResultSimplified = abducer.simplify(forgettingResult)
//    val hypothesis = abducer.filter(forgettingResultSimplified)
//
//    println("Hypothesis:")
//    println(SimpleDLFormatter.format(hypothesis))
//
//    val hypothesisDisjuncts = hypothesis match {
//      case DisjunctiveDLStatement(ds) => ds.size
//      case other => 1
//    }
//
//    println("Hypothesis size: " + hypothesisDisjuncts)
//
//    println("Expected Hypothesis:")
//    println("tautology")
//
//    hypothesis match {
//      case DisjunctiveDLStatement(disjuncts) if disjuncts.size>0 =>
//        disjuncts.foreach(assertEquivalent(_, Subsumption(TopConcept, TopConcept)))
//      case other => assert(false, "Unexpected shape of hypothesis: "+hypothesis)
//    }
//  }
//
//  def testUniversalRoleRoleAssertion() = {
//    val backgroundOntology = Ontology.buildFrom(Set(
//      RoleAssertion(BaseRole("r"), Individual("a"), Individual("b")),
//      Subsumption(ExistentialRoleRestriction(BaseRole("r"), BaseConcept("A")),
//        BaseConcept("B"))
//    ))
//
//    println("Background: ")
//    println(SimpleDLFormatter.format(backgroundOntology))
//    println()
//
//    val abducibles = Set("A")
//
//    println("Abducibles: ")
//    println(abducibles.mkString("{", ", ", "}"))
//    println()
//
//    val observation = Ontology.buildFrom(Set(
//      ConceptAssertion(BaseConcept("B"), Individual("a"))
//    ))
//
//    println("Observation: ")
//    println(SimpleDLFormatter.format(observation))
//    println()
//
//    val abducer = new FullAbducer()
//    abducer.setBackgroundOntology(backgroundOntology)
//    abducer.setAbducibles(abducibles)
//
//    val forgettingResult = abducer.forget(observation)
//    val forgettingResultSimplified = abducer.simplify(forgettingResult)
//    val hypothesis = abducer.filter(forgettingResultSimplified)
//
//    println("Hypothesis:")
//    println(SimpleDLFormatter.format(hypothesis))
//
//    val hypothesisDisjuncts = hypothesis match {
//      case DisjunctiveDLStatement(ds) => ds.size
//      case other => 1
//    }
//
//    println("Hypothesis size: " + hypothesisDisjuncts)
//
//    val correctHypothesis = ConjunctiveAssertion(Set(
//      ConceptAssertion(BaseConcept("A"), Individual("b"))
//    ))
//
//    println("Expected Hypothesis:")
//    println(SimpleDLFormatter.format(correctHypothesis))
//
//    hypothesis match {
//      case DisjunctiveDLStatement(disjuncts) if disjuncts.size==1 =>
//        assertEquivalent(disjuncts.head, correctHypothesis)
//      case other => assert(false, "Unexpected shape of hypothesis: "+hypothesis)
//    }
//  }
//
//  def testPullingDefiners() = {
//    val backgroundOntology = Ontology.buildFrom(Set(
//      Subsumption(BaseConcept("B"), UniversalRoleRestriction(BaseRole("r"), BaseConcept("C"))),
//      Subsumption(BaseConcept("C"), UniversalRoleRestriction(BaseRole("r"), BaseConcept("A"))),
//      Subsumption(BaseConcept("D"), BaseConcept("A")),
//      Subsumption(UniversalRoleRestriction(BaseRole("s"), BaseConcept("E")), BaseConcept("D")),
//      Subsumption(ExistentialRoleRestriction(
//        BaseRole("s"),
//        ConceptConjunction(Set(ConceptComplement(BaseConcept("E")), BaseConcept("B")))),
//        BaseConcept("D"))
//    ))
//
//
//    println("Background: ")
//    println(SimpleDLFormatter.format(backgroundOntology))
//    println()
//
//    val abducibles = Set("B", "r", "s")
//
//    println("Abducibles: ")
//    println(abducibles.mkString("{", ", ", "}"))
//    println()
//
//    val observation = Ontology.buildFrom(Set(
//      ConceptAssertion(BaseConcept("A"), Individual("a"))
//    ))
//
//    println("Observation: ")
//    println(SimpleDLFormatter.format(observation))
//    println()
//
//    val abducer = new FullAbducer()
//    abducer.setBackgroundOntology(backgroundOntology)
//    abducer.setAbducibles(abducibles)
//
//    val forgettingResult = abducer.forget(observation)
//    val forgettingResultSimplified = abducer.simplify(forgettingResult)
//    val hypothesis = abducer.filter(forgettingResultSimplified)
//
//    println("Hypothesis:")
//    println(SimpleDLFormatter.format(hypothesis))
//
//    val hypothesisDisjuncts = hypothesis match {
//      case DisjunctiveDLStatement(ds) => ds.size
//      case other => 1
//    }
//
//    println("Hypothesis size: " + hypothesisDisjuncts)
//
//    val correctHypothesis1 =
//      ConceptAssertion(ExistentialRoleRestriction(InverseRole(BaseRole("r")),
//        ExistentialRoleRestriction(InverseRole(BaseRole("r")), BaseConcept("B"))), Individual("a"))
//    val correctHypothesis2 =
//      ConceptAssertion(UniversalRoleRestriction(BaseRole("s"), BaseConcept("B")), Individual("a"))
//
//    println("Expected Hypothesis (two):")
//    println(SimpleDLFormatter.format(correctHypothesis1))
//    println(SimpleDLFormatter.format(correctHypothesis2))
//
//    hypothesis match {
//      case DisjunctiveDLStatement(disjuncts) if disjuncts.size==2 =>
//        assert(disjuncts.exists(equivalent(_, correctHypothesis1)))
//        assert(disjuncts.exists(equivalent(_, correctHypothesis2)))
//      case other => assert(false, "Unexpected shape of hypothesis: "+hypothesis)
//    }
//  }
//
//  def testTwoTBoxObservation() = {
//    val backgroundOntology = Ontology.buildFrom(Set(
//      Subsumption(BaseConcept("A"), BaseConcept("B")),
//      Subsumption(BaseConcept("C"), BaseConcept("D"))
//    ))
//
//
//    println("Background: ")
//    println(backgroundOntology)
//    println()
//
//    val abducibles = Set("A", "C", "E", "F")
//
//    println("Abducibles: ")
//    println(abducibles.mkString("{", ", ", "}"))
//    println()
//
//    val observation = Ontology.buildFrom(Set(
//      Subsumption(BaseConcept("E"), BaseConcept("B")),
//      Subsumption(BaseConcept("F"), BaseConcept("D"))
//    ))
//
//    println("Observation: ")
//    println(observation)
//    println()
//
//    ExtendedABoxForgetter.setMaxSteps(5)
//
//    val abducer = new FullAbducer()
//    abducer.setBackgroundOntology(backgroundOntology)
//    abducer.setAbducibles(abducibles)
//
//    val forgettingResult = abducer.forget(observation)
//    val forgettingResultSimplified = abducer.simplify(forgettingResult)
//    val hypothesis = abducer.filter(forgettingResultSimplified)
//
//    println("Hypothesis:")
//    println(hypothesis)
//
//    val hypothesisDisjuncts = hypothesis match {
//      case DisjunctiveDLStatement(ds) => ds.size
//      case other => 1
//    }
//
//    println("Hypothesis size: " + hypothesisDisjuncts)
//
//    val correctHypothesis1 =
//      ConjunctiveDLStatement(Set(
//        Subsumption(BaseConcept("E"), BaseConcept("A")),
//        Subsumption(BaseConcept("F"), BaseConcept("C"))
//      ))
//
//    println("Expected Hypothesis:")
//    println(SimpleDLFormatter.format(correctHypothesis1))
//
//    hypothesis match {
//      case DisjunctiveDLStatement(disjuncts) if disjuncts.size==1 =>
//        assert(disjuncts.exists(equivalent(_, correctHypothesis1)))
//      case other => assert(false, "Unexpected shape of hypothesis: "+hypothesis)
//    }
//  }
//
//  val entailmentChecker = LogicalEntailmentCheckerClassical
//
//  val owlExporter = new OWLExporter()
//
//  def assertEquivalent(statement1: DLStatement, statement2: DLStatement): Unit =
//
//    assertEquivalent(Ontology.buildFrom(Set(statement1)),
//      Ontology.buildFrom(Set(statement2)))
//
//  def assertEquivalent(ont1: Ontology, ont2: Ontology): Unit = {
//    val owlOnt1 = owlExporter.toOwlOntology(ont1);
//    val owlOnt2 = owlExporter.toOwlOntology(ont2);
//
//    val reasoner1 = entailmentChecker.getReasoner(owlOnt1)
//
//    owlOnt2.getLogicalAxioms().asScala.foreach{ ax =>
//      assert(entailmentChecker.isEntailed(ax, owlOnt1, reasoner1),
//        ont1+"\n should entail "+SimpleOWLFormatter.format(ax))
//    }
//
//    val reasoner2 = entailmentChecker.getReasoner(owlOnt2)
//
//    owlOnt1.getLogicalAxioms().asScala.foreach { ax =>
//      assert(entailmentChecker.isEntailed(ax, owlOnt2, reasoner2),
//        ont1 + "\n should entail " + SimpleOWLFormatter.format(ax))
//    }
//  }
//
//  def equivalent(statement1: DLStatement, statement2: DLStatement): Boolean =
//    equivalent(Ontology.buildFrom(Set(statement1)),
//      Ontology.buildFrom(Set(statement2)))
//
//  def equivalent(ont1: Ontology, ont2: Ontology): Boolean = {
//    val owlOnt1 = owlExporter.toOwlOntology(ont1);
//    val owlOnt2 = owlExporter.toOwlOntology(ont2);
//
//    val reasoner1 = entailmentChecker.getReasoner(owlOnt1)
//
//    val reasoner2 = entailmentChecker.getReasoner(owlOnt2)
//
//    owlOnt2.getLogicalAxioms().asScala.forall{ ax =>
//      entailmentChecker.isEntailed(ax, owlOnt1, reasoner1)
//    } && owlOnt1.getLogicalAxioms().asScala.forall { ax =>
//      entailmentChecker.isEntailed(ax, owlOnt2, reasoner2)
//    }
//  }
//
//  def testDisjunctiveHypothesisRedundantEquivalence(): Unit = {
//    val backgroundOntology = Ontology.buildFrom(Set(
//      Subsumption(BaseConcept("A"), BaseConcept("C")), // A <= B
//      Subsumption(BaseConcept("B"), BaseConcept("C")), // B <= C
//      ConceptEquivalence(BaseConcept("B"), BaseConcept("A")), // B <= C
//    ))
//
//    println("Background: ")
//    println(SimpleDLFormatter.format(backgroundOntology))
//    println()
//
//    val abducibles = Set("A", "B")
//
//    println("Abducibles: ")
//    println(abducibles.mkString("{", ", ", "}"))
//    println()
//
//    val observation = Ontology.buildFrom(Set(
//      ConceptAssertion(
//        BaseConcept("C"),
//        Individual("a")), // (C or D)(a)
//    ))
//
//    println("Observation: ")
//    println(SimpleDLFormatter.format(observation))
//    println()
//
//    val abducer = new FullAbducer()
//    abducer.setBackgroundOntology(backgroundOntology)
//    abducer.setAbducibles(abducibles)
//
//    val forgettingResult = abducer.forget(observation)
//    val forgettingResultSimplified = abducer.simplify(forgettingResult)
//    val hypothesis = abducer.filter(forgettingResultSimplified)
//
//    println("Hypothesis:")
//    println(SimpleDLFormatter.format(hypothesis))
//
//    val hypothesisDisjuncts = hypothesis match {
//      case DisjunctiveDLStatement(ds) => ds.size
//      case other => 1
//    }
//
//    println("Hypothesis size: " + hypothesisDisjuncts)
//
//
//    val correctHypothesis1 = ConceptAssertion(BaseConcept("A"), Individual("a"))
//    val correctHypothesis2 = ConceptAssertion(BaseConcept("B"), Individual("a"))
//
//    println("Expected Hypothesis, one of:")
//    println(SimpleDLFormatter.format(correctHypothesis1))
//    println(SimpleDLFormatter.format(correctHypothesis2))
//
//    hypothesis match {
//      case DisjunctiveDLStatement(disjuncts) if disjuncts.size==1 =>
//        assert(disjuncts.exists(equivalent(_, correctHypothesis1)) || disjuncts.exists(equivalent(_, correctHypothesis2)))
//      case other => assert(false, "Unexpected shape of hypothesis: "+hypothesis)
//    }
//  }
//  def testDisjunctiveHypothesisRedundant(): Unit = {
//    val backgroundOntology = Ontology.buildFrom(Set(
//      Subsumption(BaseConcept("A"), BaseConcept("C")), // A <= B
//      Subsumption(BaseConcept("B"), BaseConcept("C")) , // B <= C
//      Subsumption(BaseConcept("B"), BaseConcept("A")) // B <= A
//    ))
//
//    println("Background: ")
//    println(SimpleDLFormatter.format(backgroundOntology))
//    println()
//
//    val abducibles = Set("A", "B")
//
//    println("Abducibles: ")
//    println(abducibles.mkString("{", ", ", "}"))
//    println()
//
//    val observation = Ontology.buildFrom(Set(
//      ConceptAssertion(
//        BaseConcept("C"),
//        Individual("a")), // (C or D)(a)
//    ))
//
//    println("Observation: ")
//    println(SimpleDLFormatter.format(observation))
//    println()
//
//    val abducer = new FullAbducer()
//    abducer.setBackgroundOntology(backgroundOntology)
//    abducer.setAbducibles(abducibles)
//
//    val forgettingResult = abducer.forget(observation)
//    val forgettingResultSimplified = abducer.simplify(forgettingResult)
//    val hypothesis = abducer.filter(forgettingResultSimplified)
//
//    println("Hypothesis:")
//    println(SimpleDLFormatter.format(hypothesis))
//
//    val hypothesisDisjuncts = hypothesis match {
//      case DisjunctiveDLStatement(ds) => ds.size
//      case other => 1
//    }
//
//    println("Hypothesis size: " + hypothesisDisjuncts)
//
//    val correctHypothesis1 = ConceptAssertion(BaseConcept("A"), Individual("a"))
//
//    println("Expected Hypothesis:")
//    println(SimpleDLFormatter.format(correctHypothesis1))
//
//    hypothesis match {
//      case DisjunctiveDLStatement(disjuncts) if disjuncts.size==1 =>
//        assert(disjuncts.exists(equivalent(_, correctHypothesis1)))
//      case other => assert(false, "Unexpected shape of hypothesis: "+hypothesis)
//    }
//  }
//  def testDisjunctiveTBoxHypothesisRedundant(): Unit = {
//    val backgroundOntology = Ontology.buildFrom(Set(
//      Subsumption(BaseConcept("F"), BaseConcept("B")), // F <= B
//      Subsumption(BaseConcept("B"), BaseConcept("C")), // B <= C
//      Subsumption(ConceptConjunction(Set(BaseConcept("D"), BaseConcept("E"))), BottomConcept), //D and E <= bottom
//      ConceptAssertion(BaseConcept("C"), Individual("a")),
//      ConceptAssertion(BaseConcept("E"), Individual("a"))
//    ))
//
//    println("Background: ")
//    println(SimpleDLFormatter.format(backgroundOntology))
//    println()
//
//    val abducibles = Set("A", "B", "C", "D", "E", "F")
//
//    println("Abducibles: ")
//    println(abducibles.mkString("{", ", ", "}"))
//    println()
//
//    val observation = Ontology.buildFrom(Set(
//      Subsumption(
//        ConceptConjunction(Set(ExistentialRoleRestriction(BaseRole("r"), BaseConcept("A")), BaseConcept("C"))),
//        ConceptDisjunction(Set(ExistentialRoleRestriction(BaseRole("r"), BaseConcept("B")), BaseConcept("D"))))
//    ))
//
//    println("Observation: ")
//    println(SimpleDLFormatter.format(observation))
//    println()
//
//    val abducer = new FullAbducer()
//    abducer.setBackgroundOntology(backgroundOntology)
//    abducer.setAbducibles(abducibles)
//
//    val forgettingResult = abducer.forget(observation)
//    val forgettingResultSimplified = abducer.simplify(forgettingResult)
//    val hypothesis = abducer.filter(forgettingResultSimplified)
//
//    println("Hypothesis:")
//    println(SimpleDLFormatter.format(hypothesis))
//
//    val hypothesisDisjuncts = hypothesis match {
//      case DisjunctiveDLStatement(ds) => ds.size
//      case other => 1
//    }
//
//    println("Hypothesis size: " + hypothesisDisjuncts)
//
//    val correctHypothesis1 = Subsumption(BaseConcept("A"), BaseConcept("B"))
//
//    println("Expected Hypothesis:")
//    println(SimpleDLFormatter.format(correctHypothesis1))
//
//    hypothesis match {
//      case DisjunctiveDLStatement(disjuncts) if disjuncts.size==1 =>
//        assert(disjuncts.exists(equivalent(_, correctHypothesis1)))
//      case other => assert(false, "Unexpected shape of hypothesis: "+hypothesis)
//    }
//  }
//  def testDisjunctiveTBoxHypothesisNotRedundant(): Unit = {
//    val backgroundOntology = Ontology.buildFrom(Set(
//      Subsumption(BaseConcept("F"), BaseConcept("B")), // F <= B
//    ))
//
//    println("Background: ")
//    println(SimpleDLFormatter.format(backgroundOntology))
//    println()
//
//    val abducibles = Set("A", "B", "C", "D", "E", "F")
//
//    println("Abducibles: ")
//    println(abducibles.mkString("{", ", ", "}"))
//    println()
//
//    val observation = Ontology.buildFrom(Set(
//      Subsumption(
//        ConceptConjunction(Set(ExistentialRoleRestriction(BaseRole("r"), BaseConcept("A")), BaseConcept("C"))),
//        ConceptDisjunction(Set(ExistentialRoleRestriction(BaseRole("r"), BaseConcept("B")), BaseConcept("D"))))
//    ))
//
//    println("Observation: ")
//    println(SimpleDLFormatter.format(observation))
//    println()
//
//    val abducer = new FullAbducer()
//    abducer.setBackgroundOntology(backgroundOntology)
//    abducer.setAbducibles(abducibles)
//
//    val forgettingResult = abducer.forget(observation)
//    val forgettingResultSimplified = abducer.simplify(forgettingResult)
//    val hypothesis = abducer.filter(forgettingResultSimplified)
//
//    println("Hypothesis:")
//    println(SimpleDLFormatter.format(hypothesis))
//
//    val hypothesisDisjuncts = hypothesis match {
//      case DisjunctiveDLStatement(ds) => ds.size
//      case other => 1
//    }
//
//    println("Hypothesis size: " + hypothesisDisjuncts)
//
//    val correctHypothesis1 = Subsumption(BaseConcept("A"), BaseConcept("B"))
//    val correctHypothesis2 = Subsumption(BaseConcept("C"), BaseConcept("D"))
//
//    println("Expected Hypothesis (disjunction):")
//    println(SimpleDLFormatter.format(correctHypothesis1))
//    println(SimpleDLFormatter.format(correctHypothesis2))
//
//    //assertEquivalent(correctHypothesis1, hypothesis)
//
//
//    hypothesis match {
//      case DisjunctiveDLStatement(disjuncts) if disjuncts.size == 2 =>
//        assert(disjuncts.exists(equivalent(_, correctHypothesis1)))
//        assert(disjuncts.exists(equivalent(_, correctHypothesis2)))
//      case other => assert(false, "Unexpected shape of hypothesis: "+hypothesis)
//      case other => assert(false, "Unexpected shape of hypothesis: " + hypothesis)
//    }
//  }
//  def testNumberOfDisjunctions(): Unit = {
//    val backgroundOntology = Ontology.buildFrom(Set(
//      Subsumption(BaseConcept("A"), BaseConcept("C")), // A <= C
//      Subsumption(BaseConcept("B"), BaseConcept("C")),
//    ))
//
//    println("Background: ")
//    println(SimpleDLFormatter.format(backgroundOntology))
//    println()
//
//    val abducibles = Set("A", "B")
//
//    println("Abducibles: ")
//    println(abducibles.mkString("{", ", ", "}"))
//    println()
//
//    val observation = Ontology.buildFrom(Set(
//      ConceptAssertion(BaseConcept("C"), Individual("a"))
//    ))
//
//    println("Observation: ")
//    println(SimpleDLFormatter.format(observation))
//    println()
//
//    val abducer = new FullAbducer()
//    abducer.setBackgroundOntology(backgroundOntology)
//    abducer.setAbducibles(abducibles)
//
//    val forgettingResult = abducer.forget(observation)
//    val forgettingResultSimplified = abducer.simplify(forgettingResult)
//    val hypothesis = abducer.filter(forgettingResultSimplified)
//
//    println("Hypothesis:")
//    println(SimpleDLFormatter.format(hypothesis))
//
//    val hypothesisDisjuncts = hypothesis match {
//      case DisjunctiveDLStatement(ds) => ds.size
//      case other => 1
//    }
//    println("Hypothesis size, num disjuncts: ")
//    println(hypothesisDisjuncts)
//
//    val correctHypothesis1 = ConceptAssertion(BaseConcept("A"), Individual("a"))
//    val correctHypothesis2 = ConceptAssertion(BaseConcept("B"), Individual("a"))
//
//    println("Expected Hypothesis (disjunction):")
//    println(SimpleDLFormatter.format(correctHypothesis1))
//    println(SimpleDLFormatter.format(correctHypothesis2))
//
//    //assertEquivalent(correctHypothesis1, hypothesis)
//
//
//    hypothesis match {
//      case DisjunctiveDLStatement(disjuncts) if disjuncts.size == 2 =>
//        assert(disjuncts.exists(equivalent(_, correctHypothesis1)))
//        assert(disjuncts.exists(equivalent(_, correctHypothesis2)))
//      case other => assert(false, "Unexpected shape of hypothesis: "+hypothesis)
//      case other => assert(false, "Unexpected shape of hypothesis: " + hypothesis)
//    }
//  }
//  def testNumberOfDisjunctionsTBoxABox(): Unit = {
//    val backgroundOntology = Ontology.buildFrom(Set(
//      Subsumption(BaseConcept("A"), BaseConcept("C")), // A <= C
//      Subsumption(BaseConcept("B"), BaseConcept("C")), //B <= C
//      Subsumption(BaseConcept("E"), BaseConcept("F")), //E <= F
//    ))
//
//    println("Background: ")
//    println(SimpleDLFormatter.format(backgroundOntology))
//    println()
//
//    val abducibles = Set("A", "B", "D", "E")
//
//    println("Abducibles: ")
//    println(abducibles.mkString("{", ", ", "}"))
//    println()
//
//    val observation = Ontology.buildFrom(Set(
//      ConceptAssertion(BaseConcept("C"), Individual("a")),
//      Subsumption(BaseConcept("D"), BaseConcept("F")),
//    ))
//
//    println("Observation: ")
//    println(SimpleDLFormatter.format(observation))
//    println()
//
//    val abducer = new FullAbducer()
//    abducer.setBackgroundOntology(backgroundOntology)
//    abducer.setAbducibles(abducibles)
//
//    val forgettingResult = abducer.forget(observation)
//    val forgettingResultSimplified = abducer.simplify(forgettingResult)
//    val hypothesis = abducer.filter(forgettingResultSimplified)
//
//    println("Hypothesis:")
//    println(SimpleDLFormatter.format(hypothesis))
//
//    val hypothesisDisjuncts = hypothesis match {
//      case DisjunctiveDLStatement(ds) => ds.size
//      case other => 1
//    }
//    println("Hypothesis size, num disjuncts: ")
//    println(hypothesisDisjuncts)
//
//    /*
//    val correctHypothesis1 = ConceptAssertion(BaseConcept("A"), Individual("a"))
//    val correctHypothesis2 = ConceptAssertion(BaseConcept("B"), Individual("a"))
//
//    println("Expected Hypothesis (disjunction):")
//    println(SimpleDLFormatter.format(correctHypothesis1))
//    println(SimpleDLFormatter.format(correctHypothesis2))
//     */
//    //assertEquivalent(correctHypothesis1, hypothesis)
//
//    /*
//    hypothesis match {
//      case DisjunctiveDLStatement(disjuncts) if disjuncts.size == 2 =>
//        assert(disjuncts.exists(equivalent(_, correctHypothesis1)))
//        assert(disjuncts.exists(equivalent(_, correctHypothesis2)))
//      case other => assert(false, "Unexpected shape of hypothesis: "+hypothesis)
//      case other => assert(false, "Unexpected shape of hypothesis: " + hypothesis)
//    }
//
//     */
//  }
//  def testNumberOfDisjunctionsTBoxABox2(): Unit = {
//    val backgroundOntology = Ontology.buildFrom(Set(
//      Subsumption(BaseConcept("F"), BaseConcept("E")), // F <= B
//      Subsumption(BaseConcept("G"), BaseConcept("E")),
//    ))
//
//    println("Background: ")
//    println(SimpleDLFormatter.format(backgroundOntology))
//    println()
//
//    val abducibles = Set("A", "B", "C", "D", "F", "G")
//
//    println("Abducibles: ")
//    println(abducibles.mkString("{", ", ", "}"))
//    println()
//
//    val observation = Ontology.buildFrom(Set(
//      ConceptAssertion(BaseConcept("E"), Individual("a")),
//      Subsumption(
//        ConceptConjunction(Set(ExistentialRoleRestriction(BaseRole("r"), BaseConcept("A")), BaseConcept("C"))),
//        ConceptDisjunction(Set(ExistentialRoleRestriction(BaseRole("r"), BaseConcept("B")), BaseConcept("D"))))
//    ))
//
//    println("Observation: ")
//    println(SimpleDLFormatter.format(observation))
//    println()
//
//    val abducer = new FullAbducer()
//    abducer.setBackgroundOntology(backgroundOntology)
//    abducer.setAbducibles(abducibles)
//
//    val forgettingResult = abducer.forget(observation)
//    val forgettingResultSimplified = abducer.simplify(forgettingResult)
//    val hypothesis = abducer.filter(forgettingResultSimplified)
//
//    println("Hypothesis:")
//    println(SimpleDLFormatter.format(hypothesis))
//
//    val hypothesisDisjuncts = hypothesis match {
//      case DisjunctiveDLStatement(ds) => ds.size
//      case other => 1
//    }
//    println("Hypothesis size, num disjuncts: ")
//    println(hypothesisDisjuncts)
//
//    val correctHypothesis1 = Subsumption(BaseConcept("A"), BaseConcept("B"))
//    val correctHypothesis2 = Subsumption(BaseConcept("C"), BaseConcept("D"))
//
//    /*
//    println("Expected Hypothesis (disjunction):")
//    println(SimpleDLFormatter.format(correctHypothesis1))
//    println(SimpleDLFormatter.format(correctHypothesis2))
//
//     */
//
//    //assertEquivalent(correctHypothesis1, hypothesis)
//    /*
//    hypothesis match {
//      case DisjunctiveDLStatement(disjuncts) if disjuncts.size == 2 =>
//        assert(disjuncts.exists(equivalent(_, correctHypothesis1)))
//        assert(disjuncts.exists(equivalent(_, correctHypothesis2)))
//      case other => assert(false, "Unexpected shape of hypothesis: "+hypothesis)
//      case other => assert(false, "Unexpected shape of hypothesis: " + hypothesis)
//    }
//
//     */
//  }
//}
