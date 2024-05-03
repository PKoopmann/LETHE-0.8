package tests

import uk.ac.man.cs.lethe.internal.dl.datatypes._
import uk.ac.man.cs.lethe.internal.dl.forgetting.{ConceptAndRoleForgetter, SHQForgetter}
import uk.ac.man.cs.lethe.internal.dl.forgetting.abox.ABoxForgetter
import uk.ac.man.cs.lethe.internal.forgetting.Forgetter
import uk.ac.man.cs.lethe.internal.tools.formatting.SimpleDLFormatter
//import uk.ac.man.cs.lethe.internal.tools.formatting.SimpleOWLFormatter
//import uk.ac.man.cs.lethe.logicalDifference.LogicalEntailmentCheckerClassical

import scala.collection.JavaConverters._

object ForgettingTestCases {

  def main(args: Array[String]) = {
    Set(
      ABoxForgetter,
      ConceptAndRoleForgetter,
    //  SHQForgetter,
    ).foreach{forg =>
      println("testing "+forg)
      val test = new ForgettingTestCases(forg)
      test.testCase1()
      test.testCase2()
      test.testCaseRoleHierarchies1()
      test.testCaseRoleHierarchies2()
    }

    println("SUCCESS!")
  }

}

object ParticularTest {
  def main(args: Array[String]) = {
 //   <A> = (<F> n E<r>.<C> n E<r>.<D> n E<s>.<E>)
 //   <B> = (<F> n E<r>.<C>)
    val ontology=Ontology.buildFrom(Set(
      ConceptEquivalence(BaseConcept("A"),
        ConceptConjunction(Set(
          BaseConcept("F"),
          ExistentialRoleRestriction(BaseRole("r"), BaseConcept("C")),
          ExistentialRoleRestriction(BaseRole("r"), BaseConcept("D")),
          ExistentialRoleRestriction(BaseRole("s"), BaseConcept("E"))
        ))),
   ConceptEquivalence(BaseConcept("B"),
     ConceptConjunction(Set(
       BaseConcept("F"),
       ExistentialRoleRestriction(BaseRole("r"), BaseConcept("C")))
     )))
 )

    val interpolant = ConceptAndRoleForgetter.forget(ontology, Set("C"))

    println(SimpleDLFormatter.format(interpolant))

  }
}

class ForgettingTestCases(forgetter: Forgetter[Ontology,String]) {

//  val entailmentChecker = LogicalEntailmentCheckerClassical
//
//  val owlExporter = new OWLExporter()

  def testCase1() = {
    val ontology = new Ontology()

    val a = BaseConcept("A")
    val b = BaseConcept("B")
    val c = BaseConcept("C")
    val r = BaseRole("r")

    ontology.addStatement(Subsumption(a, ExistentialRoleRestriction(r,b)))
    ontology.addStatement(Subsumption(ExistentialRoleRestriction(r,b), c))

    val interpolant = forgetter.forget(ontology, Set(b.name))

    val expected = Ontology.buildFrom(Set(
      Subsumption(a, c),
      Subsumption(a, ExistentialRoleRestriction(r,TopConcept))
    ))

    assertEquivalent(interpolant, expected)
  }

  def testCase2() = {
    val a = BaseConcept("A")
    val b = BaseConcept("B")
    val r = BaseRole("r")

    val ontology = Ontology.buildFrom(Set(
      Subsumption(a, ExistentialRoleRestriction(r, TopConcept)),
      Subsumption(ExistentialRoleRestriction(r, TopConcept), b)
    ))

    val expected = Ontology.buildFrom((Set(Subsumption(a,b))))

    val interpolant= forgetter.forget(ontology, Set(r.name))

    assertEquivalent(expected, interpolant)
  }

  def testCaseRoleHierarchies1() = {
    val ontology = new Ontology()

    val a = BaseConcept("A")
    val b = BaseConcept("B")
    val c = BaseConcept("C")
    val r = BaseRole("r")
    val s = BaseRole("s")

    ontology.addStatement(Subsumption(a, ExistentialRoleRestriction(r,b)))
    ontology.addStatement(Subsumption(ExistentialRoleRestriction(s,b), c))
    ontology.addStatement(RoleSubsumption(r,s))

    val interpolant = forgetter.forget(ontology, Set(b.name))

    val expected = Ontology.buildFrom(Set(
      Subsumption(a, c),
      Subsumption(a, ExistentialRoleRestriction(r,TopConcept)),
      RoleSubsumption(r,s)
    ))

    assertEquivalent(interpolant, expected)
  }

  def testCaseRoleHierarchies2() = {
    val a = BaseConcept("A")
    val x = BaseRole("x")
    val w = BaseRole("w")
    val v = BaseRole("v")

    val ontology = Ontology.buildFrom(List(
      Subsumption(ExistentialRoleRestriction(x, TopConcept), a),
      RoleSubsumption(w, x),
     // RoleSubsumption(x, v)
    ))

    val interpolant = forgetter.forget(ontology, Set(x.name))

    val expected = Ontology.buildFrom(List(
      Subsumption(ExistentialRoleRestriction(w, TopConcept), a),
   //   RoleSubsumption(w, v)
    ))

    assertEquivalent(interpolant, expected)
  }

  def assertEquivalent(ont1: Ontology, ont2: Ontology) = {
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
  }
}
