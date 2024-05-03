package tests

import uk.ac.man.cs.lethe.internal.dl.datatypes.{DLStatement, Ontology}
import uk.ac.man.cs.lethe.internal.dl.logicalDifference.LogicalEntailmentCheckerClassical
import uk.ac.man.cs.lethe.internal.dl.owlapi.OWLExporter
import uk.ac.man.cs.lethe.internal.tools.formatting.SimpleOWLFormatter

import scala.collection.JavaConverters._

class LogicalTests {

  val entailmentChecker = LogicalEntailmentCheckerClassical

  val owlExporter = new OWLExporter()

  def assertEquivalent(statement1: DLStatement, statement2: DLStatement): Unit =

    assertEquivalent(Ontology.buildFrom(Set(statement1)),
      Ontology.buildFrom(Set(statement2)))

  def assertEquivalent(ont1: Ontology, ont2: Ontology): Unit = {
    val owlOnt1 = owlExporter.toOwlOntology(ont1);
    val owlOnt2 = owlExporter.toOwlOntology(ont2);

    val reasoner1 = entailmentChecker.getReasoner(owlOnt1)

    owlOnt2.getLogicalAxioms().asScala.foreach{ ax =>
      assert(entailmentChecker.isEntailed(ax, owlOnt1, reasoner1),
        ont1+"\n should entail "+SimpleOWLFormatter.format(ax))
    }

    val reasoner2 = entailmentChecker.getReasoner(owlOnt2)

    owlOnt1.getLogicalAxioms().asScala.foreach { ax =>
      assert(entailmentChecker.isEntailed(ax, owlOnt2, reasoner2),
        ont2 + "\n should entail " + SimpleOWLFormatter.format(ax))
    }
  }

  def equivalent(statement1: DLStatement, statement2: DLStatement): Boolean =
    equivalent(Ontology.buildFrom(Set(statement1)),
      Ontology.buildFrom(Set(statement2)))

  def equivalent(ont1: Ontology, ont2: Ontology): Boolean = {
    val owlOnt1 = owlExporter.toOwlOntology(ont1);
    val owlOnt2 = owlExporter.toOwlOntology(ont2);

    val reasoner1 = entailmentChecker.getReasoner(owlOnt1)

    val reasoner2 = entailmentChecker.getReasoner(owlOnt2)

    owlOnt2.getLogicalAxioms().asScala.forall{ ax =>
      entailmentChecker.isEntailed(ax, owlOnt1, reasoner1)
    } && owlOnt1.getLogicalAxioms().asScala.forall { ax =>
      entailmentChecker.isEntailed(ax, owlOnt2, reasoner2)
    }
  }
}
