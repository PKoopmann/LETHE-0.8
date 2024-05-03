package uk.ac.man.cs.lethe.internal.application.gui

import uk.ac.man.cs.lethe.internal.dl.datatypes.{BaseConcept, Concept, ConceptEquivalence, DLStatement, Ontology}
import uk.ac.man.cs.lethe.internal.tools.formatting.{Formatter, SimpleDLFormatter}

import scala.swing.{ListView, Publisher, ScrollPane}

class OntologyView(var ontology: Ontology,
                   formatter: Formatter[_ >: DLStatement] = SimpleDLFormatter)
  extends ScrollPane with Publisher {

  import ListView._

  var listView = new ListView[DLStatement]()

  setOntology(ontology)

  def setOntology(ontology: Ontology) = {
    this.ontology = ontology

    val tboxAxioms = ontology.tbox.axioms.map(_ match {
      case ConceptEquivalence(c: Concept, b: BaseConcept) => ConceptEquivalence(b, c)
      case c => c
    })

    var statements = tboxAxioms.toSeq.sortBy(x => formatter.format(x)).map(_.asInstanceOf[DLStatement])
    statements ++= ontology.rbox.axioms.toSeq.sortBy(x => formatter.format(x))
    statements ++= ontology.abox.assertions.toSeq.sortBy(x => formatter.format(x))

    listView = new scala.swing.ListView(statements) {
      renderer = Renderer(x => formatter.format(x))
      selection.intervalMode = IntervalMode.MultiInterval
    }

    contents = listView

    publish(OntologyChangedEvent(this, ontology))
  }
}