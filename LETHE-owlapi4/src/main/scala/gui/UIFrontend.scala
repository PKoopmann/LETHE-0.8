package uk.ac.man.cs.lethe.internal.application.gui


import scala.swing._
import scala.swing.event.Event

import org.semanticweb.owlapi.model._

import uk.ac.man.cs.lethe.internal.dl.datatypes._
import uk.ac.man.cs.lethe.internal.dl.owlapi.OWLApiConfiguration



object UIFrontend extends SimpleSwingApplication {

  OWLApiConfiguration.SIMPLIFIED_NAMES = false

  val ontologyInputPanel = new OntologyInputPanel()
  val signaturePanel = new CentralPanel()
  val uniformInterpolantPanel = new UniformInterpolantPanel()


  def top = new MainFrame { 
    title = "LETHE - Uniform Interpolation in Expressive Description Logics"
    contents = new SplitPane(Orientation.Vertical, 
			     ontologyInputPanel,
			     new SplitPane(Orientation.Vertical,
				       signaturePanel,
				       uniformInterpolantPanel))
    // contents = new BoxPanel(Orientation.Horizontal){ 
    //   contents += ontologyInputPanel
    //   contents += signaturePanel
    //   contents += uniformInterpolantPanel
    // }
  }


  listenTo(ontologyInputPanel, signaturePanel)

  reactions += { 
    case OWLOntologyChangedEvent(_, owlOntology) =>
      signaturePanel.setOWLOntology(owlOntology)
    
    case UniformInterpolantComputedEvent(_, ont) => 
      uniformInterpolantPanel.setUniformInterpolant(ont)
  }
}


case class OntologyChangedEvent(parent: Publisher, 
				ontology: Ontology) extends Event

case class OWLOntologyChangedEvent(parent: Publisher, 
				   ontology: OWLOntology) extends Event

case class UniformInterpolantComputedEvent(parent: Publisher,
				      interpolant: Ontology) extends Event
