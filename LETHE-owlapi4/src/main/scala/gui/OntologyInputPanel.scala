package uk.ac.man.cs.lethe.internal.application.gui

import java.io.File

import javax.swing.filechooser.FileNameExtensionFilter
import org.semanticweb.owlapi.model.OWLOntology

import uk.ac.man.cs.lethe.internal.dl.datatypes.Ontology
import uk.ac.man.cs.lethe.internal.dl.owlapi.{OWLApiConverter, OWLApiInterface}

import scala.swing.{Action, BoxPanel, Button, Dialog, Dimension, FileChooser, FlowPanel, Label, Orientation, Publisher}

class OntologyInputPanel extends BoxPanel(Orientation.Vertical) with Publisher {

  val ontologyView = new OntologyView(new Ontology)
  ontologyView.preferredSize = new Dimension(400, 900)


  def ontology = ontologyView.ontology
  var ontologyName = ""

  val outher = this

  def pub(ow: OWLOntology) = publish(OWLOntologyChangedEvent(this, ow))

  val openOntologyButton = new Button {
    action = Action("Open Ontology") {


      val fileChooser = new FileChooser(new File("."))
      fileChooser.fileFilter = new FileNameExtensionFilter("OWL Ontologies", "owl", "xml", "rdf")
      fileChooser.multiSelectionEnabled = false
      val res: FileChooser.Result.Value = fileChooser.showOpenDialog(ontologyView)

      if(res == FileChooser.Result.Approve) {
        try {
          val file = fileChooser.selectedFile

          val owlOntology = OWLApiInterface.getOWLOntology(file)

          pub(owlOntology)


          val ontology = OWLApiConverter.convert(owlOntology)

          ontologyName = file.getName

          ontologyView.setOntology(ontology)
        } catch {
          case e: Exception => {
            e.printStackTrace()
            Dialog.showMessage(ontologyView,
              e.getMessage(), //e.getStackTrace().mkString("\n"),
              "Error file parsing ontology",
              Dialog.Message.Error)
          }
        }
      }
    }
  }


  contents += new FlowPanel(new Label("Input Ontology"))
  contents += ontologyView
  contents += new FlowPanel(openOntologyButton)


  listenTo(ontologyView)
  reactions += {
    case OntologyChangedEvent(parent, ont) if parent!=this =>
      publish(OntologyChangedEvent(this, ont))
  }
}