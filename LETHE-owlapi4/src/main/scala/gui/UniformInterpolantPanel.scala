package uk.ac.man.cs.lethe.internal.application.gui

import java.io.File

import javax.swing.filechooser.FileNameExtensionFilter
import uk.ac.man.cs.lethe.internal.dl.datatypes.Ontology
import uk.ac.man.cs.lethe.internal.dl.owlapi.OWLExporter

import scala.swing.{Action, BoxPanel, Button, Dialog, Dimension, FileChooser, FlowPanel, Label, Orientation}

class UniformInterpolantPanel extends BoxPanel(Orientation.Vertical){

  val ontologyView = new OntologyView(new Ontology())
  ontologyView.preferredSize = new Dimension(400, 900)

  var uniformInterpolant = new Ontology()

  val saveInterpolantButton = new Button {
    action = Action("Save Uniform Interpolant") {
      val fileChooser = new FileChooser(new File("."))
      fileChooser.fileFilter = new FileNameExtensionFilter("OWL Ontologies", "owl", "xml", "rdf")
      fileChooser.multiSelectionEnabled = false
      val res: FileChooser.Result.Value = fileChooser.showSaveDialog(ontologyView)

      if(res == FileChooser.Result.Approve) {
        try {
          val file = fileChooser.selectedFile
          val owlExporter = new OWLExporter()
          owlExporter.exportOntology(uniformInterpolant, file)
        } catch {
          case e: Exception => {
            e.printStackTrace()
            Dialog.showMessage(ontologyView,
              e.getMessage(), //e.getStackTrace().mkString("\n"),
              "Error file storing ontology",
              Dialog.Message.Error)
          }
        }
      }
    }
  }


  contents += new FlowPanel(new Label("Uniform Interpolant"))
  contents += ontologyView
  contents += new FlowPanel(saveInterpolantButton)


  def setUniformInterpolant(ont: Ontology) = {
    println(ont)
    uniformInterpolant = ont
    ontologyView.setOntology(ont)
  }
}
