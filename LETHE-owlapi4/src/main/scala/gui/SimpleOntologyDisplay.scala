package uk.ac.man.cs.lethe.internal.application.gui

import scala.swing.{MainFrame, SimpleSwingApplication}


object SimpleOntologyDisplay extends SimpleSwingApplication {


  def top = new MainFrame {
    title = "No Ontology Selected"
    contents = new OntologyInputPanel()
    centerOnScreen()
  }
}
