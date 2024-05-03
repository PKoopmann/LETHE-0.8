package uk.ac.man.cs.lethe.internal.application.gui


import org.semanticweb.owlapi.model.parameters.Imports
import org.semanticweb.owlapi.model.{OWLAxiom, OWLEntity, OWLOntology, OWLTransitiveObjectPropertyAxiom}
import uk.ac.man.cs.lethe.internal.dl.datatypes.Ontology
import uk.ac.man.cs.lethe.internal.dl.forgetting.{ConceptAndRoleForgetter, SHQForgetter}
import uk.ac.man.cs.lethe.internal.dl.forgetting.abox.ABoxForgetter
import uk.ac.man.cs.lethe.internal.dl.forgetting.direct.FixpointApproximator
import uk.ac.man.cs.lethe.internal.dl.interpolation.OntologyInterpolator
import uk.ac.man.cs.lethe.internal.dl.owlapi.OWLApiInterface
import uk.ac.man.cs.lethe.internal.forgetting.Forgetter
import uk.ac.man.cs.lethe.internal.tools.{Cancelable, ProgressBarAttached, SwingProgressBar}
import uk.ac.man.cs.lethe.internal.tools.threads.CustomExecutionContext

import scala.collection.JavaConverters._
import scala.concurrent.future
import scala.swing.{Action, BoxPanel, Button, ButtonGroup, CheckBox, ComboBox, Dialog, FlowPanel, Label, Orientation, RadioButton}
import scala.swing.event.ButtonClicked

class CentralPanel extends BoxPanel(Orientation.Vertical) {

  var owlOntology: OWLOntology = _

  val signatureView = new SignatureView()

  contents += new FlowPanel(new Label("Signature"))
  contents += signatureView

  val alchButton = new RadioButton("ALCH TBoxes")
  val shqButton = new RadioButton("SHQ TBoxes")
  val alcButton = new RadioButton("SH with ABoxes")

  val methodGroup = new ButtonGroup(alchButton, shqButton, alcButton)
  methodGroup.select(alchButton)

  val fixpointsButton = new RadioButton("Fixpoints")
  fixpointsButton.enabled=false
  val approxButton = new RadioButton("Approximate")
  val definersButton = new RadioButton("Helper Concepts")
  val representationGroup = new ButtonGroup(fixpointsButton, approxButton, definersButton)
  representationGroup.select(definersButton)

  val disjunctionsButton = new CheckBox("Disjunctive Concept Assertions")
  val topRolesButton = new CheckBox("Universal Roles")
  topRolesButton.selected=false

  contents += new FlowPanel(
    new BoxPanel(Orientation.Vertical){
      Seq(new Label("Forgetting Method"),
        alchButton,
        shqButton,
        alcButton,
        new Label(" ")).foreach(contents+=_)
    },
    new Label("      "),
    new BoxPanel(Orientation.Vertical){
      Seq(new Label("Representation"),
        fixpointsButton,
        approxButton,
        definersButton,
        topRolesButton,
        disjunctionsButton).foreach(contents+=_)
    })

  val approxLabel = new Label("Approximate Fixpoints to ")
  val approxField = new ComboBox((0 to 10))

  approxLabel.enabled = approxButton.selected
  approxField.enabled = approxButton.selected
  contents += new FlowPanel(approxLabel, approxField)

  val transButton = new CheckBox("Ignore transitivity axioms")
  transButton.enabled = alcButton.selected
  contents += new FlowPanel(transButton)

  val delayedExistsButton = new CheckBox("Delayed Existential Role Restriction Elimination")
  contents += new FlowPanel(delayedExistsButton)

  listenTo(fixpointsButton,
    approxButton,
    definersButton,
    alcButton,
    alchButton,
    shqButton)

  reactions += {
    case _: ButtonClicked  =>
      approxLabel.enabled = approxButton.selected
      approxField.enabled = approxButton.selected
      transButton.enabled = alcButton.selected
  }

  def pub(o: Ontology) = publish(UniformInterpolantComputedEvent(this, o))

  implicit val exec = new CustomExecutionContext()

  val forgettingButton = new Button {

    action = Action("Compute Uniform Interpolant") {


      val forgetter: Forgetter[Ontology, String] with ProgressBarAttached =
        methodGroup.selected match {
          case Some(a) if a==alchButton => ConceptAndRoleForgetter
          case Some(a) if a==shqButton => SHQForgetter
          case Some(a) if a==alcButton => ABoxForgetter
          case _ => assert(false); ConceptAndRoleForgetter
        }

      forgetter match {
        case cancelable: Cancelable => cancelable.uncancel
        case _ => ;
      }

      ABoxForgetter.eliminateDisjunctions = !disjunctionsButton.selected

      if(forgetter == ABoxForgetter && transButton.selected)
        removeTransitivityAxioms()

      assert(ConceptAndRoleForgetter.filter==false)

      val interpolator = new OntologyInterpolator(forgetter,
        universalRoles =  topRolesButton.selected, delayedExistsElimination = delayedExistsButton.selected)

      val progressBar = new SwingProgressBar()
      progressBar.title = "Computing Uniform Interpolant"

      forgetter.progressBar = progressBar

      // val progressBarDialog = new Dialog() {
      // 	contents = progressBar.progressBarComponent
      // }
      //progressBarDialog.open


      val futureInterp = future {
        try{
          var result = interpolator.uniformInterpolantInternalFormat(owlOntology,
            signatureView.getSelectedSignature)

          if(approxButton.selected)
            result = FixpointApproximator.approximate(result,
              approxField.selection.item)

          reAddTransitivityAxioms()

          pub(result)
          progressBar.close()
        } catch {
          case _: ThreadDeath => ;
          case e: Throwable =>
            e.printStackTrace()
            Dialog.showMessage(signatureView,
              e.getMessage(), //e.getStackTrace().mkString("\n"),
              "Error while Computing Uniform Interpolant",
              Dialog.Message.Error)
            reAddTransitivityAxioms()
        }
      }

      progressBar.cancelButton.action = Action("Cancel") {
          interpolator.cancel
        reAddTransitivityAxioms()
      }

    }
  }

  var removedTransitivityAxioms = Set[OWLAxiom]()

  def removeTransitivityAxioms() = {
    removedTransitivityAxioms =
      owlOntology.getRBoxAxioms(Imports.INCLUDED)
        .asScala
        .filter(_.isInstanceOf[OWLTransitiveObjectPropertyAxiom]).toSet

    owlOntology.getOWLOntologyManager.removeAxioms(owlOntology,removedTransitivityAxioms.asJava)

  }

  def reAddTransitivityAxioms() = {
    owlOntology.getOWLOntologyManager.addAxioms(owlOntology,removedTransitivityAxioms.asJava)
  }


  contents += new FlowPanel(forgettingButton)


  def setOWLOntology(owlOntology: OWLOntology) = {
    signatureView.setSignature(OWLApiInterface.getSignature(owlOntology))
    this.owlOntology = owlOntology
  }

  def setSignature(sig: Iterable[OWLEntity]) =
    signatureView.setSignature(sig)

}
