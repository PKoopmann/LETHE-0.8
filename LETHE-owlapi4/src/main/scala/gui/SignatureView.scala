package uk.ac.man.cs.lethe.internal.application.gui

import org.semanticweb.owlapi.model.OWLEntity

import scala.swing.{Dimension, ListView, ScrollPane}

class SignatureView(var signature: Iterable[OWLEntity] = Set()) extends ScrollPane {

  var listView: ListView[OWLEntity] = new ListView()

  preferredSize = new Dimension(200, 600)

  def setSignature(signature: Iterable[OWLEntity]) = {
    println(signature)
    this.signature = signature

    listView = new ListView(signature.toSeq.sortBy(formatSignature)) {
      import ListView._
      renderer = Renderer(formatSignature)
      selection.intervalMode = IntervalMode.MultiInterval
    }

    contents = listView
  }

  def getSelectedSignature: Set[OWLEntity] = {
    listView.selection.items.toSet
  }

  def formatSignature(net: OWLEntity) =
    net.getIRI.getShortForm match {
      case null => net.toString
      case c => c
    }
  def formatSignature(string: String) = string.split('#').last match {
    case null => string.split(":")
    case c => c
  }
}