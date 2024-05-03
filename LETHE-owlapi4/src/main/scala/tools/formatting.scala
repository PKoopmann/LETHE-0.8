package uk.ac.man.cs.lethe.internal.tools.formatting

import scala.collection.JavaConversions._
import scala.collection.JavaConverters._
import org.semanticweb.owlapi.model._
import uk.ac.man.cs.lethe.internal.dl.owlapi.OWLApiConverter

object SimpleOWLFormatter extends SimpleOWLFormatterCl(SimpleDLFormatter)

class SimpleOWLFormatterCl(dlFormatter: SimpleDLFormatterCl) extends Formatter[OWLAxiom] {
  def format(ontology: OWLOntology): String = {
    ontology.getLogicalAxioms().iterator().asScala.map((axiom: OWLLogicalAxiom) => format(axiom)).mkString("\n")
  }

  override def format(axiom: OWLAxiom) = {

    axiom match {
      case dca: OWLDisjointClassesAxiom =>
        "disjoint"+
          dca.getClassExpressions.map(
            x => dlFormatter.format(OWLApiConverter.convert(x))).mkString("(",", ",")")

      //  case ecq: OWLEquivalentClassesAxiom =>
      //    ecq.getClassExpressions.map(x => dlFormatter.format(OWLApiConverter.convert(x))).mkString(""+SimpleDLFormatter.SQ_EQUIV)

      case ax: OWLObjectPropertyDomainAxiom =>
        "domain("+dlFormatter.format(OWLApiConverter.convert(ax.getProperty))+
          ") = "+
          dlFormatter.format(OWLApiConverter.convert(ax.getDomain))

      case ax: OWLObjectPropertyRangeAxiom =>
        "range("+dlFormatter.format(OWLApiConverter.convert(ax.getProperty))+
          ") = "+
          dlFormatter.format(OWLApiConverter.convert(ax.getRange))

      case _ => {
        val converted = OWLApiConverter.convert(axiom)
        converted.map(dlFormatter.format).mkString("\n")
      }
    }
  }
}
