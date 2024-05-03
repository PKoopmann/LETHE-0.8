package uk.ac.man.cs.lethe.internal.dl

import com.typesafe.scalalogging.Logger
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.OWLClassExpression
import org.semanticweb.owlapi.reasoner.InconsistentOntologyException
import uk.ac.man.cs.lethe.internal.dl.datatypes.{Concept, ConceptConjunction, Ontology, Subsumption, TopConcept}
import uk.ac.man.cs.lethe.internal.dl.forgetting.direct.ConceptClause
import uk.ac.man.cs.lethe.internal.dl.owlapi.{OWLExporter, OWLReasonerFacade}

import java.io.File

object HermitMappedReasonerFactory extends AbstractMappedReasonerFactory {
  override def newReasoner(ontology: Ontology): AbstractMappedReasoner =
    new HermitMappedReasoner(ontology)
}

class HermitMappedReasoner(ontology: Ontology) extends AbstractMappedReasoner(ontology: Ontology) {

  val logger = Logger[HermitMappedReasoner]

  val manager = OWLManager.createOWLOntologyManager()

  val factory = manager.getOWLDataFactory()

  val owlOntology = manager.createOntology()

  val owlexporter = new OWLExporter()

  ontology.tbox.axioms.foreach(axiom =>
      manager.addAxiom(owlOntology, owlexporter.toOwl(owlOntology, axiom)))

  ontology.rbox.axioms.foreach(axiom =>
      manager.addAxiom(owlOntology, owlexporter.toOwl(owlOntology, axiom)))

  val reasoner = OWLReasonerFacade.createReasoner(owlOntology)

  def addClause(clause: ConceptClause): Unit = {
      val axiom = Subsumption(TopConcept, clause.convertBack)
      owlexporter.addAxiom(owlOntology, axiom)
  }


  override def isSatisfiable(concepts: Set[Concept]): Boolean = {

    logger.debug("Checking satisfiability of " + concepts)

    satisfiable.get(concepts) match {
      case Some(result: Boolean) => return result //logger.trace(s"${concepts} known: ${result}"); result
      case None => {
        val known = concepts.subsets.collectFirst {
          case subset if satisfiable.contains(subset) && !satisfiable(subset) => false
        }
        if (known == Some(false))
          return false
        else {
          val expression = owlexporter.toOwl(owlOntology, ConceptConjunction(concepts.toSet[Concept]))
          val result = owlIsSatisfiable(expression)
          satisfiable.put(concepts, result)
          logger.trace(s"isSatisfiable(${concepts}) = ${result}")
          logger.trace(s"cache size: ${satisfiable.size}")
          //cache subset's results
          if (result == false)
            concepts.subsets.foreach { s => if (!satisfiable.contains(s)) isSatisfiable(s) }
          return result
        }
      }
    }
  }

    def owlIsSatisfiable(expression: OWLClassExpression) = {
      logger.debug("Checking satisfiability of "+expression)
      try{
        val result = reasoner.isSatisfiable(expression)
        logger.debug("Got "+result)
        result
      } catch {
        case e: InconsistentOntologyException => {
          println("Inconsistent ontology! See debugging.owl")
          owlexporter.save(owlOntology, new File("debugging.owl"))
          throw e
        }
      }
    }
}
