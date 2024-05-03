package uk.ac.man.cs.lethe.internal.dl

import java.io.File

import com.typesafe.scalalogging.Logger
//import org.semanticweb.owlapi.apibinding.OWLManager
//import org.semanticweb.owlapi.model.OWLClassExpression
//import org.semanticweb.owlapi.reasoner.InconsistentOntologyException
import uk.ac.man.cs.lethe.internal.dl.datatypes.{ABox, Axiom, Concept, ConceptConjunction, Ontology, RBox, Subsumption, TBox, TopConcept}
import uk.ac.man.cs.lethe.internal.dl.forgetting.direct.{ConceptClause, SimpleDefinerEliminator}
//import uk.ac.man.cs.lethe.internal.dl.owlapi.{OWLExporter, OWLReasonerFacade}

import scala.collection.mutable.HashMap

trait AbstractMappedReasonerFactory {
  def newReasoner(ontology: Ontology): AbstractMappedReasoner
  def newReasoner(clauses: Iterable[ConceptClause],
                  axioms: Set[Axiom] = Set(),
                  rbox: RBox = RBox(Set())): AbstractMappedReasoner = {
    newReasoner(new Ontology(new TBox(axioms ++ clauses.map{cl =>
      Subsumption(TopConcept, cl.convertBack)
    }.toSet[Axiom]),
      new ABox(Set()),
      rbox))
  }
}

abstract class AbstractMappedReasoner(ontology: Ontology) {
  def addClause(clause: ConceptClause)


  private val logger = Logger[AbstractMappedReasoner]

  logger.debug("Mapped reasoner initialised with: ")
  logger.debug(s"${uk.ac.man.cs.lethe.internal.tools.formatting.SimpleDLFormatter.format(ontology)}")

  def this(tbox: TBox, rbox: RBox) = this(new Ontology(tbox=tbox, rbox=rbox))

  def this(clauses: Iterable[ConceptClause], rbox: RBox) =
    this(new Ontology(new TBox(clauses.map{cl =>
      Subsumption(TopConcept, cl.convertBack)
      }.toSet[Axiom]),
      new ABox(Set()),
      rbox))

  def this(clauses: Iterable[ConceptClause], axioms: Set[Axiom], rbox: RBox) =
    this(new Ontology(new TBox(axioms++
      clauses.map{cl =>
        Subsumption(TopConcept, cl.convertBack)
      }.toSet[Axiom]),
      new ABox(Set()),
      rbox))


  def this(clauses: Iterable[ConceptClause]) = this(clauses, RBox(Set()))


  val satisfiable = new HashMap[Iterable[Concept], Boolean]()

  def isSatisfiable(concept: Concept): Boolean = isSatisfiable(Set(concept))

  /**
   * Checks whether the given set of concepts is conjunctively satisfiable
   */
  def isSatisfiable(concepts: Set[Concept]): Boolean

  /*= {
//
//    logger.debug("Checking satisfiability of "+concepts)
//
//    satisfiable.get(concepts) match {
//      case Some(result: Boolean) => result//logger.trace(s"${concepts} known: ${result}"); result
//      case None => {
//        val known = concepts.subsets.collectFirst{
//          case subset if satisfiable.contains(subset) && !satisfiable(subset) => false
//        }
//        if(known==Some(false))
//          false
//        else {
//          val expression = owlexporter.toOwl(owlOntology, ConceptConjunction(concepts.toSet[Concept]))
//          val result = owlIsSatisfiable(expression)
//          satisfiable.put(concepts, result)
//          logger.trace(s"isSatisfiable(${concepts}) = ${result}")
//          logger.trace(s"cache size: ${satisfiable.size}")
//          //cache subset's results
//          if(result==false)
//            concepts.subsets.foreach{ s => if(!satisfiable.contains(s)) isSatisfiable(s) }
//          result
//        }
//      }
//    }
//    true
  }*/

//  def owlIsSatisfiable(expression: OWLClassExpression) = {
//    logger.debug("Checking satisfiability of "+expression)
//    try{
//      val result = reasoner.isSatisfiable(expression)
//      logger.debug("Got "+result)
//      result
//    } catch {
//      case e: InconsistentOntologyException => {
//        println("Inconsistent ontology! See debugging.owl")
//        owlexporter.save(owlOntology, new File("debugging.owl"))
//        throw e
//      }
//    }
//  }

//  def addClause(clause: ConceptClause): Unit = {
//    val axiom = Subsumption(TopConcept, clause.convertBack)
//    owlexporter.addAxiom(owlOntology, axiom)
//  }

}
