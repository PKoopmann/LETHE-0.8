package uk.ac.man.cs.lethe.abduction

import org.semanticweb.HermiT.ReasonerFactory
import org.semanticweb.owlapi.model.{OWLAxiom, OWLLogicalAxiom, OWLOntology}
import uk.ac.man.cs.lethe.internal.dl.datatypes.extended.{ConjunctiveDLStatement, DisjunctiveDLStatement}

/**
 * Probably redundant since I didn't want to adapt Warren's implementation, but had a few things I wanted to optimize:
 *
 * 1. simplify the way each axiom looks like
 * 2. remove axioms already entailed by the background ontology
 * 3. remove axioms entailed by the other axioms (greedy)
 */
class HypothesisSimplifier(backgroundOntology: OWLOntology) {

  var manager = backgroundOntology.getOWLOntologyManager()

  val reasoner = new ReasonerFactory().createReasoner(backgroundOntology)

  def simplify(_hypothesis: Set[OWLLogicalAxiom]) = {
    var hypothesis = _hypothesis
    var result = Set[OWLLogicalAxiom]()

    var removed = Set[OWLLogicalAxiom]()

    System.out.println("Simplifying hypotheses...")

    _hypothesis.foreach{axiom =>
      if(backgroundOntology.containsAxiom(axiom))
        hypothesis -= (axiom)
      else {
        manager.addAxiom(backgroundOntology, axiom)
        removed += (axiom)
      }
    }

    hypothesis.foreach { axiom =>
      manager.removeAxiom(backgroundOntology,axiom)
      reasoner.flush()
      if(!reasoner.isEntailed(axiom)) {
        result += (axiom)
        manager.addAxiom(backgroundOntology,axiom)
      }
    }

    removed.foreach(manager.removeAxiom(backgroundOntology,_))

    result
  }
}
