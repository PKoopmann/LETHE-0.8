package uk.ac.man.cs.lethe.internal.dl.abduction.filtering;

import org.semanticweb.HermiT.Reasoner;
import org.semanticweb.owlapi.apibinding.OWLManager;
import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.reasoner.OWLReasoner;
import scala.collection.JavaConverters;
import uk.ac.man.cs.lethe.internal.dl.datatypes.Ontology;
import uk.ac.man.cs.lethe.internal.dl.datatypes.DLStatement;
import uk.ac.man.cs.lethe.internal.dl.abduction.FixpointEliminator;
import uk.ac.man.cs.lethe.internal.dl.owlapi.OWLExporter;
import java.util.*;


public class StrongRedundancyChecker {

    public Ontology checkRedundancy(Ontology backgroundOntology, Ontology unfilteredForgettingResult) {
        List<DLStatement> unfilteredAxioms = new ArrayList<DLStatement>(JavaConverters.asJavaCollection(unfilteredForgettingResult.statements()));
        Map<DLStatement, HashSet<OWLAxiom>> statementsToAxiomsMap = new HashMap<DLStatement, HashSet<OWLAxiom>>();

        OWLExporter exporter = new OWLExporter();
        Set<OWLAxiom> unfilteredAxiomsInOWL = new HashSet<OWLAxiom>();
        for(DLStatement statement:unfilteredAxioms) {
            HashSet<OWLAxiom> axInOWL = new HashSet<OWLAxiom>();
            axInOWL.addAll((exporter.toOwlOntology(Ontology.buildFrom(FixpointEliminator.eliminateFixpoints(statement)))).getAxioms());
            statementsToAxiomsMap.put(statement, axInOWL);
            unfilteredAxiomsInOWL.addAll(axInOWL);
        }

        //check for nominals / inverse roles in unfiltered hypothesis
        boolean nominalOrInverseUnfiltered = false;
        for(OWLAxiom axiom:unfilteredAxiomsInOWL) {
            for(OWLClassExpression exp:axiom.getNestedClassExpressions()) {
                if(exp instanceof OWLObjectOneOf || exp instanceof OWLObjectInverseOf) {
                    nominalOrInverseUnfiltered = true;
                    break;
                }
            }
        }
        //System.out.println("Nominals/inverse in initial hypothesis: " + nominalOrInverseUnfiltered);

        OWLOntology backgroundOntologyOWL = exporter.toOwlOntology(backgroundOntology);
        OWLOntologyManager man = OWLManager.createOWLOntologyManager();
        man.addAxioms(backgroundOntologyOWL, unfilteredAxiomsInOWL);

        Set<DLStatement> filteredAxioms = new HashSet<DLStatement>();
        Set<OWLAxiom> filteredOWLAxioms = new HashSet<OWLAxiom>();
        for(DLStatement ax:statementsToAxiomsMap.keySet()) {
            HashSet<OWLAxiom> axiomsBeingChecked = statementsToAxiomsMap.get(ax);
            man.removeAxioms(backgroundOntologyOWL, axiomsBeingChecked);
            OWLReasoner reasoner = new Reasoner.ReasonerFactory().createReasoner(backgroundOntologyOWL);

            if (!reasoner.isEntailed(axiomsBeingChecked)) {
                filteredAxioms.add(ax);
                filteredOWLAxioms.addAll(axiomsBeingChecked);
                man.addAxioms(backgroundOntologyOWL, axiomsBeingChecked);
            }
        }

        //check filtered axioms for nominals / inverse
        boolean nominalOrInverseFiltered = false;
        for(OWLAxiom axiom:filteredOWLAxioms) {
            for(OWLClassExpression exp:axiom.getNestedClassExpressions()) {
                if(exp instanceof OWLObjectOneOf || exp instanceof OWLObjectInverseOf) {
                    nominalOrInverseFiltered = true;
                    break;
                }
            }
        }
        //System.out.println("Nominals/inverse in filtered hypothesis: " + nominalOrInverseFiltered);

        Ontology filteredResult = Ontology.buildFrom(JavaConverters.asScalaSet(filteredAxioms));

        return filteredResult;
       // return backgroundOntology;
    }
}
