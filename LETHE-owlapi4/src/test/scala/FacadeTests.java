//package tests;
//
//import java.io.File;
//import java.io.InputStream;
//import java.util.Set;
//import java.util.Collections;
//
//import org.semanticweb.owlapi.model.IRI;
//import org.semanticweb.owlapi.model.OWLDataFactory;
//import org.semanticweb.owlapi.model.OWLEntity;
//import org.semanticweb.owlapi.model.OWLOntology;
//import uk.ac.man.cs.lethe.interpolation.ITimeoutableOWLInterpolator;
//import uk.ac.man.cs.lethe.interpolation.ShKnowledgeBaseInterpolator;
//import uk.ac.man.cs.lethe.internal.dl.owlapi.OWLApiInterface;
//
//public class FacadeTests {
//
//    public static void main(String[] args)  {
//        System.out.println("Ho!");
//
//        //InputStream ontologyStream = FacadeTests.class.getResourceAsStream("/pizza.owl");
//        //val sigStream = getClass.getResourceAsStream("/bottom-example-sig.owl")
//        File file = new File("pizza.owl");
//
//        OWLOntology ontology = OWLApiInterface.getOWLOntology(file, true);
//
//        OWLDataFactory dataFactory = ontology.getOWLOntologyManager().getOWLDataFactory();
//
//        Set<OWLEntity> signature = Collections.singleton(
//                dataFactory.getOWLClass(
//                        IRI.create("http://www.co-ode.org/ontologies/pizza/pizza.owl#CheeseyVegetableTopping")));
//
//        ITimeoutableOWLInterpolator forgetter = new ShKnowledgeBaseInterpolator();
//
//        forgetter.useTimeout(5000);
//
//        forgetter.uniformInterpolant(ontology, signature);
//
//    }
//}
