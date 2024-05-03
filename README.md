# LETHE: A Tool for Uniform Interpolation and Related Reasoning Tasks

LETHE is a tool for uniform interpolation and related tasks for OWL ontologies using expressive description logics. The current version comes with functionality for computing uniform interpolants, logical differences and KB abduction, and can be used as a standalone tool and as library for Java and Scala.

At the core of LETHE is the computation of so-called uniform interpolants. A uniform interpolant is a restricted version of an ontology that only uses a specified set of concept and role names, while preserving all logical entailments over this set. So far, the description logics ALC, ALCH and SHQ are supported, where forgetting role names is not supported for SHQ yet and interpolating ABoxes is only supported for ALC ontologies. If an input ontology contains axioms that are not expressible in the respective description logic, they will be removed before the algorithm is applied.

If the input ontology is cyclic, the uniform interpolant cannot always be represented finitely without fixpoint operators. Since fixpoint operators are not supported by OWL, the prototype might add additional concept names to the output ontology that simulate the behaviour of fixpoint operators. Another possibility is to approximate the uniform interpolant based on a given parameter.

This page provides the download of LETHE, and describes how to compute uniform interpolants per command line, graphical interface or using LETHE as a library. It further describes how to use the library for computing logical differences and performing TBox abduction.
More information and technical details of the underlying methods can be found in the following publications:


- Koopmann, P.:  LETHE: Forgetting and uniform interpolation for expressive description logics. KI-Künstliche Intelligenz 34.3 (2020): 381-387.
- Koopmann, P., et al.: Signature-based abduction for expressive description logics. In: Proc. KR'20. AAAI Press (2020).
- Koopmann, P., Schmidt, R.A.: Uniform Interpolation and Forgetting for ALC Ontologies with ABoxes. In: Proc. AAAI-15. AAAI Press (2015).
- Koopmann, P., Schmidt, R.A.: Count and Forget: Uniform Interpolation of SHQ-Ontologies. In: Proc. IJCAR'14. Springer (2014) 
- Koopmann, P., Schmidt, R.A.: Forgetting Concept and Role Symbols in ALCH-Ontologies. In: Proc. LPAR'13. Springer (2013).


If you have any queries, suggestions or noticed any problems or bugs, please send me a message: p.k.koopmann@vu.nl

## Directly Using LETHE 0.6 as Standalone tool

The folder lethe-standalone-0.6 contains the precompiled fat jar file and scripts to direcly use the main functionalities from command line. 
All scripts can be called without arguments to get usage instructions:
- lethe-ui.sh --- graphical user interface to LETHE (see screenshot)
- forget.sh --- computing uniform interpolants
- logicalDifference.sh --- computing logical differences between ontology versions

## Using LETHE-abduction from command line

Go to the folder LETHE-abduction-owlapi4 and compile a fat jar using:

    mvn package assembly:single

This creates a fat jar in the target sub-folder. Execute it as follows to get instructions on how to use it:

    java -cp target/lethe-abduction-owlapi4_2.12-0.8-SNAPSHOT-jar-with-dependencies.jar uk.ac.man.cs.lethe.abduction.cmd.AbductionCmd

## Compiling LETHE 0.8 to use with maven

LETHE can be compiled both with maven and with sbt. For maven, use the following command:

    mvn install

## Maven Dependencies

After installing 0.8, add one of the following dependencies, depending on whether you use OWL API 5 or OWL API 4:

After installation, one of the following maven dependencies should be added to your project, depending on whether you are using OWL API 4 or OWL API 5:

    <dependency>
        <groupId>de.tu-dresden.inf.lat</groupId>
        <artifactId>lethe-owlapi4_2.12</artifactId>
        <version>0.8-SNAPSHOT</version>
    </dependency>
	    
	 <dependency>
        <groupId>de.tu-dresden.inf.lat</groupId>
        <artifactId>lethe-owlapi5_2.12</artifactId>
        <version>0.8-SNAPSHOT</version>
    </dependency>

To use LETHE-abduction (which relies on different reasoning algorithms), add instead one of the following dependencies:
	 
    <dependency>
        <groupId>de.tu-dresden.inf.lat</groupId>
        <artifactId>lethe-abduction-owlapi4_2.12</artifactId>
        <version>0.8-SNAPSHOT</version>
    </dependency>
	    
	 <dependency>
        <groupId>de.tu-dresden.inf.lat</groupId>
        <artifactId>lethe-abduction-owlapi5_2.12</artifactId>
        <version>0.8-SNAPSHOT</version>
    </dependency>


## Usage of The Library: Uniform Interpolation
Depending whether the user wants to specify the signature of the uniform interpolant or a set of symbols to be forgotten, the LETHE offers two interfaces, IOWLInterpolator and IOWLForgetter. Both take as input an ontology object and a list of symbols, which are represented using the data types of the OWL API (see http://owlapi.sourceforge.net/ for further information). Depending on the expressivity of the input ontology, LETHE offers several implementations of these interfaces: the classes AlchTBoxInterpolator, ShqTBoxInterpolator and AlcOntologyInterpolator for uniform interpolation, and the classes AlchTBoxForgetter, ShqTBoxForgetter and AlcOntologyForgetter for forgetting. Axioms present in the ontology that are not supported by the method used will be removed by the implementation. Only AlcOntologyInterpolator and AlcOntologyForgetter support ABoxes, and ShqTBoxInterpolator and ShqTBoxForgetter do not support forgetting roles yet. Here is how the library is used in Java, exemplary for the case of a TBox in the description logic ALCH:

    import uk.ac.man.cs.koopmanp.IOWLInterpolator
    import uk.ac.man.cs.koopmanp.IOWLForgetter
    import uk.ac.man.cs.koopmanp.AlchTBoxInterpolator
    import uk.ac.man.cs.koopmanp.AlchTBoxForgetter
    
    import org.semanticweb.owlapi.model.OWLEntity
    import org.semanticweb.owlapi.model.OWLOntology
    
    
    ...
    
    OWLOntology inputOntology;
    Set<OWLEntity> symbols;
    
    ....
    
    // computing the uniform interpolant over a given set of concept symbols:
    IOWLInterpolator interpolator = new AlchTBoxInterpolator();
    OWLOntology interpolant = interpolator.uniformInterpolant(inputOntology, symbols);
   
    
    // forgetting a set of concept symbols from a given ontology:
    IOWLForgetter forgetter = new AlchTBoxForgetter();
    OWLOntology forgettingResult = forgetter.forget(inputOntology, symbols);

## Usage of the Library: Logical Difference
Logical Differences are computed using the class LogicalDifferenceComputer, which is instantiated using an IOWLInterpolator object. It is used as follows:

    import uk.ac.man.cs.koopmanp.IOWLInterpolator
    import uk.ac.man.cs.koopmanp.LogicalDifferenceComputer
    
    import org.semanticweb.owlapi.model.OWLLogicalAxiom
    import org.semanticweb.owlapi.model.OWLEntity
    import org.semanticweb.owlapi.model.OWLOntology
    
    
    ...
   
    
    OWLOntology ontology1;
    OWLOntology ontology2;
    Set<OWLEntity> signature;
    int approximationLevel = 10;
    
    IOWLInterpolator interpolator;
    
    
    // Initialise values
    
    ...
    
    
    LogicalDifferenceComputer ldc = new LogicalDifferenceComputer(interpolator);
    
    // Compute Logical Difference for common signature of the two ontologies:
    Set<OWLLogicalAxiom> diff = ldc.logicalDifference(ontology1, ontology2, approximationLevel)
    
    // Compute Logical Difference for specified signature
    Set<OWLLogicalAxiom> diff = ldc.logicalDifference(ontology1, ontology2, signature, approximationLevel)

## Usage of the Library: KB Abduction

For this, you need to add the LETHE-abduction dependency (see above). 

      import uk.ac.man.cs.lethe.abduction.OWLAbducer


      import org.semanticweb.owlapi.model.OWLAxiom
      import org.semanticweb.owlapi.model.OWLEntity
      import org.semanticweb.owlapi.model.OWLOntology


      ...

      OWLOntology ontology;
      Set<OWLEntity> abducibles;
      Set<OWLAxiom> observation;

      ...


      OWLAbducer abducer = new OWLAbducer();
      abducer.setBackgroundOntology(ontology);
      abducer.setAbducibles(abducibles);
      
      DLStatement hypotheses = abducer.abduce(observation); // compute hypotheses as DLStatement object

      abducer.formatAbductionResult(observation); // output the hypotheses in DL syntax to the standard output
      
      
      
