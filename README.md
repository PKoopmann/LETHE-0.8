Compiling LETHE and publishing it to your local repository
-----------------------------------------------------------

You need SBT to compile this project. Find it here:

https://www.scala-sbt.org/

To generate a fat jar of the project (a jar file including all needed 
dependencies), call from the command line:

sbt assembly

To compile and publish for use with Ivy (for example to use in another Scala 
project), call:

sbt publishLocal

To compile and publish for use with Maven (if you want to use it within a Java
project), use:

sbt publishM2

To specify the dependency in your project that is using LETHE, check the file 
"build.sbt" for the organization and artifact name, as well as for the current 
version. 

To run tests via Maven use:
mvn clean install -DskipTests=false

To compile and publish via Maven use:
mvn clean install

To compile and publish an uber-Jar via Maven use:
mvn clean assembly:single


Using LETHE to compute logical differences
-------------------------------------------
The following class is used to compute logical differences:

uk.ac.man.cs.lethe.logicalDifference.LogicalDifferenceComputer

The constructor takes as argument an IOWLInterpolator-object, which can be an 
instance of one of the following three:

uk.ac.man.cs.lethe.AlchTBoxInterpolator

uk.ac.man.cs.lethe.AlcOntologyInterpolator (which actually currently supports 
                                                                  SH ontologies)
                                                                  
uk.ac.man.cs.lethe.ShqTBoxInterpolator (which does not support signatures that 
                                                  do not contain all role names)


(I am currently using AlchTBoxInterpolator for my module extraction 
implementations). 

The LogicalDifferenceComputer than has the following methods:


logicalDifference(OWLOntology ontology1, 
                  OWLOntology ontology2, 
                  int approximationLevel)

logicalDifference(OWLOntology ontology1, 
                  OWLOntology ontology2, 
                  Set<OWLEntity> signature,  
                  int approximationLevel)

The approximation level is only relevant in case the corresponding uniform 
interpolant uses fixpoint expressions/definers, and denotes the role depth of 
into which fixpoints are approximated when computing the logical difference.

