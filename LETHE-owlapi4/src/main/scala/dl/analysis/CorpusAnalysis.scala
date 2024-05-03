package uk.ac.man.cs.lethe.internal.dl.filters

import java.io.File
import org.semanticweb.owlapi.model.{OWLObjectExactCardinality, OWLObjectMaxCardinality, OWLObjectMinCardinality, OWLOntology}
import org.semanticweb.owlapi.model.parameters.Imports
import uk.ac.man.cs.lethe.internal.dl.owlapi.{OWLApiConverter, OWLApiInterface}
import uk.ac.man.cs.lethe.internal.tools.statistics.IncrementalValuesCount

import scala.collection.JavaConverters.asScalaIteratorConverter
import scala.collection.mutable.HashMap

object CorpusAnalysis {
  import OWLOntologyFilters._


  var numOntologies = 0

  var individualInputValues = new HashMap[String, HashMap[String, IncrementalValuesCount]]()
  var inputValues = new HashMap[String, IncrementalValuesCount]()


  def main(args: Array[String]) = {
    val file = new File(args(0))

    if (file.isDirectory)
      analyseFolder(file)
    else {
      val ontology: OWLOntology = OWLApiInterface.getOWLOntology(file)
      if(args.size>1 && args(1)=="ALCH-TBox"){
        OWLOntologyFilters.restrictToALCH(ontology)
        OWLOntologyFilters.removeABox(ontology)
      }

      val ont = OWLApiConverter.convert(ontology)
      analyse(ontology, file.getName)
      print(file.getName+"\t axiomCount:\t "+inputValues("axioms").mean+"\t axiomSize:\t "+inputValues("axiom size").mean+
        "\t sigSize: \t" + inputValues("signature size").mean +
        "\t expr: \t")
      println(OWLFamilies.family(ont))
    }
  }

  def analyseFolder(file: File) {
    file.listFiles.foreach{ file =>
      println(file.getName)
      if(file.getName.endsWith(".xml")){
        try {
          val ontology: OWLOntology = OWLApiInterface.getOWLOntology(file)
          analyse(ontology, file.getName)
        } catch {
          case t: Throwable => println("Exception: "+t)
        }
      }
    }

    //    inputValues.foreach{ pair => println(pair._1+" "+pair._2)}
    println("Axioms: "+inputValues("axioms"))
    println("Avg. Axiom size: "+inputValues("axiom size"))
    println("Cardinality Restriction: "+inputValues("numberRes"))
  }


  def analyse(ontology: OWLOntology, ont: String) = {

    var axioms = 0;
    var alc = 0;
    var alch = 0;
    var shq = 0;
    var numberRes = 0;

    ontology.getTBoxAxioms(Imports.INCLUDED).iterator.asScala.foreach{ axiom =>{
      axioms += 1

      axiom.getNestedClassExpressions.iterator.asScala.foreach{ _ match {
        case c: OWLObjectExactCardinality if c.getCardinality>0 => numberRes+=1
        case c: OWLObjectMinCardinality if c.getCardinality>1 => numberRes +=1
        case c: OWLObjectMaxCardinality if c.getCardinality>0 => numberRes +=1
        case _ => ;
      }}

      if(alcAxiom(axiom) && axiom.getNestedClassExpressions.iterator.asScala.forall(alcClass)){
        alc += 1
        alch += 1

      }

      if(shqAxiom(axiom) && axiom.getNestedClassExpressions.iterator.asScala.forall(shqClass))
        shq += 1
    }}

    ontology.getABoxAxioms(Imports.INCLUDED).iterator.asScala.foreach{ axiom =>{
      axioms += 1
      if(alcAxiom(axiom) & axiom.getNestedClassExpressions.iterator.asScala.forall(alcClass))
        alc += 1

      axiom.getNestedClassExpressions.iterator.asScala.foreach{ _ match {
        case c: OWLObjectExactCardinality if c.getCardinality>0 => numberRes+=1
        case c: OWLObjectMinCardinality if c.getCardinality>1 => numberRes +=1
        case c: OWLObjectMaxCardinality if c.getCardinality>0 => numberRes +=1
        case _ => ;
      }}

    }}

    ontology.getRBoxAxioms(Imports.INCLUDED).iterator.asScala.foreach{ axiom => {
      axioms += 1
      if(alchAxiom(axiom))
        alch += 1
      if(shqAxiom(axiom))
        shq += 1
    }}

    addValue(ont, "axioms", axioms)
    addValue(ont, "numberRes", numberRes)

    if(axioms>0){
      addValue(ont, "alc", 1-alc.toDouble/axioms)
      addValue(ont, "alch", 1-alch.toDouble/axioms)
      addValue(ont, "shq", 1-shq.toDouble/axioms)
    }

    val ont2 = OWLApiConverter.convert(ontology)
    if(ont2.size>0)
      addValue(ont, "axiom size", ont2.size.toDouble/ont2.statements.size)
    else
      addValue(ont, key="axiom size", 0)

    addValue(ont, key="signature size", ont2.signature.size)
  }


  def addValue(ont: String, key: String, value: Double): Unit = {
    addValue(individualInputValues, ont, key, value)
    addValue(inputValues, key, value)
  }

  def addValue(map: HashMap[String, HashMap[String, IncrementalValuesCount]],
               key1: String,
               key2: String,
               value: Double): Unit = {
    if(!map.contains(key1))
      map.put(key1, new HashMap[String, IncrementalValuesCount]())

    addValue(map(key1), key2, value)
  }

  def addValue(map: HashMap[String, IncrementalValuesCount],
               key: String,
               value: Double): Unit = {

    if(!map.contains(key))
      map.put(key, new IncrementalValuesCount())

    map(key).addValue(value)
  }
}
