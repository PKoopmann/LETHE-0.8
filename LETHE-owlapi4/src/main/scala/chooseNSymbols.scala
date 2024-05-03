package uk.ac.man.cs.lethe.internal.application

import java.io.{File, FileOutputStream, PrintWriter}
import java.util.Date

import org.semanticweb.owlapi.model._
import org.semanticweb.owlapi.model.parameters.Imports

import scala.collection.JavaConverters._
import scala.collection.mutable.HashMap
import scala.io.Source
import uk.ac.man.cs.lethe.internal.dl.forgetting.ConceptAndRoleForgetter
import uk.ac.man.cs.lethe.internal.dl.interpolation.OntologyInterpolator
import uk.ac.man.cs.lethe.internal.dl.owlapi.{ModuleExtractor, OWLApiConverter, OWLApiInterface}
import uk.ac.man.cs.lethe.internal.tools.MultiSet


object SymbolSelector { 
  def main(args: Array[String]) = { 
    val ont = OWLApiInterface.getOWLOntology(new File(args(0)))
    val num = args(1).toInt

    var c = 0
    println("By Occurences: ")
    SymbolSelectorByOccurences.selectNSymbols(ont, num).foreach{ p => c+=1; println(c+". "+p._1+" "+p._2) }
    println()

    c = 0
    println("By Modules: ")
    SymbolSelectorByModules.selectNSymbols(ont, num).foreach{ p => c+=1; println(c+". "+p._1+"   "+p._2) }
    println()
  }
}

object InterpolateForMax { 
  
  def main(args: Array[String]) = { 

    val forgetter = ConceptAndRoleForgetter
    val interpolator = new OntologyInterpolator(forgetter)

    val selector: SymbolSelector = 
      if(args(0)=="occ")
	SymbolSelectorByOccurences
      else if(args(0)=="mod")
	SymbolSelectorByModules
      else { 
	assert(false)
	null
      }

    val numSymbols = args(1).toInt

    new File("../../ontologies/bioportal_full").listFiles.sortBy(_.length).foreach{ file => { 
      println("File: "+file)

      if(file.getName.endsWith(".xml")){ 

	val ont = OWLApiInterface.getOWLOntology(file)

	val sig = selector.selectNSymbols(ont, numSymbols).map(_._1).toSet

	val start = new Date().getTime
	val result = interpolator.uniformInterpolantInternalFormat(ont, sig)
	val duration = (new Date()).getTime - start

	val ontt = OWLApiConverter.convert(ont)
	println("Input num. statements: "+ ontt.statements.size)
	println("Input avg. statement size: "+ontt.size.toDouble/ontt.statements.size)
	println()
	println("Duration: "+duration)
	println()
	println("Interpolant num statements: "+result.statements.size)
	println("Interpolant avg. statement size: "+(if(result.statements.size==0)0 else result.size.toDouble/result.statements.size))
	println()
	println()
      }
    }}
  }
}

object CountAll { 
  def main(args: Array[String]) = { 
    val (selector: SymbolSelector, avgFile: String) = 
      if(args(0)=="occ")
	(SymbolSelectorByOccurences, "symbolOccurrencesStats_bioportal_avg.dat") 
      else if (args(0)=="mod")
	(SymbolSelectorByModules, "symbolModulesStats_bioportal_avg.dat")

    count(selector, avgFile)
  }

  def count(selector: SymbolSelector, avgFile: String) { 
    val sums = MultiSet[Int]()

    var fileCount = 0

    new File("../../ontologies/bioportal_full").listFiles.sortBy(_.length).foreach{ file => { 
      println("File: "+file)
      if(file.getName.endsWith(".xml")){ 

	fileCount += 1

	val ont = OWLApiInterface.getOWLOntology(file)
	
	var counter = 0
	var last = 0
	selector.getList(ont).foreach{ pair => { 
	  counter += 1
	  val newVal = pair._2
	  sums.add(counter, newVal)
	  if(newVal!=last){ 
	    println((counter-1)+" "+last) 
	    println(counter+" "+newVal)
	    last = newVal
	  }
	}}
	println(counter+" "+last)// print last value with last number
      }
    }}

    val averages = HashMap[Int, Double]()

    sums.foreach{ pair => { 
      averages.put(pair._1, pair._2.toDouble/fileCount)
    }}


    // repeat printing process for averages
    val pr1 = new PrintWriter(new FileOutputStream(new File(avgFile), false))

    var last = 0.0
    averages.toSeq.sortBy(_._1).foreach{ pair => { 
      if(pair._2!=last){ 
	pr1.println((pair._1-1)+" "+last) 
	pr1.println(pair._1+" "+pair._2)
	last = pair._2
      }
    }}
    pr1.close()
  }
}

/**
 * Abstract for selecting a specified number of symbols from an owl ontologies signature
 */
abstract class SymbolSelector { 
  def getList(ontology: OWLOntology): Seq[(OWLEntity, Int)]

  def selectNSymbols(ontology: OWLOntology, number: Int): Seq[(OWLEntity, Int)] =
    getList(ontology).take(number)
}

/**
 * Select symbols based on the number of genuine modules they occur in.
 * Idea: This number reflects the number of axioms that may directly or indirectly
 *  interact with the symbol.
 */
object SymbolSelectorByModules extends SymbolSelector { 
  override def getList(ontology: OWLOntology): Seq[(OWLEntity, Int)] = { 
    val occs = ModuleExtractor.countSymbolsInGenuineModules(ontology).toSeq.sortBy(-_._2)
    occs.map(p => (p._1, p._2))
  } 
}

object SymbolSelectorByOccurences extends SymbolSelector { 
  override def getList(ontology: OWLOntology): Seq[(OWLEntity, Int)] = { 
    val axioms = Set[OWLAxiom]()++
		 ontology.getTBoxAxioms(Imports.EXCLUDED).iterator.asScala.toSet ++
		 ontology.getABoxAxioms(Imports.EXCLUDED).iterator.asScala.toSet ++
		 ontology.getRBoxAxioms(Imports.EXCLUDED).iterator.asScala.toSet

    var count = MultiSet[OWLEntity]()

    def updateP(prop: OWLPropertyExpression)
    : Unit = prop match { 
      case pr: OWLDataProperty => count.add(pr)
      case pr: OWLObjectProperty => count.add(pr)
      case pr: OWLObjectInverseOf => updateP(pr.getInverse)
      case _ => ; // ignore remaining property expressions
    }

    def updateC(classExp: OWLClassExpression): Unit = classExp match { 
      case cl: OWLClass => count.add(cl)
//      case cl: OWLQuantifiedRestriction[_,_,_] => updateP(cl.getProperty); update(cl.getFiller)
      case cl: OWLRestriction => updateP(cl.getProperty)
//      case cl: (cl.getNestedClassExpressions - classExp).foreach(update)
      case _ => ;
    }
      

    def update(axiom: OWLAxiom): Unit = axiom match { 
      case pr: OWLPropertyAxiom => { 
	pr.getDataPropertiesInSignature.forEach(p => count.add(p))
	pr.getObjectPropertiesInSignature.forEach(p => count.add(p))
	pr.getNestedClassExpressions.forEach(updateC)
      } // <--- careful: might not always be valid. consider: r <= r.s
      case _ =>  axiom.getNestedClassExpressions().forEach{ updateC }
    }

    axioms.foreach(update)

    count.toSeq.sortBy(-_._2)
  }
}

object AverageValues { 

  def main(args: Array[String]) = { 

    val sums = MultiSet[Int]()
    
    var onts = 0

    var lastX = 0
    var lastY = 0
    Source.fromFile(new File(args(0))).getLines.foreach { pair =>
      val xy = pair.split(" ")
      val x = xy(0).toInt
      val y = xy(1).toInt

      if(x==0)
	onts +=1

      if(lastX<=x) { 
	for(i <- lastX until x)
	  sums.add(i, lastY)
      }
      if(lastX>x){ 
	sums.add(lastX, lastY)
      }

      lastX = x
      lastY = y
    }
    sums.add(lastX, lastY)

    
    sums.toSeq.sortBy(_._1).foreach(p => println(p._1+" "+(p._2.toDouble/onts)))
    
  }

}
