package uk.ac.man.cs.lethe.internal.application

import java.io.File
import java.util.Date
import org.semanticweb.owlapi.model._


import uk.ac.man.cs.lethe.interpolation._
//import uk.ac.man.cs.lethe.logicalDifference._
import uk.ac.man.cs.lethe.internal.dl.owlapi.OWLApiInterface



object LogicalDifferenceApplication { 
  
  val usageLine = """
Compute the logical difference of two ontologies.

Parameters:
--owlFile1 FILE       - specify owl-file containing the first ontology
--owlFile2 FILE       - specify owl-file containing the second ontology
--url1 URL            - specify URL of the first ontology
--url2 URL            - specify URL of the first ontology
"""+
//--dlFile FILE        - specify file in simple dl-syntax containing the ontology
"""--signature FILE     - specify file containing signature for the logical difference
		      (if absent, common signature of both ontologies is used)
"""+
//--sigSize SIZE       - randomly choose [sigSize] concept symbols
//--coherent           - when sigSize is given: use coherent signature
//--simplifyNames      - ignore url-part to simplify debugging
//--alch               - restrict input to alch axioms (specify before ontology url)
//--noABox             - remove abox before forgetting
//--filter             - filter too hard symbols from forgetting
//--ignore             - don't forget the following symbol name (only implemented for ConceptAndRoleForgetter)
//--onlyRoles          - only forget role names (except if explicitly stated)
//--onlyConcepts       - only forget concept names  (except if explicitly stated)
"""--approximationLevel LEVEL  - Determines the number of iterations used to approximate the fixpoint 
		       expressions.
--method METHOD      - use one of: 
  1 - ALCHTBoxForgetter
  2 - SHQTBoxForgetter 
  3 - ALCOntologyForgetter
"""


  var ontology1: OWLOntology = _
  var ontology2: OWLOntology = _
  var approximationLevel: Int = -1
  var interpolator: IOWLInterpolator = _

  var ontName1: String = _
  var ontName2: String = _

  def main(args: Array[String]) = { 
//
//    if(args.isEmpty){
//      println(usageLine)
//      sys.exit(0)
//    }
//
//    parseArgs(args.toList)
//
//    val logDifComputer = new LogicalDifferenceComputer(interpolator)
//    val exporter = new OWLExporter()
//
//
//    val difference1 = logDifComputer.logicalDifference(ontology1, ontology2, approximationLevel)
//    if(difference1.isEmpty){
//      println()
//      println(" no new entailments in the signature found for "
//        +ontName2+" that are not in  "+ontName1+".")
//    } else {
//      val diffOnt = exporter.toOwlOntology(difference1)
//      exporter.save(diffOnt, new File("newEntailments.owl"))
//      println()
//      println(ontName2+" has "+difference1.size+" new entailments in the signature. They are saved in \"newEntailments.owl\".")
//    }

//    val difference2 = logDifComputer.logicalDifference(ontology2, ontology1, approximationLevel)

    println("\n")
    println("\n")

    // if(difference1.isEmpty){ 
    //   println(ontName2+" is a conservative extension of "+ontName1)
    // } else { 
    //   val diffOnt = exporter.toOwlOntology(difference1)
    //   exporter.save(diffOnt, new File("newEntailments.owl"))
    //   println(ontName2+" has "+difference1.size+" new entailments in the signature. They are saved in \"newEntailments.owl\"")
    // }



    // if(difference2.isEmpty){ 
    //   println(ontName1+" is a conservative extension of "+ontName2)
    // } else { 
    //   val diffOnt = exporter.toOwlOntology(difference2)
    //   exporter.save(diffOnt, new File("lostEntailments.owl"))
    //   println(ontName2+" is missing "+difference2.size+" entailments of "+ontName1+" in the signature. They are saved in \"lostEntailments.owl\"")
    // }

  }

  def parseArgs(list: List[String]): Unit = {
    list match {
      case Nil => ()
      case "--owlFile1"::file::tail =>
	ontName1 = new File(file).getName
	println("Loading "+file)
	println(new Date())
	ontology1 = OWLApiInterface.getOWLOntology(new File(file))
	println(new Date())
	println("Ontology 1: "+ontName1)
	
	parseArgs(tail)
      case "--owlFile2"::file::tail =>
	ontName2 = new File(file).getName
	println("Loading "+file)
	println(new Date())
	ontology2 = OWLApiInterface.getOWLOntology(new File(file))
	println(new Date())
	println("Ontology 2: "+ontName2)
	parseArgs(tail)
      case "--url1"::url::tail =>
	ontName1 = url
	ontology1 = OWLApiInterface.getOWLOntology(url)
	println("Ontology 1: "+ontName1+ "("+ontology1+")")
	parseArgs(tail)
      case "--url2"::url::tail =>
	ontName2 = url
	ontology2 = OWLApiInterface.getOWLOntology(url)
	println("Ontology 2: "+ontName2+ "("+ontology2+")")
	parseArgs(tail)
      case "--approximationLevel" :: levelString :: tail =>
	println("Approximation level set to: "+levelString)
	approximationLevel = levelString.toInt
	parseArgs(tail)
      case "--method" :: method :: tail =>
	method match { 
	  case "1" => interpolator = new AlchTBoxInterpolator()
	  case "2" => interpolator = new ShqTBoxInterpolator()
	  case "3" => interpolator = new ShKnowledgeBaseInterpolator()
	}
	println("Using "+interpolator+" for interpolation.")
	parseArgs(tail)
      case option :: tail => 
	// println("Invalid option: "+option) 
	// println(usageLine)
        // sys.exit(1)
	println("unknown parameter "+option)
        parseArgs(tail)
    }

    if(ontology1==null){ 
      println("Please specify first ontology")
      System.exit(1)
    }
    if(ontology2==null){ 
      println("Please specify second ontology")
      System.exit(1)
    }
    if(interpolator==null){ 
      println("Please specify interpolation method")
      System.exit(1)
    }
    if(approximationLevel<0){ 
      println("Please specify a non-negative approximation level")
      System.exit(1)
    }

  }

}
