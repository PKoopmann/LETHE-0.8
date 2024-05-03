package uk.ac.man.cs.lethe.internal.application

import java.io.File
import java.util.Date
import org.semanticweb.owlapi.model._

import scala.io.Source
import scala.util.Random
//import uk.ac.man.cs.lethe.internal.application.benchmarks.InterpolationExperimenter
import uk.ac.man.cs.lethe.internal.dl.datatypes.{DLHelpers, Ontology}
import uk.ac.man.cs.lethe.internal.dl.filters.AxiomAnalysis
import uk.ac.man.cs.lethe.internal.dl.filters.OWLOntologyFilters
import uk.ac.man.cs.lethe.internal.dl.forgetting._
import uk.ac.man.cs.lethe.internal.dl.forgetting.abox.ABoxForgetter
import uk.ac.man.cs.lethe.internal.dl.forgetting.direct.FixpointApproximator
import uk.ac.man.cs.lethe.internal.dl.interpolation.OntologyInterpolator
import uk.ac.man.cs.lethe.internal.dl.owlapi._
import uk.ac.man.cs.lethe.internal.dl.parsing.DLParser
import uk.ac.man.cs.lethe.internal.forgetting.Forgetter
import uk.ac.man.cs.lethe.internal.tools.MockProgressBar


object ForgettingConsoleApplication {

  var printOntology = false
  var exportResult = true
  var progressBars = true

  val outputFile = "result.owl"

  val forgetters: List[Forgetter[Ontology, String]] =
    List(ConceptAndRoleForgetter,
      SHQForgetter,
      ABoxForgetter,
      DirectALCForgetter)

  // List(DirectALCForgetter,
  // 	 ALCForgetter,
  // 	 new AdaptingDlForgetter(SimpleScanForgetterSingleton,
  //  	  			 Ontology2ClausesConverter,
  //  	 			 Clauses2OntologyConverter),
  // 	 RoleForgetterS,
  // 	 ConceptAndRoleForgetter,
  // 	 SHQForgetter,
  // 	 ABoxForgetter)


  val usageLine = """Parameters:
--owlFile FILE       - specify owl-file containing the ontology
--url URL            - specify URL of the ontology
--dlFile FILE        - specify file in simple dl-syntax containing the ontology
--signature FILE     - specify file containing list of symbols to forget
--forget NAME        - specify additional symbol to forget
"""+
    //--sigSize SIZE       - randomly choose [sigSize] concept symbols
    //--coherent           - when sigSize is given: use coherent signature
    """--interpolate        - compute uniform interpolant over specified symbols instead of forgetting them
"""+
    //		       (works currently only with owl-files)
    //--simplifyNames      - ignore url-part to simplify debugging
    //--alch               - restrict input to alch axioms (specify before ontology url)
    //--noABox             - remove abox before forgetting
    //--filter             - filter too hard symbols from forgetting
    //--ignore             - don't forget the following symbol name (only implemented for ConceptAndRoleForgetter)
    //--onlyRoles          - only forget role names (except if explicitly stated)
    //--onlyConcepts       - only forget concept names  (except if explicitly stated)
    """--approximate LEVEL  - instead of using helper concepts, approximate uniform interpolate.
		       LEVEL determines the number of iterations used to approximate the fixpoint 
		       expressions.
--timeOut SECONDS    - timeout to use in seconds (returns intermediate result if reached) Only supported by method 1 and 3 at the moment.
--method METHOD      - use one of: 
  1 - ALCHTBoxForgetter
  2 - SHQTBoxForgetter 
  3 - ALCOntologyForgetter
"""


  def toOWLSignature(words: Set[String], owlOntology: OWLOntology): Iterable[OWLEntity] = {
    OWLApiInterface.getSignature(owlOntology).filter{ s =>  words(s.toString) }
  }



  var ontology: Ontology = _
  var forgettables: Set[String] = Set()
  var owlFile: File = _
  var coherent: Boolean = false

  var onlyConcepts: Boolean = false
  var onlyRoles: Boolean = false

  var approximate: Boolean = false
  var approximationLevel: Int = -1

  var randomSymbols: Boolean = false
  var includeMostFrequent: Int = 0

  def main(args: Array[String]): Unit =  {
    OWLApiConfiguration.SIMPLIFIED_NAMES=false
    if(args.isEmpty){
      println(usageLine)
      sys.exit(0)
    }

    parseArgs(args.toList)

    try {
      run()
    } catch {
      case t: Throwable => println("Exception: "+t); throw(t)
    }
  }

  def run() {

    if(!progressBars) {
      ConceptAndRoleForgetter.progressBar = MockProgressBar
      ABoxForgetter.progressBar = MockProgressBar
      SHQForgetter.progressBar = MockProgressBar
    }

    val forgetter = method match {
      case -1 => ConceptAndRoleForgetter
      case num => forgetters(num-1)
    }

    println("Using forgetter "+forgetter.toString)

    ConceptAndRoleForgetter.ignore=ignoreList

    var startTime: Long = 0

    printInformation(ontology, "Input ")


    var result =
      if(!interpolate){

        if(number!=0) {
          val mix = if(randomSymbols)
          { x: Iterable[String] => Random.shuffle(x) }
          else
          { x: Iterable[String] => x}


          if(onlyConcepts)
            forgettables ++= mix(ontology.atomicConcepts).take(number)
          else if(onlyRoles)
            forgettables ++= mix(ontology.roleSymbols).take(number)
          else
            forgettables ++= mix(ontology.signature).take(number)
        }
        forgettables --= ignoreList
        println("\n The following symbols will be forgotten:")
        forgettables.foreach(println)
        println("\n")
        println("Forgetting...")


        println("Started at "+(new Date()).toString)
        startTime = new Date().getTime
        forgetter.forget(ontology, forgettables)

      } // if interpolate
      else {


        var signature =
          if(coherent)
            ModuleExtractor.coherentSignature(owlOntology, number).toSeq
          else
            OWLApiInterface.getSignature(owlOntology).toSeq

        // signature = signature.filter(s =>
        //   s.isInstanceOf[OWLClass]||s.isInstanceOf[OWLObjectProperty])
        // <--- became redundant

        var redSig = Set[OWLEntity]()

        println("Select signature")
        redSig ++= toOWLSignature(forgettables, owlOntology)

        if(randomSymbols)
          redSig ++= Random.shuffle(signature).take(number).toSet
        else
          redSig ++= signature.take(number).toSet

        if(onlyConcepts)
          redSig = redSig.filter(_.isInstanceOf[OWLObjectProperty])

        println("Desired signature: "+redSig.map(OWLApiConverter.getName))

        val interpolator = new OntologyInterpolator(forgetter)
        interpolator.includeMostFrequent = includeMostFrequent
        interpolator.dontForget ++= ignoreList

        startTime = new Date().getTime
        interpolator.uniformInterpolantInternalFormat(owlOntology, redSig)
      }

    println("\n\nFinished at "+(new Date()).toString)
    println("Duration: "+(new Date().getTime-startTime))


    if(approximate)
      result = FixpointApproximator.approximate(result, approximationLevel)

    //    println(result)
    //    println(SimpleDLFormatter.format(result))
    printInformation(result, "Result ")

    if(exportResult){
      println("Exporting to "+outputFile+"...")
      new OWLExporter().exportOntology(result, new File(outputFile))
    }
  }

  def printInformation(ontology: Ontology, prefix: String) = {
    println()

    if(printOntology)
      println(ontology)

    println(prefix+"Number of Axioms: "+ontology.statements.size+"")
    println(prefix+"Average Axiom size: "+ontology.size.toDouble/ontology.statements.size)
//    println(prefix+"Definers: "+InterpolationExperimenter.countDefiners(ontology))
//    println(prefix+"Number Restrictions: "+InterpolationExperimenter.countNumberRestrictions(ontology))
    //println(prefix+"Nominals: "+InterpolationExperimenter.countNominals(ontology)) // slow implementation?

    println()
  }

  var number = 0
  var method = -1 // ConceptAndRoleForgetter
  var interpolate = false
  var owlOntology: OWLOntology = _
  var alch = false
  var ignoreList = Set[String]()
  var noABox = false

  def parseArgs(list: List[String]): Unit = {
    list match {
      case Nil => ()
      case "--dlFile" :: filename :: tail =>
        ontology = DLParser.parse(new File(filename))
        parseArgs(tail)
      case "--owlFile" :: filename :: tail =>
        owlFile = new File(filename)
        print("Parsing "+filename+"...")

        try {
          owlOntology = OWLApiInterface.getOWLOntology(owlFile, true)
        } catch {
          case _: org.semanticweb.owlapi.io.UnparsableOntologyException =>
            println("Parsing error"); System.exit(1)
          case t: Throwable => println(t.toString); System.exit(1)
        }

        println()
        AxiomAnalysis.analyse(owlOntology)

        if(alch)
          OWLOntologyFilters.restrictToALCH(owlOntology)
        print("converting...")
        ontology = OWLApiConverter.convert(owlOntology)
        println()
        parseArgs(tail)
      case "--noABox":: tail =>
        noABox = true
        println("No ABox")
        parseArgs(tail)
      case "--url" :: url :: tail =>
        print("Parsing "+url+"...")
        owlOntology = OWLApiInterface.getOWLOntology(url)
        if(alch)
          OWLOntologyFilters.restrictToALCH(owlOntology)
        print("converting...")
        ontology = OWLApiConverter.convert(owlOntology)
        println()
        parseArgs(tail)
      case "--signature" :: filename :: tail =>
        println("Using signature file "+filename)
        forgettables = Source.fromFile(filename).getLines.toSet[String]
        parseArgs(tail)
      case "--sigSize" :: n :: tail =>
        number = n.toInt
        println("Signature size: "+number.toString+" symbols.")

        if(owlOntology.getSignature().size <= number){
          println("Signature too small.")
          System.exit(0)
        }

        parseArgs(tail)
      case "--coherent" :: tail =>
        coherent = true
        println("Using coherent signature")
        parseArgs(tail)
      case "--forget" :: forgettable :: tail =>
        forgettables += forgettable
        println("and forget "+forgettable)
        parseArgs(tail)
      case "--method" :: n :: tail =>
        method = n.toInt
        println("Using method "+n+" ("+forgetters(method-1).getClass.getCanonicalName+")")

        if(method>forgetters.size){
          println("invalid method chosen!")
          println(usageLine)
          sys.exit(1)
        }

        parseArgs(tail)
      case "--interpolate" :: tail =>
        interpolate = true
        parseArgs(tail)
      case "--simplifyNames" :: tail =>
        println("Using simple symbol names")
        OWLApiConfiguration.SIMPLIFIED_NAMES=true
        parseArgs(tail)
      case "--alch" :: tail =>
        println("Restricting to ALCH axioms")
        alch=true
        parseArgs(tail)
      case "--filter" :: tail =>
        println("Ignoring too hard symbols")
        ConceptAndRoleForgetter.filter=true
        parseArgs(tail)
      case "--ignore" :: ignore :: tail =>
        println("Ignoring "+ignore)
        ignoreList = ignoreList+ignore
        parseArgs(tail)
      case "--onlyConcepts" :: tail =>
        println("Only concept symbols")
        onlyConcepts = true
        parseArgs(tail)
      case "--summarize1":: numOfConcepts :: tail =>
        interpolate = true
        println("Selecting "+numOfConcepts+" main symbols by modules")
        if(owlOntology.getSignature().size <= numOfConcepts.toInt){
          println("Signature too small.")
          System.exit(0)
        }
        forgettables = SymbolSelectorByModules.selectNSymbols(owlOntology, numOfConcepts.toInt).map(_._1.toString).toSet
        println(forgettables)
      case "--summarize2":: numOfConcepts :: tail =>
        interpolate = true
        println("Selecting "+numOfConcepts+" main symbols by occurences")
        if(owlOntology.getSignature().size <= numOfConcepts.toInt){
          println("Signature too small.")
          System.exit(0)
        }
        forgettables = SymbolSelectorByOccurences.selectNSymbols(owlOntology, numOfConcepts.toInt).map(_._1.toString).toSet
        println(forgettables)
      case "--onlyRoles" :: tail =>
        println("Only role symbols")
        onlyRoles = true
        parseArgs(tail)
      case "--approximate" :: levelString :: tail =>
        println("Approximating uniform interpolant. Level: "+levelString)
        approximate = true
        approximationLevel = levelString.toInt
        parseArgs(tail)
      case "--noProgressBars" :: tail =>
        progressBars = false
        parseArgs(tail)
      case "--printOntologies" :: tail =>
        printOntology = true
        parseArgs(tail)
      case "--randomSymbols" :: tail =>
        randomSymbols = true
        parseArgs(tail)
      case "--includeMostFrequent"::number::tail =>
        includeMostFrequent = number.toInt
        parseArgs(tail)
      case "--timeOut"::number::tail =>
        println("Time out of "+number+" seconds used")

        DirectALCForgetter.useTimeout(number.toInt*1000)

        DirectALCForgetterRoles.useTimeout(number.toInt*1000)

        RoleForgetter.useTimeout(number.toInt*1000)

        ConceptAndRoleForgetter.useTimeout(number.toInt*1000)

        ABoxForgetter.useTimeout(number.toInt*1000)

        parseArgs(tail)
      case option :: tail =>
        // println("Invalid option: "+option)
        // println(usageLine)
        // sys.exit(1)
        println("unknown parameter "+option)
        parseArgs(tail)
    }

    if(ontology==null){
      println("Ontology has to be specified!")
      sys.exit(1)
    }

    if(noABox && owlOntology!=null)
      OWLOntologyFilters.removeABox(owlOntology)
    if(noABox && ontology!=null)
      ontology.abox.assertions = Set()

  }
}
