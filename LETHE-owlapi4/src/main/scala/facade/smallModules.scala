package uk.ac.man.cs.lethe.moduleExtraction

import java.io.File
import java.util.Date
import scala.collection.JavaConverters._
import scala.collection.convert.ImplicitConversions.`collection AsScalaIterable`
import org.semanticweb.owlapi.model.{OWLClass, OWLEntity, OWLLogicalAxiom, OWLObjectProperty, OWLOntology}

import uk.ac.man.cs.lethe.forgetting.{AlchTBoxForgetter, IOWLForgetter}
import uk.ac.man.cs.lethe.internal.dl.datatypes._
import uk.ac.man.cs.lethe.internal.dl.forgetting.QuickConceptForgetter
import uk.ac.man.cs.lethe.internal.dl.forgetting.direct.SymbolOrderings
import uk.ac.man.cs.lethe.internal.dl.owlapi.{ModuleExtractor, OWLApiConverter, OWLApiInterface, OWLExporter}
import uk.ac.man.cs.lethe.internal.tools.ConsoleProgressBar



object SmallModuleExtraction { 
  def main(args: Array[String]) = { 
    println("Loading ontology...")
    println(new Date())
    val ontology = OWLApiInterface.getOWLOntology(new File(args(0)))
    println(new Date())
    println("Loaded")

    val forgetter = new AlchTBoxForgetter()
    forgetter.internalForgetter.deactivateProgressBar
    val moduleExtractor = new SmallModuleExtractor(new AlchTBoxForgetter())

    //val moduleExtractor = new SmallModuleExtractorAlch()//(new AlchTBoxForgetter())

    val signature = ontology.getSignature().iterator.asScala.slice(0, 100).toSet

    var result = moduleExtractor.extractSmallest(ontology, setAsJavaSet(signature))


    println(result.getSignature().size +" symbols remain in the signature")

//    println(SimpleOWLFormatter.format(result))
  }
}

class SmallModuleExtractorAlch extends SmallModuleExtractor(new AlchTBoxForgetter()) { 


  // override def extract(owlOntology: OWLOntology, signature: java.util.Set[OWLEntity]) = { 


  //   val forgetter = DirectALCForgetter // supports onlyIfSmaller

  //   println("Nr. axioms input ontology: "+owlOntology.getAxioms.size)
  //   val module = ModuleExtractor.getModule(owlOntology, signature)
  //   println("Nr. axioms extracted module: "+module.size)

  //   val modulesSignature = module.flatMap(_.getSignature).filter(s => s.isInstanceOf[OWLClass]||s.isInstanceOf[OWLObjectProperty])

  //   val owlEntitiesToForget = modulesSignature--signature

  //   var ontology = new Ontology()
  //   OWLApiConverter.convert(module).foreach{ ontology.addStatement }
  //   println("Nr. axioms converted module: "+ontology.statements.size)
  //   println("Size converted module: "+ontology.size)

  //   val oldAxiomsNr = ontology.statements.size
  //   val oldSize = ontology.size


  //   var symbolsToForget = owlEntitiesToForget.map(OWLApiConverter.getName)

  //   var orderedSymbols =
  //     SymbolOrderings.orderByNumOfOccurrences(symbolsToForget, ontology) // start with most frequent

  //   symbolsToForget --= 
  //     orderedSymbols.slice(0, (orderedSymbols.size.toDouble*.1).toInt) // remove most frequent 10%

  //   ontology = forgetter.forget(ontology, symbolsToForget, onlyIfSmaller = true)
       

  //   println("\n\n")
  //   println("Done.")
  //   println("Nr. of Axioms changed from "+oldAxiomsNr+" to "+ontology.statements.size)
  //   println("Size changed from "+oldSize+" to "+ontology.size)


  //   (new OWLExporter()).toOwlOntology(ontology)
  // }

} 

/**
 * Class for extracting smaller "modules" using forgetting.
 *
 * @param owlForgetter The forgetter to be used, determining the
 * supported expressivity.
 */
class SmallModuleExtractor(owlForgetter: IOWLForgetter) { 

  val forgetter = owlForgetter.internalForgetter
  
  /**
   * Extract a top-bottom-star module. Then heuristically forget
   * from these module symbols that are not in the signature for
   * which this leads to a smaller ontology.
   */
  def extract(owlOntology: OWLOntology, signature: java.util.Set[OWLEntity]): OWLOntology = { 
    println("Nr. axioms input ontology: "+owlOntology.getAxioms().size)
    val module = ModuleExtractor.getModule(owlOntology, signature.asScala).filter(_.isInstanceOf[OWLLogicalAxiom])
    println("Nr. axioms extracted module: "+module.size)


    val modulesSignature = module.flatMap(_.getSignature().iterator().asScala.toSet).filter(s => s.isInstanceOf[OWLClass]||s.isInstanceOf[OWLObjectProperty])

    val owlEntitiesToForget = modulesSignature--signature.asScala

    var ontology = new Ontology()
    OWLApiConverter.convert(module).foreach{ ontology.addStatement }
    println("Nr. axioms converted module: "+ontology.statements.size)
    println("Size converted module: "+ontology.size)



    var symbolsToForget = owlEntitiesToForget.map(OWLApiConverter.getName)

    ontology = extract(ontology,symbolsToForget)

    (new OWLExporter()).toOwlOntology(ontology)
  }

  def extract(_ontology: Ontology, symbolsToForget: Set[String], keepHardest: Double=.1): Ontology = { 

    val oldAxiomsNr = _ontology.statements.size
    val oldSize = _ontology.size


    var ontology = QuickConceptForgetter.forget(_ontology, symbolsToForget)

    println("Nr. axioms after purification: "+ontology.statements.size)
    println("Size after purification: "+ontology.size)
    
    var orderedSymbols =
      SymbolOrderings.orderByNumOfOccurrences(symbolsToForget, ontology).reverse

    orderedSymbols = orderedSymbols.slice(0, (orderedSymbols.size.toDouble*(1-keepHardest)).toInt)

    val progressBar = new ConsoleProgressBar()
    progressBar.init(orderedSymbols.size, "Forgetting ")

    orderedSymbols.foreach{ symbol =>
      progressBar.increment(symbol)
      if(ontology.signature.contains(symbol)){ 
//	println()
//	println("Forgetting "+symbol)
	val forgettingResult = forgetter.forget(ontology, Set(symbol))
	if(forgettingResult.size<ontology.size){ 
	  ontology = forgettingResult
//	  println("\n\nAxioms: "+ontology.statements.size)
//	  println("Size: "+ontology.size)
	} else { 
//	  println("\n\nSkipped "+symbol)
	}
      }
    }

    progressBar.finish()
       

    println("\n\n")
    println("Done.")
    println("Nr. of Axioms changed from "+oldAxiomsNr+" to "+ontology.statements.size)
    println("Size changed from "+oldSize+" to "+ontology.size)

    return ontology
  }

  def extractSmallest(owlOntology: OWLOntology, signature: java.util.Set[OWLEntity], keepHardest: Double=.1) = { 
    println("Nr. axioms input ontology: "+owlOntology.getAxioms().size)
    val module = ModuleExtractor.getModule(owlOntology, signature.asScala).filter(_.isInstanceOf[OWLLogicalAxiom])
    println("Nr. axioms extracted module: "+module.size)

    val modulesSignature = module.flatMap(_.getSignature().iterator().asScala.toSet).filter(s => s.isInstanceOf[OWLClass]||s.isInstanceOf[OWLObjectProperty])

    val owlEntitiesToForget = modulesSignature--signature.asScala

    var ontology = new Ontology()
    OWLApiConverter.convert(module).foreach{ ontology.addStatement }
    println("Nr. axioms converted module: "+ontology.statements.size)
    println("Size converted module: "+ontology.size)

    val oldAxiomsNr = ontology.statements.size


    var symbolsToForget = owlEntitiesToForget.map(OWLApiConverter.getName)

    var oldSize = 0
    while(oldSize!=ontology.size){ 
      oldSize=ontology.size
      ontology = extract(ontology,symbolsToForget, keepHardest)
    }

    (new OWLExporter()).toOwlOntology(ontology)
  }
}
