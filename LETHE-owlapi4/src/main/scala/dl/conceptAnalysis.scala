package uk.ac.man.cs.lethe.internal.dl.analysis

import java.io.File
import java.io.PrintStream
import uk.ac.man.cs.lethe.internal.dl.datatypes._
import uk.ac.man.cs.lethe.internal.dl.filters.OWLOntologyFilters
import uk.ac.man.cs.lethe.internal.dl.forgetting.DirectALCForgetter
import uk.ac.man.cs.lethe.internal.dl.forgetting.RoleForgetter
import uk.ac.man.cs.lethe.internal.dl.owlapi.OWLApiConverter
import uk.ac.man.cs.lethe.internal.dl.owlapi.OWLApiInterface
import uk.ac.man.cs.lethe.internal.tools.MockProgressBar

object AnalyseDirectory {
  def main(args: Array[String]) = {
    // var skipped = false
    // val skipUntil = "rexo_v1.01.obo"

    DirectALCForgetter.progressBar = MockProgressBar
    RoleForgetter.progressBar = MockProgressBar

    new File(args(0)).listFiles.sortBy(_.length).foreach{ file =>
      try{
        // if(file.getName==skipUntil)
        //   skipped = true
        // if(skipped){
        println(file)
        val owlOntology = OWLApiInterface.getOWLOntology(file)

        // restrict expressivity, if required

        args(1) match {
          case "ALC" => OWLOntologyFilters.restrictToALC(owlOntology)
          case "ALCH" => OWLOntologyFilters.restrictToALCH(owlOntology)
          case "ALCHI" => OWLOntologyFilters.restrictToALCHI(owlOntology)
          case "SHQ" => OWLOntologyFilters.restrictToSHQ(owlOntology)
          case "SHIQ" => OWLOntologyFilters.restrictToSHIQ(owlOntology)
        }

        val ontology = OWLApiConverter.convert(owlOntology)

        // only check TBox and RBox
        ontology.abox = new ABox(Set())

        val output = new PrintStream(new File(file.getName+".stats"))
        DeepSymbolAnalyser.forgetEach(ontology, output, forgetConcepts=false)
        output.close
        //	}
      } catch {
        case e: Throwable => println("Exception: "+e)
      }
    }
  }
}

