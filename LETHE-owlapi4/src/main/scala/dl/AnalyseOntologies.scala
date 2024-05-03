package uk.ac.man.cs.lethe.internal.dl.owlapi

import uk.ac.man.cs.lethe.internal.dl.filters.OWLOntologyFilters

import java.io.File




object AnalyseOntologies {
  def main(args: Array[String]) = {
    val files = new File(args(0)).listFiles

    files.foreach{ file =>
      print(file.getName+"\n")
      try{
        val owlOntology = OWLApiInterface.getOWLOntology(file)
        OWLOntologyFilters.restrictToALCH(owlOntology)
        val ontology = OWLApiConverter.convert(owlOntology)
        // val ontology = OWLApiConverter.convert(owlOntology)
        // println("\t"+OWLFamilies.family(ontology))
        println("\tNumber of atomic concepts: "+ontology.atomicConcepts.size)
        println("\tTBox size: "+ontology.tbox.size)
        println("\tABox size: "+ontology.abox.size)
        println("\tRBox size: "+ontology.rbox.size)
        println()
      } catch {
        case _: Throwable => println("Exception")
      }
    }
  }
}
