package uk.ac.man.cs.lethe.internal.dl.abduction

import uk.ac.man.cs.lethe.internal.dl.datatypes.extended.GreatestFixpoint
import uk.ac.man.cs.lethe.internal.dl.datatypes.{BaseConcept, ConceptComplement, DLStatement, Subsumption}
import uk.ac.man.cs.lethe.internal.dl.forgetting.QuickConceptForgetter
import uk.ac.man.cs.lethe.internal.dl.forgetting.direct.{ALCFormulaPreparations, SimpleDefinerEliminator}

/**
 * Eliminate greatest fixpoint expressions by using auxiliary concepts
 */
object FixpointEliminator {
  def eliminateFixpoints(statement: DLStatement): Iterable[DLStatement] = {
    statement.subConcepts.find(_._1.isInstanceOf[GreatestFixpoint]) match {
      case None => Set(statement) // end of recursion - we are done, everything replaced
      case Some((GreatestFixpoint(variable, concept),_)) =>
        val helperConcept = nextConcept()
        val rhs = QuickConceptForgetter.replaceCBy(concept, Set(variable), helperConcept)
        val replaced = QuickConceptForgetter.replaceCBy(statement,
          Set(GreatestFixpoint(variable, concept)), helperConcept)

        Set(replaced,
          Subsumption(helperConcept, rhs)).flatMap(eliminateFixpoints) // recursively continue until all are gone
    }
  }

  var counter = 0

  def nextConcept() = {
    counter+=1
    BaseConcept("___X"+counter)
  }
}
