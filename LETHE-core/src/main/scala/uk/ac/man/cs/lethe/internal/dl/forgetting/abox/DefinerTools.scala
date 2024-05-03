package uk.ac.man.cs.lethe.internal.dl.forgetting.abox

import uk.ac.man.cs.lethe.internal.dl.datatypes.{Axiom, BaseConcept, DLStatement, Expression}
import uk.ac.man.cs.lethe.internal.dl.forgetting.direct.{ALCFormulaPreparations, ConceptClause}

object DefinerTools {
  /**
   * Extends "subset" with clauses from "clauses" so that, if a definer D occurs in "subset", then all clauses
   * from "clauses" with D are also in clauses
   */
  def definerClosureClauses(clauses: Set[ConceptClause], subset: Set[ConceptClause]) = {
    var unprocessedDefiners = definers(subset)

    var extension = subset

    var processedDefiners = Set[String]()
    while(!unprocessedDefiners.isEmpty) {
      processedDefiners ++= unprocessedDefiners

      val nextClauses = clauses.filter(_.atomicConcepts.exists(unprocessedDefiners))
      extension ++= nextClauses
      unprocessedDefiners = definers(nextClauses) -- processedDefiners
    }

    extension
  }

  def definerClosureAxioms(clauses: Set[Axiom], subset: Set[Axiom]) = {
    var unprocessedDefiners = definers(subset)

    var extension = subset

    var processedDefiners = Set[String]()
    while(!unprocessedDefiners.isEmpty) {
      processedDefiners ++= unprocessedDefiners

      val nextClauses = clauses.filter(_.atomicConcepts.exists(unprocessedDefiners))
      extension ++= nextClauses
      unprocessedDefiners = definers(nextClauses) -- processedDefiners
    }

    extension
  }

  def definers(clauses: Set[_<:Expression]) = {
    clauses.flatMap(_.atomicConcepts)
      .filter(c => ALCFormulaPreparations.isDefiner(BaseConcept(c)))
  }
}
