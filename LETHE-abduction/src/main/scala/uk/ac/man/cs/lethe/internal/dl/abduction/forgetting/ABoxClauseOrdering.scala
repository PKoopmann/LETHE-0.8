package uk.ac.man.cs.lethe.internal.dl.abduction.forgetting

import uk.ac.man.cs.lethe.internal.dl.datatypes.Individual
import uk.ac.man.cs.lethe.internal.dl.datatypes.extended.ExtendedABoxClause
import uk.ac.man.cs.lethe.internal.dl.forgetting.direct.ConceptClause


class ABoxClauseOrdering(clauseOrdering: Ordering[ConceptClause])
  extends Ordering[ExtendedABoxClause] {

  val compareTuples = {
    (pair1: (Individual, ConceptClause), pair2: (Individual, ConceptClause)) =>
      // pair1._1.name.compare(pair2._1.name) match {
      //   case 0 => clauseOrdering.compare(pair1._2, pair2._2)
      //   case other => other
      // }
      clauseOrdering.compare(pair1._2, pair2._2) match {
        case 0 => pair1._1.name.compare(pair2._1.name)
        case other => other
      }
  }

  def compare(clause1: ExtendedABoxClause, clause2: ExtendedABoxClause) = {
    SeqSorting.compare(clause1.literals.toList.sortWith(compareTuples(_,_)<0),
      clause2.literals.toList.sortWith(compareTuples(_,_)<0),
      compareTuples)
  }


  object SeqSorting {
    def compare[A](seq1: Seq[A], seq2: Seq[A], lt: (A,A) => Int): Int = (seq1, seq2) match {
      case (Seq(), Seq()) => 0
      case (Seq(), _) => -1
      case (_, Seq()) => +1
      case (a::as, b::bs) if a==b => compare(as, bs, lt)
      case (a::_, b::_) => lt(a,b)
    }
  }
}