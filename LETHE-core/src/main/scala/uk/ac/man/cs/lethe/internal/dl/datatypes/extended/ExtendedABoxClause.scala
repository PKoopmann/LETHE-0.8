package uk.ac.man.cs.lethe.internal.dl.datatypes.extended

/**
 * Classes related to basic data types used in direct resolution methods
 */

import uk.ac.man.cs.lethe.internal.dl.datatypes.{Assertion, Expression, Individual, RoleAssertion}
import uk.ac.man.cs.lethe.internal.dl.forgetting.direct._

//import com.dongxiguo.fastring.Fastring.Implicits._



object ExtendedABoxClause {

  // return union of literals
  def combine(clauses: Iterable[ExtendedABoxClause]) = {
    // assume same ordering for all clauses

    val ord = clauses.collectFirst{
      case eac if eac.literals.size>0 => eac.literals.head._2.ordering }

    val conceptClauseMap = clauses.flatMap(_.literals.keySet).map(key =>
      (key, new ConceptClause(clauses.flatMap(_.literalsOf(key)),
        ord.get // this Optional.get is safe as we wouldn't get here if the optional would be empty
      ))).toMap[Individual, ConceptClause]


    val roleAssertions = clauses.flatMap(_.roleAssertions).toSet[RoleAssertion]
    val negatedRoleAssertions = clauses.flatMap(_.negatedRoleAssertions).toSet[RoleAssertion]

    new ExtendedABoxClause(conceptClauseMap, roleAssertions, negatedRoleAssertions)
  }

  def from(assertion: Assertion) = assertion match {
    case ra: RoleAssertion =>
      new ExtendedABoxClause(Map[Individual, ConceptClause](),
        Set[RoleAssertion](ra), Set[RoleAssertion]())
    case nra: NegatedRoleAssertion =>
      new ExtendedABoxClause(Map[Individual, ConceptClause](), Set[RoleAssertion](), Set[RoleAssertion]())
  }

  def empty =
    new ExtendedABoxClause()
}

class ExtendedABoxClause(var literals: Map[Individual, ConceptClause] = Map(),
                         var roleAssertions: Set[RoleAssertion] = Set(),
                         var negatedRoleAssertions: Set[RoleAssertion] = Set(),
                         temp: Boolean=false) extends Expression {

  literals = literals.filterNot(_._2.literals.isEmpty)



//  println(this)
//  println(negatedRoleAssertions.isEmpty)

//  assert(temp || !literals.isEmpty || !roleAssertions.isEmpty || !negatedRoleAssertions.isEmpty,
//    "empty ABox clause - ontology inconsistent?")

  def this(individual: Individual, conceptClause: ConceptClause, temp: Boolean) =
    this(Map(individual -> conceptClause), Set(), Set(), temp)

  def this(individual: Individual, conceptClause: ConceptClause) =
    this(Map(individual -> conceptClause), Set(), Set(), false)

  def without(ra: RoleAssertion): ExtendedABoxClause =
    new ExtendedABoxClause(Map[Individual, ConceptClause]()++literals,
      roleAssertions-ra,
      Set[RoleAssertion]()++negatedRoleAssertions, temp)

  def without(individual: Individual, literal: ConceptLiteral) =
    replace(individual, literals(individual).without(literal))

  /**
   * union of the literal sets of this clause and the other
   */
  def combineWith(other: ExtendedABoxClause) =
    ExtendedABoxClause.combine(List(this, other))

  /**
   * replace literals associated to ind with clause
   */
  def replace(ind: Individual, clause: ConceptClause) =
    new ExtendedABoxClause(literals-ind+(ind->clause),
      roleAssertions, negatedRoleAssertions)


  def literalsOf(ind: Individual): Set[ConceptLiteral] = {
    literals.get(ind) match {
      case Some(clause) => clause.literals
      case None => Set()
    }
  }

  def withOrdering(ordering: Ordering[ConceptLiteral]) =
    new ExtendedABoxClause(literals.mapValues(_.withOrdering(ordering)),
      roleAssertions,
      negatedRoleAssertions,
      temp)


  override def toString = {
    var cl = literals.flatMap(_ match {
      case (individual, clause) => clause.literals.map(_.toString+"("+individual.toString+")")
    }).toSet[String]
    cl ++= roleAssertions.map(_.toString)
    cl ++= negatedRoleAssertions.map("-"+_.toString)
    val result = cl.mkString(" v ")
    if(result=="")
      "EMPTY_CLAUSE"
    else
      result
  }

  override def atomicConcepts = literals.values.flatMap(_.atomicConcepts).toSet
  override def roleSymbols =
    literals.values.flatMap(_.roleSymbols).toSet ++
      roleAssertions.flatMap(_.roleSymbols) ++
      negatedRoleAssertions.flatMap(_.roleSymbols)
  override def signature =
    literals.values.flatMap(_.signature).toSet ++
      roleAssertions.flatMap(_.roleSymbols) ++
      negatedRoleAssertions.flatMap(_.roleSymbols)

  override def size =
    literals.values.map(_.size).sum*2 +
      roleAssertions.size*3 +
      negatedRoleAssertions.size*3

  override def subConcepts = literals.values.map(_.subConcepts).reduce(_++_)


}


// object ABoxLiteral { 
//   def instantiate(conceptLiteral: ConceptLiteral, individual: Individual) = 
//     ABoxLiteral(conceptLiteral.polarity, conceptLiteral.concept, individual)
// }

// case class ABoxLiteral(polarity: Boolean, concept: Concept, individual: Individual) extends Expression{ 
//   def negate = ABoxLiteral(!polarity, concept, individual)

//   override def signature = concept.signature
//   override def atomicConcepts = concept.atomicConcepts
//   override def roleSymbols = concept.roleSymbols
//   override def subConcepts = concept.subConcepts
//   override def size = 3

//   override def toString = 
//     (if(polarity) "+" else "-")+concept.toString+"("+individual.toString+")"


// }

