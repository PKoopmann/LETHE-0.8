
package uk.ac.man.cs.lethe.internal.dl.forgetting

//import com.dongxiguo.fastring.Fastring.Implicits._

import com.typesafe.scalalogging.Logger
import uk.ac.man.cs.lethe.internal.dl.datatypes.extended.{ConjunctiveAssertion, DisjunctiveAssertion, GreatestFixpoint, NegatedRoleAssertion}
import uk.ac.man.cs.lethe.internal.dl.datatypes._
import uk.ac.man.cs.lethe.internal.dl.forgetting.direct.ALCFormulaPreparations
import uk.ac.man.cs.lethe.internal.forgetting.{Forgetter, ForgetterWithBackgroundKB}

/**
 * quick, but incomplete forgetting method, used as preprocessing step:
 *
 * only forget symbols that occur either only positively or only negatively. Simply replace them
 * by TOP respectively BOTTOM and simplify the result.
 */
object QuickConceptForgetter extends Forgetter[Ontology, String]
  with ForgetterWithBackgroundKB[Ontology, String] {

  //  implicit val (logger, formatter, appender) = ZeroLoggerFactory.newLogger(this)
  //  import formatter._

  val logger = Logger(QuickConceptForgetter.getClass)

  override def steps = 1


  override def forget(ontology: Ontology, symbols: Set[String]): Ontology =
    forget(ontology, symbols, new Ontology())

  override def forget(ontology: Ontology, symbols: Set[String], background: Ontology): Ontology =
    if(symbols.size==1)
      return forget(ontology, symbols.head, background)
    else
      forget(ontology.statements, symbols, background.statements)


  def forget(statements: Iterable[DLStatement],
             symbols: Set[String],
             background: Iterable[DLStatement]): Ontology = {

    var allConceptSymbols = statements.flatMap(_.atomicConcepts).toSet
    var conceptSymbols = symbols.filter(allConceptSymbols)

    var onlyPositive = conceptSymbols
    var onlyNegative = conceptSymbols

    // check occurrences in nnf of both background ontology and knowledge base
    val nnfStatements = (statements++background).flatMap(nnf)

    def checkPositively(concept: Concept): Unit = concept match {
      case BaseConcept(name) => onlyNegative -= name
      case ConceptComplement(BaseConcept(name)) => onlyPositive -= name
      case ConceptComplement(TopConcept) => ;
      case ConceptComplement(BottomConcept) => ;
      case TopConcept => ;
      case BottomConcept => ;
      case ConceptConjunction(cs) => cs.foreach(checkPositively)
      case ConceptDisjunction(ds) => ds.foreach(checkPositively)
      case UniversalRoleRestriction(_, c) => checkPositively(c)
      case ExistentialRoleRestriction(_, c) => checkPositively(c)
      case MinNumberRestriction(_,_,c) => checkPositively(c)
      case MaxNumberRestriction(_,_,ConceptComplement(c)) => checkPositively(c)
      case _: NominalSet => ;
    }

    def checkNegatively(concept: Concept): Unit = concept match {
      case BaseConcept(name) => onlyPositive -= name
      case ConceptComplement(BaseConcept(name)) => onlyNegative -= name
      case TopConcept => ;
      case BottomConcept => ;
      case TopConcept => ;
      case BottomConcept => ;
      case ConceptConjunction(cs) => cs.foreach(checkNegatively)
      case ConceptDisjunction(ds) => ds.foreach(checkNegatively)
      case UniversalRoleRestriction(_, c) => checkNegatively(c)
      case ExistentialRoleRestriction(_, c) => checkNegatively(c)
      case MinNumberRestriction(_,_,c) => checkNegatively(c)
      case MaxNumberRestriction(_,_,ConceptComplement(c)) => checkNegatively(c)
      case _: NominalSet => ;
    }

    nnfStatements.foreach { _ match {
      case Subsumption(a, b) => checkNegatively(a); checkPositively(b)
      case ConceptAssertion(c, i) => checkPositively(c)
      case _ => ;
    } }


    // if only positively or only negatively, we can safely replace them in 
    // the knowledge base 
    var result = statements

    result = replaceBy(result, onlyPositive, TopConcept)
    result = replaceBy(result, onlyNegative, BottomConcept)
    logger.info("Purified "+(onlyPositive++onlyNegative)+" concept symbols")

    assert(!result.exists(_.atomicConcepts.exists(onlyPositive)))
    assert(!result.exists(_.atomicConcepts.exists(onlyNegative)))

    Ontology.buildFrom(result)
    //logger.trace(s"Before simplifiying: ${result}")
    //DLHelpers.simplify(Ontology.buildFrom(result))
  }

  def forget(ontology: Ontology, symbol: String, background: Ontology): Ontology = {
    DLHelpers.simplify(
      determinePolarity(ontology, symbol, background) match {
        case Polarity.POS => replaceBy(ontology, Set(symbol), TopConcept)
        case Polarity.NEG => replaceBy(ontology, Set(symbol), BottomConcept)
        case Polarity.MIXED => ontology
      }
    )
  }

  object Polarity extends Enumeration {
    type Polarity = Value
    val POS, NEG, MIXED = Value
  }

  def determinePolarity(ontology: Ontology,
                        symbol: String,
                        background: Ontology): Polarity.Polarity = {
    var pos, neg, mixed = false

    def checkPos(concept: Concept): Unit = concept match {
      case BaseConcept(name) if(neg) => mixed = true
      case BaseConcept(name) => pos = true
      case ConceptComplement(BaseConcept(name)) if(pos) => mixed = true
      case ConceptComplement(BaseConcept(name)) => neg = true
      case ConceptComplement(TopConcept) => ;
      case ConceptComplement(BottomConcept) => ;
      case TopConcept => ;
      case BottomConcept => ;
      case ConceptConjunction(cs) => cs.foreach(checkPos)
      case ConceptDisjunction(ds) => ds.foreach(checkPos)
      case UniversalRoleRestriction(_, c) => checkPos(c)
      case ExistentialRoleRestriction(_, c) => checkPos(c)
      case MinNumberRestriction(_,_,c) => checkPos(c)
      case MaxNumberRestriction(_,_,ConceptComplement(c)) => checkPos(c)
      case _: NominalSet => ;
    }
    def checkNeg(concept: Concept): Unit = concept match {
      case BaseConcept(name) if(pos) => mixed = true
      case BaseConcept(name) => neg = true
      case ConceptComplement(BaseConcept(name)) if(neg) => mixed = true
      case ConceptComplement(BaseConcept(name)) => pos = true
      case ConceptComplement(TopConcept) => ;
      case ConceptComplement(BottomConcept) => ;
      case TopConcept => ;
      case BottomConcept => ;
      case ConceptConjunction(cs) => cs.foreach(checkNeg)
      case ConceptDisjunction(ds) => ds.foreach(checkNeg)
      case UniversalRoleRestriction(_, c) => checkNeg(c)
      case ExistentialRoleRestriction(_, c) => checkNeg(c)
      case MinNumberRestriction(_,_,c) => checkNeg(c)
      case MaxNumberRestriction(_,_,ConceptComplement(c)) => checkNeg(c)
      case _: NominalSet => ;
    }

    ontology.tbox.axioms.view.flatMap(nnf).foreach{ _ match {
      case Subsumption(a, b) => {
        checkNeg(a)
        checkPos(b)
        if(mixed)
          return Polarity.MIXED
      }
    }}

    ontology.abox.assertions.foreach{ _ match {
      case ConceptAssertion(c, i) => {
        val nnfC = nnf(c)
        checkNeg(nnfC)
        checkPos(nnfC)
        if(mixed)
          return Polarity.MIXED
      }
      case ra: RoleAssertion => ;
    }}

    // check also background knowledge base
    background.tbox.axioms.view.flatMap(nnf).foreach{ _ match {
      case Subsumption(a, b) => {
        checkNeg(a)
        checkPos(b)
        if(mixed)
          return Polarity.MIXED
      }
    }}

    background.abox.assertions.foreach{ _ match {
      case ConceptAssertion(c, i) => {
        val nnfC = nnf(c)
        checkNeg(nnfC)
        checkPos(nnfC)
        if(mixed)
          return Polarity.MIXED
      }
      case ra: RoleAssertion => ;
    }}

    if(pos && !neg)
      Polarity.POS
    else if(!pos && neg)
      Polarity.NEG
    else
      Polarity.MIXED
  }

  // replace every occurrence of an concept named from 'conceptNames' by 'concept'
  def replaceBy(ontology: Ontology,
                conceptNames: Set[String],
                concept: Concept): Ontology = {

    ontology.tbox.axioms = replaceBy(ontology.tbox.axioms, conceptNames, concept).toSet
    ontology.abox.assertions = replaceBy(ontology.abox.assertions, conceptNames, concept).toSet

    ontology
  }

  def replaceBy[A <: DLStatement](dlStatements: Iterable[A],
                                  conceptNames: Set[String],
                                  concept: Concept): Iterable[A] =
    dlStatements.map(replaceBy(_, conceptNames, concept).asInstanceOf[A])


  def replaceBy(statement: DLStatement,
                conceptNames: Set[String],
                concept: Concept): DLStatement = statement match {
    case Subsumption(a, b) =>
      //assert(!conceptNames(a.toString), statement+", "+conceptNames)  
      // <--- why is this assertion meaningful?

      Subsumption(replaceBy(a, conceptNames, concept), replaceBy(b, conceptNames, concept))
    case ConceptEquivalence(a, b) =>
      ConceptEquivalence(replaceBy(a, conceptNames, concept), replaceBy(b, conceptNames, concept))
    case ConceptAssertion(c, i) =>
      ConceptAssertion(replaceBy(c, conceptNames, concept), i)
    case DisjunctiveConceptAssertion(cas) =>
      DisjunctiveConceptAssertion(cas.map(replaceBy(_, conceptNames, concept).asInstanceOf[ConceptAssertion]))
    case ra: RoleAssertion => ra
    case ra: RoleAxiom => ra
    case DisjunctiveAssertion(assertions) =>
      DisjunctiveAssertion(assertions.map(replaceBy(_, conceptNames, concept).asInstanceOf[Assertion]))
    case ConjunctiveAssertion(assertions) =>
      ConjunctiveAssertion(assertions.map(replaceBy(_, conceptNames, concept).asInstanceOf[Assertion]))
    case nra: NegatedRoleAssertion => nra
  }

  def replaceBy(c: Concept, conceptNames: Set[String], concept: Concept): Concept = c match {
    case TopConcept => TopConcept
    case BottomConcept => BottomConcept
    case BaseConcept(n) if conceptNames(n) => concept
    case b: BaseConcept => b
    case ConceptComplement(c) => ConceptComplement(replaceBy(c, conceptNames, concept))
    case ConceptDisjunction(ds) => ConceptDisjunction(ds.map(replaceBy(_, conceptNames, concept)))
    case ConceptConjunction(cs) => ConceptConjunction(cs.map(replaceBy(_, conceptNames, concept)))
    case UniversalRoleRestriction(r,c) => UniversalRoleRestriction(r, replaceBy(c, conceptNames, concept))
    case ExistentialRoleRestriction(r,c) => ExistentialRoleRestriction(r, replaceBy(c, conceptNames, concept))
    case MinNumberRestriction(n,r,c) => MinNumberRestriction(n,r,replaceBy(c,conceptNames,concept))
    case MaxNumberRestriction(n,r,c) => MaxNumberRestriction(n,r,replaceBy(c,conceptNames,concept))
    case GreatestFixpoint(variable, c) => GreatestFixpoint(variable, replaceBy(c, conceptNames,concept))
    case _: NominalSet => c
  }

  def replaceCBy[A<:Concept](statement: DLStatement,
                 toReplace: Set[A],
                 replaceWith: Concept): DLStatement = statement match {
    case Subsumption(a, b) =>
      //assert(!conceptNames(a.toString), statement+", "+conceptNames)
      // <--- why is this assertion meaningful?

      Subsumption(replaceCBy(a, toReplace, replaceWith), replaceCBy(b, toReplace, replaceWith))
    case ConceptEquivalence(a, b) =>
      ConceptEquivalence(replaceCBy(a, toReplace, replaceWith), replaceCBy(b, toReplace, replaceWith))
    case ConceptAssertion(c, i) =>
      ConceptAssertion(replaceCBy(c, toReplace, replaceWith), i)
    case DisjunctiveConceptAssertion(cas) =>
      DisjunctiveConceptAssertion(cas.map(replaceCBy(_, toReplace, replaceWith).asInstanceOf[ConceptAssertion]))
    case ra: RoleAssertion => ra
    case ra: RoleAxiom => ra
    case DisjunctiveAssertion(assertions) =>
      DisjunctiveAssertion(assertions.map(replaceCBy(_, toReplace, replaceWith).asInstanceOf[Assertion]))
    case ConjunctiveAssertion(assertions) =>
      ConjunctiveAssertion(assertions.map(replaceCBy(_, toReplace, replaceWith).asInstanceOf[Assertion]))
    case nra: NegatedRoleAssertion => nra
  }

  def replaceCBy[A<:Concept](c: Concept, concepts: Set[A], concept: Concept): Concept = c match {
    case c:A  if concepts.contains(c) => concept
    case TopConcept => TopConcept
    case BottomConcept => BottomConcept
    case b: BaseConcept => b
    case ConceptComplement(c) => ConceptComplement(replaceCBy(c, concepts, concept))
    case ConceptDisjunction(ds) => ConceptDisjunction(ds.map(replaceCBy(_, concepts, concept)))
    case ConceptConjunction(cs) => ConceptConjunction(cs.map(replaceCBy(_, concepts, concept)))
    case UniversalRoleRestriction(r,c) => UniversalRoleRestriction(r, replaceCBy(c, concepts, concept))
    case ExistentialRoleRestriction(r,c) => ExistentialRoleRestriction(r, replaceCBy(c, concepts, concept))
    case MinNumberRestriction(n,r,c) => MinNumberRestriction(n,r,replaceCBy(c,concepts,concept))
    case MaxNumberRestriction(n,r,c) => MaxNumberRestriction(n,r,replaceCBy(c,concepts,concept))
    case GreatestFixpoint(variable, c) => GreatestFixpoint(variable, replaceCBy(c, concepts,concept))
    case _: NominalSet => c
  }

  /**
   * Return a set of subsumptions, where both subsumer and subsumee are in nnf
   */
  def nnf(statement: DLStatement): Set[DLStatement] = statement match {
    case ConceptEquivalence(a, b) => nnf(Subsumption(a,b)) ++ nnf(Subsumption(b,a))
    case Subsumption(a, b) => Set(Subsumption(ALCFormulaPreparations.nnf(a),
      ALCFormulaPreparations.nnf(b)))
    case ConceptAssertion(c, i) => Set(ConceptAssertion(nnf(c), i))
    case DisjunctiveConceptAssertion(cs) => Set(DisjunctiveConceptAssertion(cs.flatMap(nnf).map(_.asInstanceOf[ConceptAssertion])))
    case ra: RoleAssertion => Set(ra)
    case ra: RoleAxiom => Set(ra)
  }

  def nnf(concept: Concept): Concept = ALCFormulaPreparations.nnf(concept)
}
