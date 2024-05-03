package uk.ac.man.cs.lethe.internal.dl.forgetting.direct


import scala.collection.mutable.HashMap
import uk.ac.man.cs.lethe.internal.dl.datatypes.RoleAssertion

import com.typesafe.scalalogging.Logger

import uk.ac.man.cs.lethe.internal.dl.datatypes._

// meaning: clauses containing derivation candidates should be fully resolved on
trait DerivationCandidate extends BaseConcept {
  override def toString = super.toString+"'dc"
}

trait IndividualDefiner extends BaseConcept {
  override def toString = super.toString+"'i"
}

object Test {
  def main(args: Array[String]) = {


    //(~G and ~C) or (C and ~B ) or (F and C) or (D and ~C)

    val concept = ConceptDisjunction(Set(
      ConceptConjunction(Set(
        ConceptComplement(BaseConcept("G")),
        ConceptComplement(BaseConcept("C")))),
      ConceptConjunction(Set(
        BaseConcept("C"),
        ConceptComplement(BaseConcept("B")))),
      ConceptConjunction(Set(
        BaseConcept("F"),
        BaseConcept("C"))),
      ConceptConjunction(Set(
        BaseConcept("D"),
        ConceptComplement(BaseConcept("C"))))
    ))

    println(concept)
    println(ALCFormulaPreparations.cnf(concept))
  }

}

object ALCFormulaPreparations extends ConceptGenerator {
  //  implicit val (logger, formatter, appender) = ZeroLoggerFactory.newLogger(this)

  val logger = Logger(ALCFormulaPreparations.getClass)

  var currentStatements: Iterable[DLStatement] = _

  def clauses(ontology: Ontology): Set[ConceptClause] =
    clauses(ontology.statements)

  def clauses(dlStatements: Iterable[DLStatement]): Set[ConceptClause] =
    clauses(dlStatements, SimpleLiteralOrdering)

  def clauses(dlStatements: Iterable[DLStatement], ordering: Ordering[ConceptLiteral]): Set[ConceptClause] = {

    //    currentStatements = dlStatements

    logger.info("analysing individuals relations...")


    logger.info("clausifying...")

    val result = prepare(dlStatements).conjuncts.map(_ match {
      case ConceptDisjunction(ds) => new ConceptClause(ds.map(toLiteral(_)), ordering)
      case p => new ConceptClause(Set(toLiteral(p)), ordering)
    })

    //    println("Definers: "+definers.size)

    result
  }

  def toLiteral(concept: Concept): ConceptLiteral = concept match {
    case ConceptComplement(c: BaseConcept) => ConceptLiteral(false, c)
    case _: BaseConcept => ConceptLiteral(true, concept)
    case UniversalRoleRestriction(_, _:BaseConcept) => ConceptLiteral(true, concept)
    case ExistentialRoleRestriction(_, _:BaseConcept) => ConceptLiteral(true, concept)
    case UniversalRoleRestriction(_, _: BaseConcept) => ConceptLiteral(true, concept)
    case MinNumberRestriction(_, _, _: BaseConcept) => ConceptLiteral(true, concept)
    case MaxNumberRestriction(_, _, ConceptComplement(_: BaseConcept)) => ConceptLiteral(true, concept)
  }


  def prepare(dlStatements: Iterable[DLStatement]): ConceptConjunction = {
    //    initDefinitions()
    val withoutRBox = dlStatements.filterNot(_.isInstanceOf[RoleAxiom])
    //    val withoutIndividuals = withoutRBox.map(replaceIndividuals)


    val concepts = withoutRBox.flatMap(x => replaceFillers(nnf(toConcept(x))))
    val result = cnf(ConceptConjunction(concepts.toSet[Concept])).asInstanceOf[ConceptConjunction]

    result
  }

  // Replace fillers by newly introduced concepts

  var knownIndividuals: Map[Individual, IndividualDefiner] = _


  var reuseDefiners = true

  var definitions: List[Concept] = _
  var known: Map[Concept, BaseConcept] = _

  var definerMap: Map[BaseConcept, Concept] = Map() // only for debugging purposes

  var definers: Set[BaseConcept] = _


  def initDefinitions() = {
    definitions = List()
    known = Map()
    //    definerMap = Map()
    definers = Set()
    knownIndividuals = Map()
  }

  def isDefiner(c: Concept) = c match {
    case c: BaseConcept => c.name.startsWith("_D") || (definers!=null && definers(c))
    case _ => false
  }

  // def replaceIndividuals(dlStatement: DLStatement): Axiom = dlStatement match { 
  //   case axiom: Axiom => axiom
  //   case ConceptAssertion(c, i) => Subsumption(getIndividualDefiner(i),c)
  //   case RoleAssertion(r,i1,i2) => Subsumption(getIndividualDefiner(i1), 
  // 					       ExistentialRoleRestriction(r, getIndividualDefiner(i2)))
  //   case _ => assert(false, "not expected "+dlStatement); null 
  // }

  // def getIndividualDefiner(individual: Individual) = knownIndividuals.get(individual) match { 
  //   case Some(definer) => definer
  //   case None => { 
  //     val definer = individualDefiner(individual)
  //     knownIndividuals += ((individual, definer))
  //     definer
  //   }
  // }

  /**
   * Flatten a concept by replacing all fillers by newly introduced definers.
   * The result is a set of concepts, the first being the original concept with fillers replaced,
   * the remaining elements representing the definitions of the newly introduced symbols
   */
  def replaceFillers(concept: Concept): Iterable[Concept] = {

    var newDefinitions = List[Concept]()
    logger.trace("replacing fillers in " + concept.toString)

    var universalRestrictions = 0
    var derivationCandidate = false

    concept match {
      case ConceptDisjunction(ds) => if(ds.exists(_ match {
        case ConceptComplement(dc: DerivationCandidate) =>  true
        case _ => false
      }))
        derivationCandidate = true
      case _ => ;
    }

    def getDefiner(concept: Concept, derivationCandidate: Boolean): BaseConcept = {
      if (reuseDefiners) {
        // if(isDefiner(concept))
        // 	concept.asInstanceOf[BaseConcept]
        // else
        known.get(concept) match {
          case Some(definer) =>
            assert(!derivationCandidate || definer.isInstanceOf[DerivationCandidate])
            logger.trace("reusing definition " + definer.toString)
            definer
          case None =>
            if (isDefiner(concept))
              concept.asInstanceOf[BaseConcept]
            else {
              val definer = newConcept(derivationCandidate)
              definers += definer
              newDefinitions = DLHelpers.disjunction(Set(DLHelpers.neg(definer), concept)) :: newDefinitions
              known += ((concept, definer))
              definerMap += ((definer, concept))
              logger.trace("new definer for " + concept.toString + ": " + definer.toString)
              definer
            }
        }
      } // <----- checked, looks correct
      else {
      val definer = newConcept(derivationCandidate)
      definers += definer
      newDefinitions = DLHelpers.disjunction(Set(DLHelpers.neg(definer), concept)) :: newDefinitions
      known += ((concept, definer))
      definerMap += ((definer, concept))
      logger.trace("new definer for " + concept.toString + ": " + definer.toString)
      definer
    }
    }


    def inner(concept: Concept, derivationCandidate: Boolean): Concept = concept match {
      case b: BaseConcept => assert(!Set("TOP", "BOTTOM")(b.name), "Base concept "+b+"! expected?"); b
      case TopConcept => TopConcept
      case BottomConcept => BottomConcept
      case ConceptComplement(a: BaseConcept) => ConceptComplement(a)
      case ExistentialRoleRestriction(r, f) =>
        ExistentialRoleRestriction(r, getDefiner(inner(f, derivationCandidate), derivationCandidate))
      case UniversalRoleRestriction(r, f) =>
        universalRestrictions += 1
        UniversalRoleRestriction(r, getDefiner(inner(f, derivationCandidate), derivationCandidate))
      case MinNumberRestriction(n, r, f) => MinNumberRestriction(n, r, getDefiner(inner(f, derivationCandidate), derivationCandidate))
      case MaxNumberRestriction(n, r, ConceptComplement(f)) =>
        MaxNumberRestriction(n, r, ConceptComplement(getDefiner(inner(f, derivationCandidate), derivationCandidate)))
      case ConceptConjunction(cs) => ConceptConjunction(cs.map(inner(_, derivationCandidate)))
      // case ConceptDisjunction(ds) if(ds.exists(_.isInstanceOf[IndividualDefiner])) => 
      // 	ConceptDisjunction(ds.map(inner(_, derivationCandidate=true)))
      case ConceptDisjunction(ds) =>
        ConceptDisjunction(ds.map(inner(_, derivationCandidate)))
      case p =>
        assert(false, "Not implemented: "+p.toString)
        null
    }

    val c = inner(concept, derivationCandidate=false)

    logger.debug("resulted in "+c.toString)

    //    println(universalRestrictions+" universal restrictions")

    definitions ++= newDefinitions

    c::newDefinitions
  }

  def toConcept(dlStatement: DLStatement): Concept = {


    val result = dlStatement match {
      case Subsumption(TopConcept, b) => b
      case Subsumption(a, b) => ConceptDisjunction(Set(ConceptComplement(a), b)) //DLHelpers.disjunction(Set(DLHelpers.neg(a), b))
      case ConceptEquivalence(a, b) =>
        DLHelpers.conjunction(Set(toConcept(Subsumption(a,b)), toConcept(Subsumption(b, a))))
      case d =>
        assert(false, "Not supported: "+d)
        null
    }

    result
  }

  def nnf(concept: Concept): Concept = concept match {

    case ConceptComplement(TopConcept) => BottomConcept
    case ConceptComplement(BottomConcept) => TopConcept
    case ConceptComplement(b: BaseConcept) => ConceptComplement(b)
    case ConceptComplement(MinNumberRestriction(n, r, f)) => MaxNumberRestriction(n-1, r, ConceptComplement(nnf(ConceptComplement(f))))
    case ConceptComplement(MaxNumberRestriction(n, r, f)) => MinNumberRestriction(n+1, r, nnf(f))
    case ConceptComplement(ExistentialRoleRestriction(r, f)) => UniversalRoleRestriction(r, nnf(ConceptComplement(f)))
    case ConceptComplement(UniversalRoleRestriction(r, f)) => ExistentialRoleRestriction(r, nnf(ConceptComplement(f)))
    case ConceptComplement(ConceptConjunction(cs)) => ConceptDisjunction(cs.map(f => nnf(ConceptComplement(f))))
    case ConceptComplement(ConceptDisjunction(ds)) => ConceptConjunction(ds.map(f => nnf(ConceptComplement(f))))
    case ConceptComplement(ConceptComplement(f)) => nnf(f)
    //    case ConceptComplement(f) => ConceptComplement(nnf(f))
    case MinNumberRestriction(n, r, f) => MinNumberRestriction(n, r, nnf(f))
    case MaxNumberRestriction(n, r, ConceptComplement(f)) => MaxNumberRestriction(n, r, ConceptComplement(nnf(f)))
    case MaxNumberRestriction(n, r, f) => MaxNumberRestriction(n, r, ConceptComplement(nnf(ConceptComplement(f))))
    case ExistentialRoleRestriction(r, f) => ExistentialRoleRestriction(r, nnf(f))
    case UniversalRoleRestriction(r, f) => UniversalRoleRestriction(r, nnf(f))
    case ConceptConjunction(cs) => ConceptConjunction(cs.map(nnf))
    case ConceptDisjunction(ds) => ConceptDisjunction(ds.map(nnf))
    case b: BaseConcept => b
    case TopConcept => TopConcept
    case BottomConcept => BottomConcept
  }




  var conceptCounter = 0
  override def newConcept(): BaseConcept =
    newConcept(derivationCandidate=false)

  def newConcept(derivationCandidate: Boolean): BaseConcept = {
    conceptCounter += 1
    val name = "_D"+conceptCounter.toString
    val result = if(derivationCandidate)
      new BaseConcept(name) with DerivationCandidate
    else {
      BaseConcept(name)
    }
    allGeneratedNames += result
    result
  }

  var usedDefinerNames:Set[String] = Set()
  var allGeneratedNames:Set[BaseConcept] = Set()

  def individualDefiner(individual: Individual): IndividualDefiner = {
    val baseName = "_DI_"+individual.name
    var counter = 0
    var name = baseName
    while(usedDefinerNames.contains(name)){
      name = baseName + counter.toString
      counter += 1
    }
    if(leftInRoleAssertion(individual))
      new BaseConcept(name) with IndividualDefiner with DerivationCandidate
    else
      new BaseConcept(name) with IndividualDefiner
  }

  def leftInRoleAssertion(individual: Individual) =
    currentStatements.exists(_ match {
      case RoleAssertion(_, i, _) => i==individual
      case _ => false
    })

  def resetConceptCounter() = conceptCounter=0


  def cnf(concept: Concept): Concept = {
    // Assumption: concept is in nnf

    val result = concept match {
      case TopConcept => ConceptConjunction(Set())
      case BottomConcept => ConceptDisjunction(Set())
      case b: BaseConcept => b
      case ConceptComplement(b: BaseConcept) => ConceptComplement(b)
      case e: ExistentialRoleRestriction => e
      //    case ConceptComplement(e: ExistentialRoleRestriction) => ConceptComplement(e)
      case u: UniversalRoleRestriction => u
      //    case ConceptComplement(u: UniversalRoleRestriction) => ConceptComplement(u)
      case m: MinNumberRestriction => m
      //    case ConceptComplement(m: MinNumberRestriction) => ConceptComplement(m)
      case m: MaxNumberRestriction => m
      //    case ConceptComplement(m: MaxNumberRestriction) => ConceptComplement(m)


      case ConceptConjunction(cs) =>  {
        val flat = flattenConjuncts(cs.map(cnf))
        if(flat(BottomConcept))
          ConceptConjunction(Set())
        else
          ConceptConjunction(flat)
      }

      case ConceptDisjunction(ds) => {
        val flat = flattenDisjuncts(ds) // looks good
        if(flat(TopConcept))
          ConceptConjunction(Set())
        else{
          val c = flat.find(_.isInstanceOf[ConceptConjunction]) // looks good
          c match {
            case None => new ConceptConjunction(Set(ConceptDisjunction(flat))) // looks good
            case Some(conjunction: ConceptConjunction) => {
              val ds2 = flat - conjunction
              ConceptConjunction(
                flattenConjuncts(conjunction.conjuncts.map(x => cnf(ConceptDisjunction(ds2 + x)))))
            }
          }
        }
      }


    }

    result
  }

  def flattenConjuncts(conjuncts: Set[Concept]): Set[Concept] =
    conjuncts.flatMap { _ match {
      case ConceptConjunction(cs) => flattenConjuncts(cs)
      case TopConcept => Set.empty[Concept]
      case c => Set(c)
    }}


  def flattenDisjuncts(disjuncts: Set[Concept]): Set[Concept] =
    disjuncts.flatMap { _ match {
      case ConceptDisjunction(cs) => flattenDisjuncts(cs)
      case BottomConcept => Set.empty[Concept]
      case c => Set(c)
    }}


  def definitionMapping(clauses: Iterable[ConceptClause]) =
    clauses.groupBy(_.literals.find((l: ConceptLiteral) => isDefiner(l.concept)) match {
      case Some(definer) => definer.concept
      case _ => TopConcept
    }).map{ case (key, value) =>
      (key, value.map(c => new ConceptClause(c.literals-ConceptLiteral(false,key), c.ordering)))
    }

}



// object CNFTransformer = { 
//   def cnf(axiom: Axiom): Set[Axiom] = axiom match { 
//     case ConceptEquivalence(a, b) => cnf(Subsumption(a, b))++cnf(Subsumption(b, a))
//     case Subsumption(a, b) => { 

//     }
//   }
// }
