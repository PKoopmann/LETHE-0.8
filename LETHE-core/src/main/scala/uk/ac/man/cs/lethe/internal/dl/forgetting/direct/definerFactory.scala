package uk.ac.man.cs.lethe.internal.dl.forgetting.direct

import java.util.Date
import scala.collection.mutable.HashSet
import scala.collection.mutable.{HashMap, MultiMap, Set => MutSet}
import com.typesafe.scalalogging.Logger
import uk.ac.man.cs.lethe.internal.dl.datatypes._
import uk.ac.man.cs.lethe.internal.dl.forgetting.Configuration
import uk.ac.man.cs.lethe.internal.dl.proofs.InferenceLogger
//import uk.ac.man.cs.lethe.internal.tools.FileTools



trait ClauseSetListener{
  def notifyClauseAddition(clauses: Set[ConceptClause])
}

/**
 * Introduction of definers and management of those during computation.
 * @param conceptGenerator
 * @param ordering
 * @param roleHierarchy
 */
class DefinerFactory(conceptGenerator: ConceptGenerator,
                     ordering: Ordering[ConceptLiteral],
                     roleHierarchy: RoleHierarchy=null) {
  // TODO argument roleHierarchy actually not needed

  val logger = Logger[DefinerFactory]

  val inferenceLogger = InferenceLogger



  val knownDefiners = new HashMap[DefinerBase, BaseConcept]()// concepts to definer
  val definerBases = new HashMap[BaseConcept, DefinerBase]() // definer to origin
  //  val knownRepresentations = new HashMap[Concept, BaseConcept]()

  def addDefinerBases(definerFactory: DefinerFactory) = {
    definerFactory.definerBases.foreach(entry => definerBases.put(entry._1,entry._2))
  }

  def getBaseDefiners(definer: BaseConcept): Set[BaseConcept] = {
    definerBases.get(definer) match {
      case Some(definerBase) => {
        assert(definerBases(definer).minus.isEmpty,
          "definer not universally quantified: minus not empty! "+definer)
        definerBase.plus
      }
      case None => Set(definer) // assume base definer
    }
  }


  def getNegUniv(restriction: UniversalRoleRestriction) = restriction match {
    case UniversalRoleRestriction(r, definer: BaseConcept) =>
      NegUniv(r, getBaseDefiners(definer))
  }

  def getDefinerBase(definer: BaseConcept): DefinerBase = definerBases.get(definer) match {
    case Some(definerBase) => definerBase
    case None => DefinerBase(Set(definer), Set())
  }

  // (D1 n D2) subsumes (D1 n D2 n D3)
  def subsumes(definer1: BaseConcept, definer2: BaseConcept) = {
    val (base1, base2) = (getDefinerBase(definer1), getDefinerBase(definer2))

    base1.plus.forall(base2.plus) && base1.minus.forall(base2.minus)
  }


  val listeners = new HashSet[ClauseSetListener]()

  def addListener(listener: ClauseSetListener) =
    listeners.add(listener)

  def removeListener(listener: ClauseSetListener) =
    listeners.remove(listener)

  def notifyClauseAddition(clauses: Set[ConceptClause]) = {
    logger.trace(s"notifying listeners about ${clauses}")
    logger.trace(s"currently listening are: ${listeners}")
    listeners.foreach{ _.notifyClauseAddition(clauses) }
  }


  // returns a representative of the conjunction of concept1 and concept2, if existent
  def representative(definer1: BaseConcept, definer2: BaseConcept): Option[BaseConcept] = {
    val (base1, base2) = (getDefinerBase(definer1), getDefinerBase(definer2))

    val base = base1.combine(base2)

    knownDefiners.get(base)
  }

  def representative(definers: Iterable[BaseConcept]) = {
    val base = definers.flatMap(getBaseDefiners)
    knownDefiners.get(DefinerBase(base.toSet, Set()))
  }

  def combine(concept1: Concept, concept2: Concept): Concept = {
    (concept1, concept2) match {
      case (definer1: BaseConcept, definer2: BaseConcept) => combineDefiners(definer1, definer2)._1
      case (disj: ConceptDisjunction, definer: BaseConcept) => combine(definer, disj)
      case (definer: BaseConcept, ConceptDisjunction(ds)) => {
        ConceptDisjunction(ds.map(c => combineDefiners(definer, c.asInstanceOf[BaseConcept])._1))
      }
      case _ => assert(false, "unexpected combination: "+concept1+" "+concept2); null
    }
  }

  def combineDefiners(definer1: BaseConcept, definer2: BaseConcept): (BaseConcept, Set[ConceptClause]) = {
    val (base1, base2) = (getDefinerBase(definer1), getDefinerBase(definer2))

    val base = base1.combine(base2)

    knownDefiners.get(base) match {
      case Some(definer) => (definer, Set())
      case None => {
        val derivationCandidate = (definer1.isInstanceOf[DerivationCandidate] &&
          definer2.isInstanceOf[DerivationCandidate])

        val newDefiner = conceptGenerator.newConcept(derivationCandidate)
        logger.trace(s"New definer: ${newDefiner} = ${base}")
        knownDefiners.put(base, newDefiner)
        definerBases.put(newDefiner, base)

        val newClauses =
          Set(makeDefiningClause(newDefiner, definer1),
            makeDefiningClause(newDefiner, definer2))

        notifyClauseAddition(newClauses)
        inferenceLogger.sendInputClauses(newClauses)


        (newDefiner, newClauses)
      }
    }
  }

  def newDefiner() =
    conceptGenerator.newConcept(false)


  def substract(definer1: BaseConcept,
                minus: UniversalRoleRestriction,
                restClause: ConceptClause): (BaseConcept, Set[ConceptClause]) = {
    val base1 = getDefinerBase(definer1)
    val negUniv = getNegUniv(minus)
    val base = DefinerBase(base1.plus, base1.minus+negUniv)

    knownDefiners.get(base) match {
      case Some(definer) => {
        logger.trace("Known: "+base+" = "+definer)
        (definer, Set(makeDefiningClause(definer, restClause)))
      }
      case None => {
        val newDefiner = conceptGenerator.newConcept()
        logger.trace("New definer: "+newDefiner+" = "+base)
        knownDefiners.put(base, newDefiner)
        definerBases.put(newDefiner, base)
        (newDefiner,
          Set(makeDefiningClause(newDefiner, definer1),
            makeDefiningClause(newDefiner, restClause)))
      }
    }
  }

  // def getBaseDefiners(concept: Concept): Set[Concept] = concept match { 
  //   case b: BaseConcept => baseDefiners.get(b) match { 
  //     case Some(set) => set
  //     case None => Set(concept)
  //   }
  //   case cl: ConceptClause => { 
  //     Set(new ConceptClause(cl.literals.flatMap(_ match { 
  // 	case ConceptLiteral(false, definer) => getBaseDefiners(definer).map(ConceptLiteral(false, _))
  // 	case ConceptLiteral(true, ExistentialRoleRestriction(r, definer)) =>
  // 	  getBaseDefiners(definer).map(d => ConceptLiteral(true, ExistentialRoleRestriction(r, d)))
  // 	case l => Set(l)
  //     }), ordering))
  //   }
  //   case c => Set(c)
  // }

  // def getRepresentation(concept: Concept): Concept = { 

  //   def inner(concept: Concept): Concept = { 
  //     concept match { 
  // 	case b: BaseConcept => baseDefiners.get(b) match { 
  // 	  case None => b
  // 	  case Some(set) => ConceptConjunction(set.map(inner))
  // 	}
  // 	case cl: ConceptClause => ConceptDisjunction(cl.literals.map(inner))
  // 	case ConceptLiteral(true, c) => getRepresentation(c)
  // 	case ConceptLiteral(false, c) => ConceptComplement(inner(c))
  // 	case ExistentialRoleRestriction(r, c) => ExistentialRoleRestriction(r, inner(c))
  // 	case UniversalRoleRestriction(r, c) => UniversalRoleRestriction(r, inner(c))
  // 	case ConceptConjunction(cs) => ConceptConjunction(cs.map(inner))
  //     }
  //   }
  //   return DLHelpers.simplify(simplify(inner(concept)))
  // }

  // def getDefiner(concept1: Concept, concept2: Concept): (BaseConcept, Set[ConceptClause]) = { 
  //   val base = getBaseDefiners(concept1)++getBaseDefiners(concept2)
  //   var representation = getRepresentation(ConceptConjunction(Set(concept1, concept2)))

  //   knownRepresentations.get(representation) match { 
  //     case Some(definer) => logger.trace("Representation known: "+definer); return (definer, Set())
  //     case None => ;
  //   }
  //   knownDefiners.get(base) match { 
  //     case Some(definer) => logger.trace("Reusing "+definer+" for "+base); (definer, Set())
  //     case None => createNewDefiner(Set(concept1, concept2))
  //   }
  // }

  // def undoDefiner(definer: BaseConcept) = { 
  //   baseDefiners.get(definer) match { 
  //     case Some(definition) => { 
  // 	knownDefiners.remove(definition)
  // 	baseDefiners.remove(definer)
  //     }
  //     case None => ;
  //   }
  // }

  // def createNewDefiner(concepts: Set[Concept]): (BaseConcept, Set[ConceptClause]) = { 
  //   val newDefiner = conceptGenerator.newConcept()
  //   logger.trace("Creating new definer "+newDefiner+" for "+concepts)
  //   val definition = concepts.flatMap(getBaseDefiners)
  //   knownDefiners.put(definition, newDefiner)
  //   baseDefiners.put(newDefiner, definition)
  //   logger.trace("Representation: "+getRepresentation(ConceptConjunction(concepts)))
  //   knownRepresentations.put(getRepresentation(ConceptConjunction(concepts)), newDefiner)
  //   (newDefiner, concepts.map(makeDefiningClause(newDefiner, _)))
  // }

  def makeDefiningClause(definer: BaseConcept, concept: Concept) = concept match {
    case b: BaseConcept => new ConceptClause(Set(ConceptLiteral(false, definer),
      ConceptLiteral(true, b)),
      ordering)
    case c: ConceptClause => new ConceptClause(c.literals + ConceptLiteral(false, definer),
      ordering)
    // no other concepts supported
  }


  // def simplify(concept: Concept): Concept = DLHelpers.simplify(concept) match{ 
  //   case ExistentialRoleRestriction(r, ExistentialRoleRestriction(s, c)) 
  //   if roleHierarchy.equiv(r, InverseRole(s)) => simplify(c)
  //   case TopConcept => TopConcept
  //   case BottomConcept => BottomConcept
  //   case b: BaseConcept => b
  //   case ConceptComplement(c) => DLHelpers.simplify(ConceptComplement(simplify(c)))
  //   case ConceptConjunction(cs) => DLHelpers.simplify(ConceptConjunction(cs.map(simplify)))
  //   case ConceptDisjunction(ds) if (ds.forall(_.isInstanceOf[ExistentialRoleRestriction]) &&
  //   ds.map(_ match { 
  //     case ExistentialRoleRestriction(r, _) => r
  //   }).toSet.size == 1) => { 
  //     val exs = ds.map(_.asInstanceOf[ExistentialRoleRestriction])
  //     simplify(ExistentialRoleRestriction(exs.head.role, ConceptDisjunction(exs.map(_.filler))))
  //   }
  //   case ConceptDisjunction(ds) => DLHelpers.simplify(ConceptDisjunction(ds.map(simplify)))
  //   case ExistentialRoleRestriction(r, c) => DLHelpers.simplify(ExistentialRoleRestriction(r, simplify(c)))
  //   case UniversalRoleRestriction(r, c) => DLHelpers.simplify(UniversalRoleRestriction(r, simplify(c)))
  // }
}

// represents base of any definer
// represent negated universal restriction, ¬Ar(D_1 n ... D_n)
case class NegUniv(role: Role, baseDefiners: Set[BaseConcept]) {
  override def toString = "¬A"+role+"."+baseDefiners.mkString("(", " n ", ")")
}
case class DefinerBase(plus: Set[BaseConcept], minus: Set[NegUniv]) {
  def combine(second: DefinerBase) = DefinerBase(plus++second.plus, minus++second.minus)
  override def toString = (plus++minus).mkString("("," n ",")")
}

trait DefinerSubsumptionChecker extends SubsumptionChecker {

  var definerFactory: DefinerFactory = _

  def setDefinerFactory(definerFactory: DefinerFactory) =
    this.definerFactory = definerFactory

  // check definer2 <= definer
  // A n B n C subsumes A n B
  override def subsumes(definer1: BaseConcept, definer2: BaseConcept) = {
    assert(definerFactory!=null)


    super.subsumes(definer1, definer2) ||
      definerFactory.subsumes(definer2, definer1) // done with purpose (other method should be renamed)
    //    definerFactory.getBaseDefiners(definer1).forall(definerFactory.getBaseDefiners(definer2))
  }

  override def subsumes(literal1: ConceptLiteral, literal2: ConceptLiteral) = (literal1, literal2) match {
      // subsumption between definers should only be used for definers under role restrictions
    case (ConceptLiteral(false, d1: BaseConcept), ConceptLiteral(false, d2: BaseConcept)) =>  d1==d2
    case (ConceptLiteral(true, d1: BaseConcept), ConceptLiteral(true, d2: BaseConcept)) => d1==d2
    case _ => super.subsumes(literal1, literal2)
  }
}
