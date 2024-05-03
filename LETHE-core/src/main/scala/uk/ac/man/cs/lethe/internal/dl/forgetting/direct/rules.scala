package uk.ac.man.cs.lethe.internal.dl.forgetting.direct

import java.util.Date
import scala.collection.mutable.HashSet

import scala.collection.mutable.{ HashMap, MultiMap, Set => MutSet }

//import com.dongxiguo.fastring.Fastring.Implicits._
import com.typesafe.scalalogging.Logger


import uk.ac.man.cs.lethe.internal.dl.datatypes._
import uk.ac.man.cs.lethe.internal.dl.forgetting.{ Configuration, DirectALCForgetter } // latter only needed for inverse roles (experimental)
//import uk.ac.man.cs.lethe.internal.tools.FileTools


abstract class AbstractDerivation(premisses: Iterable[_ <: Expression],
                                  conclusions: Iterable[_ <: Expression],
                                  ruleName: String) {
  override def toString = premisses.mkString(", ")+"  --->  "+conclusions.mkString(", ") + "  ("+ruleName+")"

  val getRuleName: String = ruleName

  def copy: AbstractDerivation

  def premisses(): Iterable[_ <: Expression] = premisses
  def conclusions(): Iterable[_ <: Expression] = conclusions
}

case class Derivation(override val premisses: Iterable[ConceptClause],
                      override val conclusions: Iterable[ConceptClause],
                      rule: Rule) extends AbstractDerivation(premisses, conclusions, rule.getClass.getSimpleName()) {
  override def copy =
    Derivation(Set()++premisses, Set()++conclusions, rule);
}

case class ExtendedDerivation(override val premisses: Iterable[Expression],
                              override val conclusions: Iterable[Expression],
                              ruleName: String) extends AbstractDerivation(premisses, conclusions, ruleName) {
  override def copy =
    ExtendedDerivation(Set()++premisses, Set()++conclusions, ruleName);
}

abstract class Rule {

  import Configuration._
  import Configuration.Method._

  def getDerivations(clause: ConceptClause, clauses: Iterable[ConceptClause]): Iterable[Derivation]

  def valid(cc: ConceptClause): Boolean = {
    // if(method==METHOD2)
    //   true
    // else 
    cc.literals.filter(x => ALCFormulaPreparations.isDefiner(x.concept)).size<2
  }

}


// object RuleLogger

/**
 * Resolution Rule.
 * If "onlyFirstSideClause" is set to true (standard behaviour), only resolves in
 * maximal literals in both clauses, unless if a definer is involved. For most cases,
 * this behaviour is sufficient. If the rule is used for forgetting with background,
 * it should be set to "false", though, since otherwise important inferences may be
 * skipped.
 */
class ResolutionRule(ordering: Ordering[ConceptLiteral],
                     onlyFirstSideClause: Boolean = true) extends Rule {
  //  implicit val (logger, formatter, appender) = ZeroLoggerFactory.newLogger(RuleLogger)
  //  import formatter._

  val logger = Logger[ResolutionRule]

  var timeUsed = 0L

  var ignoreInvalid = true

  var onlyMaxLiterals = true // only resolve on maximal literals in both clauses

  override def getDerivations(clause: ConceptClause, clauses: Iterable[ConceptClause]) = {
    val selected = clause.literals.head

    logger.trace(s"check for resolvents of ${clause}")
    logger.trace(s"with ${clauses}")


    if(onlyFirstSideClause && !ALCFormulaPreparations.isDefiner(selected.concept))
      fastGetDerivations(clause, clauses, selected)

    val start = new Date().getTime()
    //    val result = clauses.view.flatMap {
    val result = clauses.flatMap {
      x=>
        logger.trace(s"check side clause: ${x}")
        val sideConceptLiteral = ConceptLiteral(!selected.polarity, selected.concept)
        logger.trace(s"side concept literal: ${sideConceptLiteral}")
        if(x.literals.contains(sideConceptLiteral)) {
          logger.trace("  resolving with " + x.toString)
          val newClause = new ConceptClause(clause.literals.tail ++ (x.literals - sideConceptLiteral), ordering)
          if(!valid(newClause) && ignoreInvalid){
            logger.trace("- not valid")
            None
          } else
            Some(Derivation(premisses = Set(clause, x),
              conclusions = Set(newClause), rule = this))
        } else
          None
    }

    timeUsed += new Date().getTime() - start
    result
  }

  // applied if first literal is not a definer
  // difference: in side clauses, we only check the first literal
  private def fastGetDerivations(clause: ConceptClause, clauses: Iterable[ConceptClause], selected: ConceptLiteral) = {
    val start = new Date().getTime()
    //    val result = clauses.view.flatMap {
    val result = clauses.flatMap {
      x=>
        val sideConceptLiteral = ConceptLiteral(!selected.polarity, selected.concept)
        if(x.literals.head==sideConceptLiteral) {
          logger.trace("  resolving with " + x.toString)
          val newClause = new ConceptClause(clause.literals.tail ++ x.literals.tail, ordering)
          if(!valid(newClause)){
            logger.trace("- not valid")
            None
          } else
            Some(Derivation(premisses = Set(clause, x),
              conclusions = Set(newClause), rule = this))
        } else
          None
    }

    timeUsed += new Date().getTime() - start
    result

  }

}


/**
  * Main clause expresses unsatisfiability of definer, side clauses contain existential restrictions on that definer,
  * which are then removed as result of the rule application.
  */
object ExistentialRoleRestrictionEliminationRule1 extends Rule {

  val logger = Logger(ExistentialRoleRestrictionEliminationRule1.getClass)

  override def getDerivations(clause: ConceptClause, clauses: Iterable[ConceptClause])
  : Set[Derivation] = {

    if(unaryDefinerClause(clause)){
      val definer = clause.literals.head.concept

      return clauses.flatMap{ clause2 =>
        val remove = clause2.literals.filter{ _.concept match {
          case ExistentialRoleRestriction(_, d) => d==definer
          case _ => false
        } }

        if(!remove.isEmpty)
          Some(Derivation(premisses = Set(clause, clause2),
            conclusions = Set(clause2.without(remove)),
            ExistentialRoleRestrictionEliminationRule1))
        else
          None
      }.toSet
    }

    return Set()
  }

  def unaryDefinerClause(clause: ConceptClause): Boolean = {
    if(clause.literals.size!=1)
      return false

    val l = clause.literals.head

    !l.polarity==false && ALCFormulaPreparations.isDefiner(l.concept)

  }
}

/**
  * As ExistentialRoleRestrictionEliminationRule1, but negative definer clause as side clause.
  */
object ExistentialRoleRestrictionEliminationRule2 extends Rule {

  val logger = Logger(ExistentialRoleRestrictionEliminationRule2.getClass)

  override def getDerivations(clause: ConceptClause, clauses: Iterable[ConceptClause])
  : Iterable[Derivation] = {

    val sideClauses = MutSet[ConceptClause]()

    val remove = clause.literals.filter{ _.concept match {
      case ExistentialRoleRestriction(_, d) =>
        val clause2 = new ConceptClause(Set(ConceptLiteral(false, d)), clause.ordering)
        if(clauses.toSet(clause2)) {
          sideClauses.add(clause2)
          true
        }
        else
          false
      case _ => false
    } }

    if(!remove.isEmpty)
      return Some(Derivation(premisses=sideClauses+clause,
        conclusions = Set(clause.without(remove)),
        rule = ExistentialRoleRestrictionEliminationRule2))
    else
      return None
  }
}


object AxiomTracker {
  var keys = HashMap[RoleSubsumption, BaseConcept]()
  var counter = 0

  def getConcept(axiom: RoleSubsumption) =
    if(keys.contains(axiom))
      keys(axiom)
    else{
      counter +=1
      keys.put(axiom, BaseConcept("TRACK_"+counter))
      keys(axiom)
    }

  def clean() =
    keys = HashMap[RoleSubsumption, BaseConcept]()
}


class RolePropagationRule(ordering: Ordering[ConceptLiteral],
                          forgettables: Set[String],
                          definerFactory: DefinerFactory,
                          roleHierarchy: RoleHierarchy,
                          trackRoleAxioms: Boolean = false) extends Rule {

  //  implicit val (logger, formatter, appender) =
  //  ZeroLoggerFactory.newLogger(RuleLogger)
  val logger = Logger[RolePropagationRule]

  var everythingInteresting = false

  var timeUsed = 0L

  var newSymbols: Set[String] = Set()

  var onlyInterestingCombinations: Boolean = true // not sound ?

  var reachable: Map[BaseConcept, Set[String]] = Map()

  var positive: Set[BaseConcept] = _// definers that are connected to a positive literal of the current concept
  // to be forgotten
  var negative: Set[BaseConcept] = _ // definers that are connected to a negative literal of the current concept

  var positiveDist: Map[BaseConcept, Set[Int]] = _
  var negativeDist: Map[BaseConcept, Set[Int]] = _
  var cyclic: Set[BaseConcept] = _

  override def getDerivations(clause: ConceptClause, clauses: Iterable[ConceptClause])
  : Iterable[Derivation] = {

    assert(forgettables!=null)

    // only apply rule if max element is restriction
    if(clause.literals.isEmpty || clause.literals.head.concept.isInstanceOf[BaseConcept])
      return Set()

    val start = new Date().getTime()

    // general remark of what follows:
    // if we are forgetting roles, all definer pairs and definers will be interesting
    // since the sets positive and positiveDist will not be instantiated
    // TODO check: can we still use this optimisation for roles, treating exists as positive and forall as negative?
    // TODO check: if we are forgetting the current role, are inferences between universal role restrictions still interesting?

    val restrictions = getRoleRestrictions(clause)

    var result = Set[Derivation]()

    //val result = 
    restrictions.foreach { restriction =>
      val role = restriction.role

      if(clause.isDerivationCandidate
        || role.signature.exists(forgettables)
        || interestingDefiner(restriction.filler.asInstanceOf[BaseConcept], clauses)
        || DirectALCForgetter.inverseRoles // do all role propagations if inverse roles (experimental)
      ) {

        logger.trace("checking role propagations for role " + role.toString)
        clauses.foreach { clause2 =>
          logger.trace(s"clause2: ${clause2}")
          logger.trace(s"parents of clause2: ${clause2.parents.mkString("{\"", "\", \"", "\"}")}")
          if (valid(new ConceptClause(clause.literals ++ clause2.literals, ordering))) {
            val corr = getCorrespondingRoleRestrictions(role, clause2)
            //	  var result = Set[Derivation]()

            corr.foreach { pair =>
              if (clause.isDerivationCandidate
                || role.signature.exists(forgettables)
                || (interestingDefiner(pair._2, clauses)
                && interestingFillerPair(definer1 = restriction.filler.asInstanceOf[BaseConcept],
                                         definer2 = pair._2,
                                         clauses = clauses))
                || DirectALCForgetter.inverseRoles) {
                result ++= rolePropagate(clause, clause2, restriction, pair._1, pair._2, clauses)
              }
            }
            //println("result for "+clause2+": "+result)
            logger.trace(s"current result after ${clause2}: ${result}")
          }
        }
      }
    }

    logger.trace(s"Overall result: ${result}")

    timeUsed += new Date().getTime() - start
    result
  }


  def rolePropagate(clause1: ConceptClause,
                    clause2: ConceptClause,
                    restriction1: RoleRestriction,
                    restriction2: RoleRestriction,
                    filler2: Concept,
                    clauses: Iterable[ConceptClause]): Option[Derivation] = {

    if(restriction1.isInstanceOf[ExistentialRoleRestriction] && restriction2.isInstanceOf[ExistentialRoleRestriction])
      return None

    logger.trace(s"Role propagation between ${clause1.toString} and ${clause2.toString} on ${restriction1.toString}")

    // if(areIndividualPremisses(clause1, filler2))
    //   return individualsRolePropagate(clause1, clause2, restriction1, filler2)

    val definer1 = restriction1.filler.asInstanceOf[BaseConcept]
    val definer2 = filler2.asInstanceOf[BaseConcept]

    val (definer, definition) = definerFactory.combineDefiners(definer1, definer2)


    val newClause = new ConceptClause( (clause1.literals - ConceptLiteral(true, restriction1))
      ++ (clause2.literals - ConceptLiteral(true, restriction2))
      + ConceptLiteral(true, newRestriction(restriction1, restriction2, definer)),
      ordering)

    if(!definition.isEmpty && positive!=null){  // only updated if it has been instantiated 
      // (role propagation does not instantiate it yet)
      // update connections to the current symbol
      if(positive(definer1)
        || positive(definer2)){
        positive += definer
      }
      if(negative(definer1)
        || negative(definer2)){
        negative += definer
      }

      def getPosDist(definer: BaseConcept) = positiveDist.getOrElse(definer, Set[Int]())
      def getNegDist(definer: BaseConcept) = negativeDist.getOrElse(definer, Set[Int]())

      positiveDist = positiveDist.updated(definer, getPosDist(definer1)++getPosDist(definer2))
      negativeDist = negativeDist.updated(definer, getNegDist(definer1)++getNegDist(definer2))
      if(cyclic(definer1)||cyclic(definer2))
        cyclic += definer
    }

    logger.trace(s"positiveDist: ${positiveDist}")
    logger.trace(s"negativeDist: ${negativeDist}")
    logger.trace(s"cyclic: ${cyclic}")


    if(Set(clause1, clause2).contains(newClause)){ // || subsume(clauses, newClause)){ 
      return None//definition <--- Whats the point in returning definition? should be empty?
    }
    else if(trackRoleAxioms) {
      val superRole = restriction1.role
      val justifications: Set[Set[RoleSubsumption]] = restriction2 match {
          case ExistentialRoleRestriction(subRole, _) => roleHierarchy.justificationsFor(subRole, superRole)
          case UniversalRoleRestriction(subRole, _) => roleHierarchy.justificationsFor(subRole, superRole)
      }
      val newClauses = justifications.map{
        just => new ConceptClause(just.map(ax => ConceptLiteral(true,AxiomTracker.getConcept(ax))))
      }.map(newClause._with)

      logger.trace(s"result of role propagation: ${definition++newClauses}")

      return Some(Derivation(premisses = Set(clause1, clause2),
        conclusions = definition++newClauses, rule = this))
    }
    else {
      logger.trace(s"result of role propagation: ${definition+newClause}")
      return Some(Derivation(premisses = Set(clause1, clause2),
        conclusions = definition+newClause, rule = this))
    }
  }

  def removeNegDefiner(clause: ConceptClause) = {
    clause.literals.filter{ _ match {
      case ConceptLiteral(neg, d) if ALCFormulaPreparations.isDefiner(d) => false
      case _ => true
    }}
  }

  /*
   * check whether the premisses qualify for the individuals role propagation
   */
  def areIndividualPremisses(univClause: ConceptClause, secondFiller: Concept): Boolean = {
    if(!secondFiller.isInstanceOf[IndividualDefiner])
      return false

    return true
    // minimal requirements - everything else should follow from this
  }

  def sameNegDefiner(clause1: ConceptClause, clause2: ConceptClause) = {
    def negDef(clause: ConceptClause) = clause1.literals.collect{
      case ConceptLiteral(false, d: Concept) if ALCFormulaPreparations.isDefiner(d) => d
    }
    val def1 = negDef(clause1)
    val def2 = negDef(clause2)

    def1.forall(def2) || def2.forall(def1)
  }

  /*
   * apply the variant of role propagation for individuals
   */
  def individualsRolePropagate(prem1: ConceptClause,
                               prem2: ConceptClause,
                               indRestriction: ExistentialRoleRestriction): Option[Derivation] = {
    println("Individual role propagation!")

    if(!sameNegDefiner(prem1, prem2))
      return None

    var restrictions = getCorrespondingRoleRestrictions(indRestriction.role, prem1)

    restrictions = restrictions.filter(pair => pair._1.isInstanceOf[UniversalRoleRestriction])

    if(restrictions.size != removeNegDefiner(prem1).size)
      return None

    val individualDefiner = indRestriction.filler.asInstanceOf[IndividualDefiner]

    val conclusion = new ConceptClause(Set(ConceptLiteral(false, individualDefiner))
      ++ restrictions.map(r => ConceptLiteral(true, r._2)),
      ordering)

    println("Brings: "+conclusion)
    println("From: "+prem1)
    println("and "+prem2)

    Some(Derivation(premisses = Set(prem1, prem2), conclusions= Set(conclusion), rule=this))
  }

  // def individualsRolePropagate(clause1: ConceptClause, 
  // 			       clause2: ConceptClause,
  // 			       restriction: UniversalRoleRestriction, 
  // 			       individualDefiner: Concept) = { 

  //   val withoutNegDef = clause1.literals.filter(_ match { 
  //     case ConceptLiteral(false, d) if isDefiner(d) => false
  //     case _ => true
  //   }

  //   if(withoutNegDef.forall(_.concept.isInstanceOf[UniversalRoleRestriction])) { 
  //     val fillers = withoutNegDef.map(_.concept match { 
  // 	case UniversalRoleRestriction(r, c) => if roleHierarchy.subsumedBy(rm role
  //     }

  //   Some(Derivation(premisses = Set(clause1, clause2),
  // 		    conclusions = Set(new ConceptClause(Set(ConceptLiteral(false, individualDefiner),
  // 							    ConceptLiteral(true, restriction.filler)),
  // 							ordering))))
  // }




  // /**
  //  * Introduce new concepts for all possible combinations of fillers, that can be created using role propagation,
  //  * and return a set of clauses defining these.
  //  */
  // def createAllRoleCombinations(clauses: Iterable[ConceptClause]): Iterable[ConceptClause] = { 
  //   var universalMap = getUniversalRoleFillersMap(clauses)
  //   var existentialMap = getExistentialRoleFillersMap(clauses)

  //   universalMap.keys.flatMap{ key =>
  //     val fillers = universalMap(key)
  //     baseFillers = baseFillers ++ fillers 
  //     fillers.subsets.flatMap{ combination =>
  // 	if(combination.size==1){ 
  // 	  val filler = combination.head
  // 	  fillerDefinitions = fillerDefinitions.updated(filler, Set(filler))
  //         fillerCombinations = fillerCombinations.updated(Set(filler), filler)
  // 	}
  // 	combine(combination) ++ existentialMap(key).flatMap{ c => combine(Set(c) ++ combination) }
  //    }
  //   }
  // }

  // def subsume(clauses: Iterable[ConceptClause], clause: ConceptClause) =
  //   clauses.exists(subsumes(_, clause))


  // def combine(combination: Set[Concept]): Set[ConceptClause] = { 	  
  //   if(combination.size>1 && !fillerCombinations.contains(combination)){ 
  //     val newDefiner = conceptGenerator.newConcept()
  //     newSymbols += newDefiner.name
  //     logger.trace("new role-combination: " + newDefiner.toString + " -> " + combination.toString)

  //     fillerDefinitions = fillerDefinitions.updated(newDefiner, combination)
  //     fillerCombinations = fillerCombinations.updated(combination, newDefiner)

  //     combination.map(filler => new ConceptClause(Set(ConceptLiteral(false, newDefiner), 
  // 						      ConceptLiteral(true, filler)), 
  // 						  ordering))
  //   } else
  //     Set.empty[ConceptClause]
  // }

  /**
   * Return a map that maps each role occuring in the clause set to all fillers which are used with this role
   */
  def getUniversalRoleFillersMap(clauses: Iterable[ConceptClause]): Map[Role, Set[Concept]] = {
    var result: Map[Role, Set[Concept]] = Map().withDefaultValue(Set())

    clauses.flatMap(_.literals).foreach { _.concept match {
      case UniversalRoleRestriction(role, filler) => result = result.updated(role, result(role)+filler)
      case _ => ()
    }}

    result
  }

  def getExistentialRoleFillersMap(clauses: Iterable[ConceptClause]): Map[Role, Set[Concept]] = {
    var result: Map[Role, Set[Concept]] = Map().withDefaultValue(Set())

    clauses.flatMap(_.literals).foreach { _.concept match {
      case ExistentialRoleRestriction(role, filler) => result = result.updated(role, result(role)+filler)
      case _ => ()
    }}

    result
  }



  // override def subsumes(c1: BaseConcept, c2: BaseConcept): Boolean = { 
  //   baseFillersFor(c2).subsetOf(baseFillersFor(c1))
  // }

  // def subsumes(literal1: ConceptLiteral, literal2: ConceptLiteral): Boolean = 
  //   literal1.polarity==literal2.polarity && ((literal1.concept, literal2.concept) match { 
  //     case (a, b) if a==b => true
  //     case (UniversalRoleRestriction(r1, f1), UniversalRoleRestriction(r2, f2)) if r1==r2 => baseFillersFor(f2).subsetOf(baseFillersFor(f1))
  //     // <-- Attention: subset-relation works in the other direction here!
  //     case _ => false
  //   })

  // def baseFillersFor(definer: Concept): Set[Concept] = 
  //   fillerDefinitions.getOrElse(definer, Set(definer))

  def interestingDefiner(definer: BaseConcept, clauses: Iterable[ConceptClause]) = {
    logger.trace(s"Check whether this is interesting: ${definer}")
    if(everythingInteresting){
      logger.trace(s"every definer is interesting")
      true
    }
    // if we are forgetting roles, we have positive==null
    else if(!isDefined(definer, clauses) || (positive!=null && !positive(definer) && !negative(definer))){
      logger.trace(s"the definer ${definer} is not interesting")
      false
    }
    else {
      logger.trace(s"the definer ${definer} is indeed interesting")
      true
    }

    // // If the definer is not a base definer, role propagation only leads to new inferences on the
    // // current symbol if it is built from at least one mixed definer

    // def mixed(definer: BaseConcept) = positive(definer) && negative(definer)

    // !fillerDefinitions.contains(definer) || 
    // fillerDefinitions(definer).size==1 //||
    // //fillerDefinitions(definer).map(_.asInstanceOf[BaseConcept]).exists(mixed)
    // // <-- possibly unsound
  }

  def interestingFillerPair(definer1: BaseConcept,
                            definer2: BaseConcept,
                            clauses: Iterable[ConceptClause]) = {
    if(definer1==definer2)
      false
    // else 
    //   true
    // not sure whether rest is sound with inverse role propagation
    else if(positiveDist==null)
      true // possibly role forgetting
    else if(positiveDist.contains(definer1)
      && negativeDist.contains(definer2)
      && positiveDist(definer1).exists(negativeDist(definer2))){
      logger.trace("yep, definer is interesting")
      true
    }
    else if(positiveDist.contains(definer2)
      && negativeDist.contains(definer1)
      && positiveDist(definer2).exists(negativeDist(definer1))){
      logger.trace("yep, definer is interesting")
      true
    }
    else if(cyclic(definer1) || cyclic(definer2)) {
      if(positiveDist.contains(definer1) && negativeDist.contains(definer2))
        true
      else if(negativeDist.contains(definer1) && positiveDist.contains(definer2))
        true
      else
        false
    }
    else
      false
  }
  // else if(!((positive(definer1) && negative(definer2)) ||
  // 	 (negative(definer1) && positive(definer2))))
  //   false
  // else if(!isDefined(definer1, clauses) || !isDefined(definer2, clauses))
  //   false
  // else if(!onlyInterestingCombinations)
  //   definer1!=definer2
  // else if(reachable.contains(definer1.asInstanceOf[BaseConcept])
  // 	    && reachable.contains(definer2.asInstanceOf[BaseConcept])){
  //   // // even if all definer clauses are saturated immediately and preferred when doing resolutions
  //   // // the following step is not valid (only apply role propagation if the definer sets share a
  //   // // non base symbol), because role propagation might be neccessary on the resulting definer set
  //   // val contents1 = getNonBaseContent(definer1, clauses)
  //   // val contents2 = getNonBaseContent(definer2, clauses)

  //   // //!contents1.isEmpty && !contents2.isEmpty
  //   // contents1.exists{ case ConceptLiteral(p1, c1) => contents2.exists{ case ConceptLiteral(p2, c2) => p1!=p2 && c1==c2 }}

  //   // sound version

  //   val contents1 = reachable(definer1.asInstanceOf[BaseConcept])
  //   val contents2 = reachable(definer2.asInstanceOf[BaseConcept])

  //   !contents1.isEmpty && !contents2.isEmpty //&& !contents1.intersect(contents2).isEmpty

  //   }
  // else
  //   true
  //  }

  def isDefined(definer: Concept, clauses: Iterable[ConceptClause]): Boolean = {
    clauses.exists(clause => clause.literals.exists(l => (!l.polarity && l.concept==definer)))
  }

  // def getNonBaseContent(definer: Concept, clauses: Iterable[ConceptClause]): Iterable[ConceptLiteral] = {  

  //   // direct non base content
  //   def inner(definer: Concept) = 
  //     clauses.flatMap{ c => 
  // 	if(c.literals.contains(ConceptLiteral(false, definer))) { 
  // 	  c.literals.find(isNonBase)
  // 	}
  // 	else
  //         None
  //   }

  //   baseFillersFor(definer).flatMap(inner)
  // }

  def isNonBase(literal: ConceptLiteral): Boolean = literal match {
    case ConceptLiteral(_, BaseConcept(n)) if forgettables.contains(n) => true
    case _ => false
  }


  def getCorrespondingRoleRestrictions(role: Role, clause: ConceptClause): Set[(RoleRestriction, BaseConcept)] =
    clause.literals.collect {
      case ConceptLiteral(true, UniversalRoleRestriction(r, filler: BaseConcept))
        if roleHierarchy.subsumedBy(r, role) =>
        (UniversalRoleRestriction(r, filler), filler)
      case ConceptLiteral(true, ExistentialRoleRestriction(r, filler: BaseConcept))
        if roleHierarchy.subsumedBy(r, role) =>
        (ExistentialRoleRestriction(r, filler), filler)
    }

  def getUniversalRoleRestrictions(clause: ConceptClause) =
    clause.literals.collect { case ConceptLiteral(true, r: UniversalRoleRestriction) => r }

  def getRoleRestrictions(clause: ConceptClause): Set[RoleRestriction] =
    clause.literals.collect {
      case ConceptLiteral(true, r: UniversalRoleRestriction) => r
      case ConceptLiteral(true, r: ExistentialRoleRestriction) => r
    }


  def newRestriction(restriction1: RoleRestriction, restriction2: RoleRestriction, filler: Concept): Concept =
    (restriction1,restriction2) match {
      case (ExistentialRoleRestriction(r, _),_) => ExistentialRoleRestriction(r, filler)
      case (_, ExistentialRoleRestriction(r, _)) => ExistentialRoleRestriction(r, filler)
      case (UniversalRoleRestriction(r, _),_) => UniversalRoleRestriction(r, filler)
      case other => assert(false); null // should not be possible
  }

  // def safeMappingsToFile(filename: String) =
  //   FileTools.writeMap(Map("fillerDefinitions" -> fillerDefinitions, "fillerCombinations" -> fillerCombinations),
  // 		       filename)

  /**
   * Computes the distances of definers to positive and negative literals of the current symbol
   */
  def computeDistances(clauses: Set[ConceptClause],
                       symbol: String) = {

    positiveDist = Map()
    negativeDist = Map()
    cyclic = Set()

    val(posLit, negLit) =
      (ConceptLiteral(true, BaseConcept(symbol)),
        ConceptLiteral(false, BaseConcept(symbol)))

    def addPos(definer: BaseConcept, dist: Int) = {
      logger.trace("add "+dist.toString+" to pos "+definer.toString)

      if(!positiveDist.contains(definer))
        positiveDist = positiveDist.updated(definer, Set())

      positiveDist = positiveDist.updated(definer, positiveDist(definer)+dist)
    }
    def addNeg(definer: BaseConcept, dist: Int) = {
      logger.trace("add "+dist.toString+" to neg "+definer.toString)
      if(!negativeDist.contains(definer))
        negativeDist = negativeDist.updated(definer, Set())

      negativeDist = negativeDist.updated(definer, negativeDist(definer)+dist)
    }

    def descendants(definer: BaseConcept): Set[BaseConcept] = {
      val defLit = new ConceptLiteral(false, definer)
      val defining = clauses.filter(_.literals.contains(defLit))
      defining.flatMap(clause => clause.literals.collect{ literal =>
        literal.concept match {
          case UniversalRoleRestriction(_, b: BaseConcept) => b
          case ExistentialRoleRestriction(_, b: BaseConcept) => b
        }})

    }

    var processedCyclic = Set[BaseConcept]()

    def updateCyclic(definer: BaseConcept, processed: Set[BaseConcept]): Unit = {
      logger.trace("checking cyclicity of "+definer)

      if(processed(definer)){
        logger.trace("is cyclic")
        cyclic += definer
        processedCyclic += definer
      }
      else if(processedCyclic(definer))
        return
      else {
        logger.trace("descending")
        processedCyclic += definer
        val nexts = descendants(definer)
        if(nexts.exists(cyclic))
          cyclic += definer
        else {
          nexts.foreach{ next =>
            updateCyclic(next, processed+definer)
          }
          if(nexts.exists(cyclic))
            cyclic += definer
        }
        // if(!cyclic(definer))
        //   nonCyclic += definer
      }

    }

    var distancesProcessed = Set[BaseConcept]() // known inputs

    def distances(definer: BaseConcept,
                  processed: Set[BaseConcept]): Unit = {
      logger.trace("getting distances from "+definer.toString)

      if(distancesProcessed(definer)||cyclic(definer))
        return

      distancesProcessed += definer

      if(!processed(definer)) {
        val defLit = new ConceptLiteral(false, definer)
        val defining = clauses.filter(_.literals.contains(defLit))
        if(defining.exists(_.literals(posLit)))
          addPos(definer, 0)
        if(defining.exists(_.literals(negLit)))
          addNeg(definer, 0)

        descendants(definer).foreach{ d =>
          if(!processed(d) && !(cyclic(d) && (positiveDist.contains(d) || negativeDist.contains(d)))){
            distances(d, processed+definer)
          }
          if(positiveDist.contains(d))
            positiveDist(d).map(_+1).foreach(addPos(definer, _))
          if(negativeDist.contains(d))
            negativeDist(d).map(_+1).foreach(addNeg(definer, _))
        }
      }
    }

    // definer of negative definer literal of clause, if existent
    def definer(clause: ConceptClause): Option[BaseConcept] = clause.literals.collectFirst {
      case ConceptLiteral(false, b: BaseConcept) if ALCFormulaPreparations.isDefiner(b) => b
    }

    val definers = clauses.collect{ definer(_) match {
      case Some(definer) => definer
    }}

    definers.foreach(updateCyclic(_, Set()))
    definers.foreach(distances(_, Set()))

    logger.debug("pos: "+positiveDist.toString)
    logger.debug("neg: "+negativeDist.toString)
    logger.debug("cyclic: "+cyclic.toString)
  }

  /**
   * Updates which definer is connected to a positive occurrence of the current symbol, and which is
   * connected to a negative occurrence
   */
  def updateRoleConnections(clauses: Set[ConceptClause], symbol: String) = {

    logger.info("updating role connections")

    positive = Set()
    negative = Set()

    val positiveA = new ConceptLiteral(true, BaseConcept(symbol))
    val negativeA = new ConceptLiteral(false, BaseConcept(symbol))

    // definer of negative definer literal of clause, if existent
    def definer(clause: ConceptClause): Option[BaseConcept] = clause.literals.collectFirst {
      case ConceptLiteral(false, b: BaseConcept) if ALCFormulaPreparations.isDefiner(b) => b
    }

    val definers = clauses.collect{ definer(_) match {
      case Some(definer) => definer
    }}

    clauses.foreach{ clause =>
      definer(clause) match {
        case Some(definer) =>
          if(clause.literals.contains(positiveA))
            positive += definer
          if(clause.literals.contains(negativeA))
            negative += definer
        case None => ;
      }
    }

    /**
     * Checks whether concept1 is "connected" to concept2, meaning there is a sequence of clauses
     * ¬concept1 u .. u Qr.D1, ¬D1 u .. u Qr.D2, ... , ¬Dn u .. u (¬)concept2,
     */
    var connections:Set[(BaseConcept, BaseConcept)] = Set()
    var notConnected:Set[(BaseConcept, BaseConcept)] = Set()
    def connected(concept1: BaseConcept, concept2: BaseConcept, clauses: Set[ConceptClause]): Boolean = {
      logger.debug("checking connection between "+concept1.toString+" and "+concept2.toString+" "+clauses.size.toString)
      if(connections((concept1, concept2)))
        return true
      if(notConnected((concept1, concept2)))
        return false
      val l1 = ConceptLiteral(false, concept1)
      val l2a = ConceptLiteral(false, concept2)
      val l2b = ConceptLiteral(true, concept2)
      if( clauses.exists{ clause =>
        clause.literals.contains(l1) && (
          clause.literals.contains(l2a) || clause.literals.contains(l2b) || clause.literals.exists{ _ match{
            case ConceptLiteral(true, UniversalRoleRestriction(r, c2: BaseConcept)) => {
              connections += ((concept1, c2))
              connected(c2, concept2, clauses-clause)
            }
            case ConceptLiteral(true, ExistentialRoleRestriction(r, c2: BaseConcept)) =>
              connections += ((concept1, c2))
              connected(c2, concept2, clauses-clause)
            case _ => false
          }

          }
          )
      }
      ){
        connections += ((concept1, concept2))
        true
      } else {
        notConnected += ((concept1, concept2))
        false
      }
    }


    positive ++= definers.filter(
      a => positive.exists(connected(a, _, clauses)))
    negative ++= definers.filter(
      a => negative.exists(connected(a, _, clauses)))


    logger.info("Positively connected: "+positive.mkString(", "))
    logger.info("Negatively connected: "+negative.mkString(", "))
  }


}

object ClauseFiltering {
  def clausesUsingConcepts(clauses: Iterable[ConceptClause], concepts: Set[Concept]): Iterable[ConceptClause] =
    clauses.filter{ cl => cl.literals.exists{ l => concepts.contains(l.concept) } }
}

object ExtendedPurificationRule {
  //  implicit val (logger, formatter, appender) = ZeroLoggerFactory.newLogger(this)
  //  import formatter._

  val logger = Logger(ExtendedPurificationRule.getClass)

  var purified: Set[Concept] = _

  def purifiableClauses(clauses: Iterable[ConceptClause], symbols: Set[String]): Iterable[ConceptClause] = {
    val purifiable = purifiableSymbols(clauses, symbols)
    purified = purified ++ purifiable
    if(!purifiable.isEmpty) {
      logger.debug("Extended purification is applicable for the following symbols: " + purifiable.toString)
      ClauseFiltering.clausesUsingConcepts(clauses, purifiable)
    } else
      Set.empty[ConceptClause]
  }

  def purifiableSymbols(clauses: Iterable[ConceptClause], symbols: Set[String]): Set[Concept] = {
    (symbols.map(BaseConcept(_)).toSet[Concept]--purified).filterNot{ concept =>
      (clauses.exists{ cl => cl.literals.exists{ l => l.polarity && l.concept==concept } }
        && clauses.exists{ cl => cl.literals.exists{ l => !l.polarity && l.concept==concept } })
    }
  }


}

abstract class LiteralBasedRule extends Rule {

  //  implicit val (logger, formatter, appender) = ZeroLoggerFactory.newLogger(SHQRuleLogger)
  //  import formatter._

  val logger = Logger[LiteralBasedRule]

  def combineLiterals(literal1: ConceptLiteral, literal2: ConceptLiteral)
  : Set[Set[ConceptLiteral]]

  override def getDerivations(clause: ConceptClause, clauses: Iterable[ConceptClause])
  : Set[Derivation] = {
    clauses.flatMap(getDerivations(clause, _)).toSet
  }

  def getDerivations(clause1: ConceptClause, clause2: ConceptClause)
  : Set[Derivation] =
    clause1.literals.flatMap(getDerivations(clause1, clause2, _))

  def getDerivations(clause1: ConceptClause, clause2: ConceptClause, literal1: ConceptLiteral)
  : Set[Derivation] = {
    clause2.literals.map{ l2 =>
      Derivation(premisses = Set(clause1, clause2),
        conclusions = derive(clause1, literal1, clause2, l2), rule = this)
    }
  }

  def getDerivations(clause1: ConceptClause, clauses: Set[ConceptClause], literal1: ConceptLiteral)
  : Set[Derivation] = {
    clauses.flatMap(getDerivations(clause1, _, literal1))
  }

  def derive(clause1: ConceptClause, literal1: ConceptLiteral,
             clause2: ConceptClause, literal2: ConceptLiteral)
  : Set[ConceptClause] = {
    logger.trace(s"check: ${clause1}, ${literal1}, ${clause2}, ${literal2}")
    //    val rest = ((clause1.literals++clause2.literals)-literal1)-literal2 <-- would remove duplicates, which is unsound!
    val rest = (clause1.literals-literal1)++(clause2.literals-literal2)

    logger.trace(s"${literal1} + ${literal2} = ${combineLiterals(literal1,literal2)}")

    combineLiterals(literal1, literal2)
      .filterNot{ l=> l.contains(literal1) || l.contains(literal2) }
      .map{ literals =>
      new ConceptClause(rest++literals, clause1.ordering)
    }

  }
}


// object CutUnusedFillersRule { 
//   implicit val (logger, formatter, appender) = ZeroLoggerFactory.newLogger(this)

//   var rolePropagationRule: RolePropagationRule = _ // for reference of base fillers and combinations

//   /**
//    * returns all clauses that contain negative literals of newly introduced
//    * base fillers that are not used elsewhere
//    */
//   def cutUnusedFillers(clauses: Iterable[ConceptClause]): Iterable[ConceptClause] = { 
//     var unused = getUnusedFillers(clauses)

//     if(unused.isEmpty)
//       Set()
//     else { 
//       logger.trace("The following fillers are not used anymore: " + unused.toString)
//       ClauseFiltering.clausesUsingConcepts(clauses, unused)
//     }
//   }

//   def getUnusedFillers(clauses: Iterable[ConceptClause]): Set[Concept] = { 
//     var baseFillers: Set[Concept] = rolePropagationRule.baseFillers
//     var used: Set[Concept] = Set()

//     import scala.util.control._

//     val loop = new Breaks

//     loop.breakable { 
//       for(clause <- clauses;
// 	  literal <- clause.literals
// 	) { 
// 	val concept = literal match { 
// 	  case ConceptLiteral(true, concept: BaseConcept) => concept
// 	  case ConceptLiteral(true, UniversalRoleRestriction(_, concept)) => concept
// 	  case ConceptLiteral(true, ExistentialRoleRestriction(_, concept)) => concept
// 	  case ConceptLiteral(false, _) => TopConcept
// 	}
// 	if(baseFillers(concept))
// 	  used += concept
// 	if(baseFillers.size==used.size)
// 	  loop.break
//       }
//     }

//     baseFillers -- used
//   }

// }
