package uk.ac.man.cs.lethe.internal.dl.forgetting.direct

import scala.collection.mutable.HashSet
import scala.collection.mutable.{HashMap, MultiMap, Set => MutSet}
import com.typesafe.scalalogging.Logger
import uk.ac.man.cs.lethe.internal.dl.datatypes.extended.{DisjunctiveAssertion, GreatestFixpoint, LeastFixpoint, NegatedRoleAssertion}
import uk.ac.man.cs.lethe.internal.dl.datatypes._
import uk.ac.man.cs.lethe.internal.dl.forgetting.QuickConceptForgetter

/**
 * Eliminate non-cyclic definers, create DL statements
 */


object SimpleDefinerEliminator {
  //implicit val (logger, formatter, appender) =
  //ZeroLoggerFactory.newLogger(this)

  val logger = Logger(SimpleDefinerEliminator.getClass)

  def eliminateDefiners(clauses: Iterable[ConceptClause]): Iterable[DLStatement] = {
    val subs = clauses.map(toSubsumption)
    eliminateDefiners2(subs)
  }

  def eliminateDefiners2(subs: Iterable[Subsumption]): Iterable[DLStatement] = {
    logger.trace("grouping subsumptions...")
    var result = groupSubsumptions(subs).toSet[DLStatement]
    logger.trace("grouped subsumptions.")
    var cyclicDefiners = Set[Concept]()


    var nextOption: Option[Subsumption] = None


    var definerSubs = result.collect{
      case Subsumption(d, c) if isDefiner(d) => Subsumption(d, c)
    }

    result --= definerSubs



    def invertMap[A,B](map: Map[A,B]): Map[B,A] = {
      val seq = map.toSeq
      Map[B,A]() ++ seq.map(a => (a._2,a._1))
    }

    val individualsMap = invertMap(ALCFormulaPreparations.knownIndividuals)


    def isIndividualDefiner(d: BaseConcept) = d.name.startsWith("_DI")

    def splitConjuncts(c: Concept) = c match {
      case ConceptConjunction(cs) => cs
      case c => Set(c)
    }

    while(!definerSubs.isEmpty){
      val next = definerSubs.head
      definerSubs = definerSubs.tail
      next match {

        // individual definer
        case Subsumption(d: BaseConcept, c) if isIndividualDefiner(d) => {
          val individual = individualsMap(d.asInstanceOf[IndividualDefiner])
          splitConjuncts(c).foreach{ _ match {
            case ExistentialRoleRestriction(r,d2: BaseConcept) if isIndividualDefiner(d2) => {
              // reconstruct role restriction
              val individual2 = individualsMap(d2.asInstanceOf[IndividualDefiner])
              result += RoleAssertion(r, individual, individual2)
            }
            case c: Concept => result += ConceptAssertion(c, individual)
          } }
        }

        // cyclic definer
        case Subsumption(d,c) if c.atomicConcepts.contains(d.toString) => {
          logger.trace("Cyclic definer: "+d.toString)
          if(tautologicFixpoint(d,c)){
            result = result.map(expandDefinition(Subsumption(d, TopConcept),_))
            definerSubs = definerSubs.map(expandDefinition(Subsumption(d, TopConcept),_))
          }
          else if(result.exists(_.atomicConcepts(d.toString))
            || definerSubs.exists(_.atomicConcepts(d.toString))){
            result += Subsumption(d,c)
            cyclicDefiners += d
          }
        }

        // "normal" definer
        case definition => {
          result = result.map(expandDefinition(definition, _))
          definerSubs = definerSubs.map(expandDefinition(definition, _))
        }
      }
    }
    // do { 
    //   nextOption =.collectFirst{ _ match { 
    // 	case d: Subsumption if isDefiner(d.subsumer) && !cyclicDefiners(d.subsumer) => d 
    //   }}

    //   logger.trace("Processing axiom "+nextOption.toString)

    //   nextOption match { 
    // 	case Some(Subsumption(d, c)) if c.atomicConcepts.contains(d.toString) => { 
    // 	  logger.trace("Cyclic definer: "+d.toString)
    // 	  if(tautologicFixpoint(d,c)){ 
    // 	    subsumptions -= Subsumption(d, c)
    // 	    subsumptions = subsumptions.map(expanDefinition(Subsumption(d, TopConcept), _))
    // 	  }
    // 	  else if(!subsumptions.flatMap(_.atomicConcepts).contains(d.toString)){ 
    // 	    // not used, can safely be removed
    // 	    subsumptions -= Subsumption(d, c)
    // 	  }
    // 	  cyclicDefiners += d
    // 	}
    // 	case Some(definition) => { 
    // 	  subsumptions -= definition
    // 	  subsumptions = subsumptions.map(expandDefinition(definition, _))
    // 	}
    // 	case None => ;
    //   }
    // } while(nextOption!=None)

    // remaining non-cyclic definers get replaced by top
    // (if there is no definition for D, it corresponds to D<=TOP)


    result = result.map(definersToTop(_, cyclicDefiners))

    result
  }

  def tautologicFixpoint(definer: Concept, definition: Concept): Boolean = {
    logger.debug("Checking tautological fixpoint for "+Subsumption(definer,definition).toString)
    val concept = expandDefinition(Subsumption(definer, TopConcept), definition)
    val simplified = CheapSimplifier.simplify(concept) //DLHelpers.simplify(concept)
    logger.debug("Simplified: "+simplified.toString)
    simplified==TopConcept
  }

  def definersToTop(subsumption: DLStatement, cyclic: Set[Concept]): DLStatement = subsumption match {
    case Subsumption(c, d) => Subsumption(definersToTop(c, cyclic), definersToTop(d, cyclic))
    case ConceptAssertion(c, i) => ConceptAssertion(definersToTop(c, cyclic), i)
    case ra: RoleAssertion => ra
  }


  def definersToTop(concept: Concept, cyclic: Set[Concept]): Concept = concept match {
    case d: BaseConcept if isDefiner(d) && !cyclic(d) => TopConcept
    case TopConcept => TopConcept
    case BottomConcept => BottomConcept
    case b: BaseConcept => b
    case ConceptComplement(b: BaseConcept) => ConceptComplement(b)
    case ConceptConjunction(cs) => ConceptConjunction(cs.map(definersToTop(_, cyclic)))
    case ConceptDisjunction(ds) => {
      val disjuncts = ds.map(definersToTop(_, cyclic))
   //   if(tautologicalDisjunction(disjuncts))
   //     TopConcept
   //   else
      if(CheapSimplifier.tautologicalDisjunction(disjuncts))
        TopConcept
      else
        ConceptDisjunction(disjuncts)
    }
    case UniversalRoleRestriction(r,f) => UniversalRoleRestriction(r, definersToTop(f, cyclic))
    case ExistentialRoleRestriction(r,f) => ExistentialRoleRestriction(r, definersToTop(f, cyclic))
    case MinNumberRestriction(n,r,f) => MinNumberRestriction(n, r, definersToTop(f, cyclic))
    case MaxNumberRestriction(n,r,ConceptComplement(f)) =>
      MaxNumberRestriction(n, r, ConceptComplement(definersToTop(f, cyclic)))
    case _ => assert(false, "Definition should not be expanded in this expression: "+concept.toString); concept
  }

  def expandDefinition(definition: Subsumption, stat: DLStatement): DLStatement = stat match {
    case Subsumption(c, d) =>
      Subsumption(expandDefinition(definition, c), expandDefinition(definition, d))
    case ConceptAssertion(c, i) =>
      ConceptAssertion(expandDefinition(definition, c), i)
    case DisjunctiveConceptAssertion(cas) =>
      DisjunctiveConceptAssertion(cas.map(expandDefinition(definition, _).asInstanceOf[ConceptAssertion]))
    case DisjunctiveAssertion(da) =>
      DisjunctiveAssertion(da.map(expandDefinition(definition,_).asInstanceOf[Assertion]))
    case ra: RoleAssertion =>
      ra
    case nra: NegatedRoleAssertion =>
      nra
  }

  def expandDefinition(definition: Subsumption, sub: Subsumption): Subsumption = sub match {
    case Subsumption(c, d) =>
      Subsumption(expandDefinition(definition, c), expandDefinition(definition, d))
  }

  def expandDefinition(definition: Subsumption, concept: Concept): Concept = concept match {
    case d: BaseConcept if d==definition.subsumer => definition.subsumee
    case TopConcept => TopConcept
    case BottomConcept => BottomConcept
    case b: BaseConcept => b
    case ConceptComplement(b: BaseConcept) => ConceptComplement(b)
    case ConceptConjunction(cs) => ConceptConjunction(cs.map(expandDefinition(definition, _)))
    case ConceptDisjunction(ds) => ConceptDisjunction(ds.map(expandDefinition(definition, _)))
    case UniversalRoleRestriction(r,f) => UniversalRoleRestriction(r, expandDefinition(definition, f))
    case ExistentialRoleRestriction(r,f) => ExistentialRoleRestriction(r, expandDefinition(definition, f))
    case MinNumberRestriction(n, r, f) => MinNumberRestriction(n, r, expandDefinition(definition, f))
    case MaxNumberRestriction(n, r, ConceptComplement(f)) =>
      MaxNumberRestriction(n, r, ConceptComplement(expandDefinition(definition, f)))
    case GreatestFixpoint(variable, concept) =>
      GreatestFixpoint(variable, expandDefinition(definition, concept))
    case LeastFixpoint(variable, concept) =>
      LeastFixpoint(variable, expandDefinition(definition, concept))
    case ns: NominalSet => ns // nothing to expand
    case ConceptComplement(ns: NominalSet) => concept // nothing to expand
    case _ => assert(false, "Definition should not be expanded in this expression: "+concept.toString); concept
  }

  def groupSubsumptions(subsumptions: Iterable[Subsumption]): Iterable[Subsumption] =
    subsumptions.groupBy(_.subsumer).toSet[(_>:Concept, Iterable[Subsumption])].flatMap { _ match {
      case (x: Concept, y: Set[Subsumption]) if isDefiner(x) =>
        Set(Subsumption(x, ConceptConjunction(y.map(_.subsumee).toSet)))
      case (x, y: Set[Subsumption]) => y.toSet // only group subsumptions if definer subsumer
    }}

  def toSubsumption(clause: ConceptClause): Subsumption = {
    val (subsumer, rest) = getNegDefiner(clause) match {
      // erster ein neg Definer
      case Some(concept) => (concept, clause.literals-ConceptLiteral(false, concept))
      case None => (TopConcept, clause.literals)
      // case None => clause.literals.collectFirst{ case ConceptLiteral(false, c) => c } match {
      //   // erster irgendein negiertes Konzept
      //   case Some(concept) =>
      //     (concept, clause.literals-ConceptLiteral(false, concept))
      //   case None => (TopConcept, clause.literals)
      //     // lassen wir mal lieber erstmal - gefaehrlich wegen der definer elimination spaeter
      //     // clause.literals.collectFirst{ case ConceptLiteral(true, c: BaseConcept) => c} match {
      //     // 	case Some(concept) =>
      //     // 	  (ConceptComplement(concept), clause.literals-ConceptLiteral(true, concept))
      //     // 	case None => (TopConcept, clause.literals)
      //     // }
      //   }
    }

    Subsumption(subsumer, ConceptDisjunction(rest.map(_.convertBack)))
  }

  def getNegDefiner(clause: ConceptClause): Option[BaseConcept] =
    clause.literals.collectFirst{
      case ConceptLiteral(false, c: BaseConcept) if isDefiner(c) => c
    }

  def isDefiner(concept: Concept) = concept match {
    case BaseConcept(n) => n.startsWith("_D")
    case _ => false
  }

}


object DefinerPurification {
  // assumption: clauses in proper form, defining clauses _D1 <= C
  def purifyRemainingDefiners(ontology: Ontology): Ontology = {
    val positive = new HashSet[String]()
    val negative = new HashMap[String, MutSet[DLStatement]]() with MultiMap[String, DLStatement]

    ontology.statements.foreach{ dls => dls match {
      case Subsumption(BaseConcept(name), c) if isDefiner(name) =>
        negative.addBinding(name, dls)
        definers(dls).foreach(positive.add)
      case _ => definers(dls).foreach(positive.add)
    } }

    val negativeKeys = Set() ++ negative.keys

    positive.foreach(negative.remove)
    negativeKeys.foreach(positive.remove)
    negative.values.flatten.foreach{ ontology.remove }
    QuickConceptForgetter.replaceBy(ontology, positive.toSet, TopConcept)
  }

  def definers(dlStatement: DLStatement) = {
    dlStatement.atomicConcepts.filter(isDefiner)
  }

  def isDefiner(conceptName: String) =
    conceptName.startsWith("_D")
}
