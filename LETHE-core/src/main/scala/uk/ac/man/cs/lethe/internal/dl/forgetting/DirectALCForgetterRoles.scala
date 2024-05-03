package uk.ac.man.cs.lethe.internal.dl.forgetting


import java.util.Date
import uk.ac.man.cs.lethe.internal.dl.{AbstractMappedReasoner}
import uk.ac.man.cs.lethe.internal.tools.Timeoutable

import scala.collection.immutable.{Set, SortedSet, TreeSet}
import scala.collection.mutable
import scala.concurrent.TimeoutException

//import com.dongxiguo.fastring.Fastring.Implicits._
import com.typesafe.scalalogging.Logger

import uk.ac.man.cs.lethe.internal.dl.datatypes._
import uk.ac.man.cs.lethe.internal.dl.forgetting.direct._
import uk.ac.man.cs.lethe.internal.forgetting.Forgetter

//import uk.ac.man.cs.lethe.internal.tools.FileTools




// forgetting single roles from clause sets
// does not extend forgetter, since it doesn't work with sets of forgettables
object DirectALCForgetterRoles extends Timeoutable {
  import Configuration._
  import Configuration.Method._

  //implicit val (logger, formatter, appender) = ZeroLoggerFactory.newLogger(this)
  //import formatter._

  val logger = Logger(DirectALCForgetterRoles.getClass)

  // used for module computation
  var trackRoleAxioms = false

  def setTrackRoleAxioms(value: Boolean) =
    trackRoleAxioms = value

  var counter = 0

  def steps = counter

  var nonBaseSymbols: Set[String] = _

  var clauses: mutable.TreeSet[ConceptClause] = _
  var old: mutable.TreeSet[ConceptClause] = _
  var all: mutable.TreeSet[ConceptClause] = _
  var result: mutable.TreeSet[ConceptClause] = _

  var removed: Iterable[ConceptClause] = _

  // set optimizing application of resolution rules
  var prefixSortedClauses: mutable.TreeSet[ConceptClause] = _

  // data structure optimizing subsumption checking
  var subsumptionTree: SubsumptionTree = _

  var definerFactory: DefinerFactory = _

  var resolutionRule: ResolutionRule = _
  var rolePropagationRule: RolePropagationRule = _
  //  var inverseRolePropagationRule: InverseRolePropagationRule = _
  var roleResolutionRule: RoleResolutionRule = _

  var subsumptionChecker: SubsumptionChecker = _
  var mapBasedSubsumptionChecker: SubsumptionCheckerWithMap = _

  var mappedReasoner: AbstractMappedReasoner = _

  var ordering: ConceptLiteralOrdering = _

  var subsumptionTime = 0L
  var addingTime = 0L
  var filteringTime = 0L
  var pureAddingTime = 0L

  var structuralTransformer: StructuralTransformer = _

  private var roleHierarchy: RoleHierarchy = _

  def clean() = {
    clauses = null
    old = null
    all = null
    result = null
    removed = null
    prefixSortedClauses = null
    definerFactory = null
    rolePropagationRule = null
    subsumptionChecker = null
    mapBasedSubsumptionChecker = null
    mappedReasoner = null
    structuralTransformer = null
    roleHierarchy = null
    ALCFormulaPreparations.initDefinitions()
  }

  var onlyPropagate=false

  def setRoleHierarchy(roleHierarchy: RoleHierarchy) =
    this.roleHierarchy=roleHierarchy


  def forget(_clauses: Set[ConceptClause],
             symbol: String,
             justPropagate: Boolean = false): Set[ConceptClause] = {

    startTiming

    logger.debug(s"forgetting ${symbol} in clause set ${_clauses.mkString("\n")}")


    this.onlyPropagate=justPropagate

    nonBaseSymbols = Set(symbol)

    ordering = new ConceptLiteralOrdering(Seq(symbol))

    definerFactory = new DefinerFactory(ALCFormulaPreparations, ordering, roleHierarchy)
    resolutionRule = new ResolutionRule(ordering)
    rolePropagationRule = new RolePropagationRule(
      ordering,
      nonBaseSymbols,
      definerFactory,
      roleHierarchy,
      trackRoleAxioms=trackRoleAxioms)

    if(!justPropagate){
      roleResolutionRule = new RoleResolutionRule(ordering,
        BaseRole(symbol),
        definerFactory,
        mappedReasoner)
    }

    structuralTransformer = new StructuralTransformer(nonBaseSymbols)



    val subsChecker = new SubsumptionCheckerWithMap(Map()) with DefinerSubsumptionChecker with RoleSubsumptionChecker
    subsChecker.setDefinerFactory(definerFactory)
    subsChecker.setRoleHierarchy(roleHierarchy)
    mapBasedSubsumptionChecker = subsChecker
    subsumptionChecker = mapBasedSubsumptionChecker// new CombinedSubsumptionChecker(Set(mapBasedSubsumptionChecker,
    //					  new DefinerSubsumptionChecker(definerFactory)))


    var (withNB, withoutNB) = sortClauses(_clauses) // also sets interestingDefiners


    clauses = mutable.TreeSet()(new ConceptClauseOrdering(ordering))
    clauses ++= structuralTransformer.transform(withNB.toSet, ordering, interestingDefiners)

    removed = withoutNB

    logger.trace("Processing clauses: \n" + clauses.mkString("\n"))


    old = mutable.TreeSet()(new ConceptClauseOrdering(ordering))

    // note: we need the removed clauses in all since this set is used for resolution on definers.
    all = mutable.TreeSet()(new ConceptClauseOrdering(ordering))++clauses ++ removed

    if(method==METHOD2)
      prefixSortedClauses = mutable.TreeSet()(PrefixConceptClauseOrdering) ++ clauses

    if(useSubsumptionTree) {
      subsumptionTree = new SubsumptionTree()
      clauses.foreach{ subsumptionTree.add(_) }
    }

    result = mutable.TreeSet()(new ConceptClauseOrdering(ordering)) ++ withoutNB


    logger.info("Resolving...")
    val startResolving = new Date().getTime()

    resolve()



    if(!clauses.isEmpty) {
      println("Execution has not been completed.")
    }
    logger.info("Time used for calculus: "+ (new Date().getTime()-startResolving).toString)
    logger.info("Time used for resolution: " + resolutionRule.timeUsed.toString)
    logger.info("Time used for rule-propagation: " + rolePropagationRule.timeUsed.toString)
    logger.info("Time used for subsumption checking: "+subsumptionTime.toString)
    logger.info("Time used for adding clauses: "+addingTime.toString)
    logger.info("Time used for filtering out clauses: "+filteringTime.toString)
    logger.info("Time used for adding to sorted clause sets: "+pureAddingTime.toString)




    logger.trace("result clauses: ")
    logger.trace(result.mkString("\n"))
    logger.trace("<----------- result of innerForget")

    result.toSet
  }


  def translateBack(result: Iterable[ConceptClause], base: Set[DLStatement]) = {

    val resultingOntology = new Ontology()

    try {
      val startTranslating = new Date().getTime()


      val backTranslated = SimpleDefinerEliminator.eliminateDefiners(result)

      (base++backTranslated).foreach(resultingOntology.addStatement)


      //       logger.info("Time used for back translation: " + (new Date().getTime()-startTranslating).toString)
      logger.info("Forgetting result:")
      //      logger.info(resultingOntology.toString)

      resultingOntology
    }catch {
      case LoopException(m) =>
        logger.info("Definitions form a loop: can't find the uniform interpolant.")
        new Ontology()
      case e: Throwable => throw e
    }
  }

  def sortClauses(clauses: Set[ConceptClause]): (Iterable[ConceptClause], Iterable[ConceptClause]) = {
    var withNB = clauses.filter( clause => clause.signature.exists(nonBaseSymbols.contains(_)) )

    var hull = definerHull(withNB, clauses)

    (hull, clauses -- hull)
  }

  var interestingDefiners: Set[String] = _

  /** Computes the "transitive hull of definers" - the set of clauses
   * such that for every definer D that occurs in the result all clauses containing D as filler
   * are in the result as well
   */
  def definerHull(clauses: Set[ConceptClause], reference: Set[ConceptClause]): Set[ConceptClause] = {
    var processed = Set[String]()
    var toProcess = Set[String]()
    var result = clauses

    do {
      toProcess = result.flatMap(cl => cl.literals.collect(l => l match {
        case ConceptLiteral(false, BaseConcept(d)) if defined(d) => d
      })) -- processed

      logger.trace("Processing: "+toProcess.mkString("[", ", ", "]"))

      result ++= reference.filter( clause => clause.literals.exists(_.concept match {
        case UniversalRoleRestriction(_, BaseConcept(d)) => toProcess.contains(d)
        case ExistentialRoleRestriction(_, BaseConcept(d)) => toProcess.contains(d)
        case _ => false
      }))

      processed ++= toProcess
    } while(!toProcess.isEmpty)

    interestingDefiners = processed

    result
  }


  def defined(string: String) = string.startsWith("_D")

  case class LoopException(m: String) extends Exception(m)

 /* def propagateRoles() = {
    while(!clauses.isEmpty){
      val next = clauses.head
      clauses -= next

      logger.trace("Current clause: " + next.toString)

      var derivations = Set[Derivation]()
      derivations ++= rolePropagationRule.getDerivations(next, clauses)
      //      derivations ++= inverseRolePropagationRule.getDerivations(next, clauses)

      derivations.flatMap(_.conclusions).foreach{ x =>
        logger.trace("+ New clause: " + x.toString)
        if(!tautology(x) && !subsumed(x)){
          clauses+=x
          all+=x
        }
        else
          logger.trace("- tautology / subsumed")

      }
      cycleCheck(false)
      old += next
    }
  }*/

  def resolve() = {
    ExtendedPurificationRule.purified=Set()
    //    CutUnusedFillersRule.rolePropagationRule=rolePropagationRule

    while(!clauses.isEmpty && !isCanceled){

      val next = clauses.head

      val definers = next.literals.filter(l => ALCFormulaPreparations.isDefiner(l.concept))

      if(method==METHOD2)
        prefixSortedClauses -= clauses.head

      def isCandidate(cl: ConceptClause): Boolean = {
        // the clause is a candidate for inferences if
        // 1. it contains a universal role restriction with the role to be forgotten, and
        // 2. it contains no definer or the same definer as the current clause
        var result = false
        cl.literals.foreach{ _.concept match {
          case UniversalRoleRestriction(r, _) if(r.signature.exists(nonBaseSymbols)) => result = true
          case d: BaseConcept if ALCFormulaPreparations.isDefiner(d) => {
            if(definers.size>0 && definers.head.concept!=d)
              return false // definer compatible with current clause?
          }
          case _ => ;
        }}
        result
      }


      logger.trace("Current clause: " + next.toString)
      if(tautology(next))
        logger.trace("- Tautology")
      else if(subsumedWithout(next))
        logger.trace("- Subsumed")
      else {

        val relevantExistentialExists = next.literals.exists(_.concept match{
          case ExistentialRoleRestriction(r, _) => r.signature.exists(nonBaseSymbols)
          case _ => false
        })

        if(!relevantExistentialExists && valid(next)){
          if(addToResult(next) && !onlyPropagate){
            logger.trace(".. checking for role propagations")
            var derivations = Set[Derivation]()
            derivations ++= rolePropagationRule.getDerivations(next, all)
            //	    derivations ++= inverseRolePropagationRule.getDerivations(next, all)
            derivations.foreach(addConclusions)
            cycleCheck(false)
          }
        } else if(valid(next) && addToResult(next)) { // es gibt eine existential role restriction
          if(onlyPropagate){
            logger.trace(".. checking for role propagations")
            var derivations = Set[Derivation]()
            derivations ++= rolePropagationRule.getDerivations(next, all)
            //	    derivations ++= inverseRolePropagationRule.getDerivations(next, all)
            logger.trace(s"derivations are: ${derivations}")
            derivations.foreach(addConclusions)
            cycleCheck(false)
/*          } else if(method==METHOD2 && !ALCFormulaPreparations.isDefiner(next.literals.head.concept)){
            logger.trace("selecting appropriate clauses")

            roleResolutionRule.getDerivations(next, candidates).foreach(addConclusions)

          } else if(ALCFormulaPreparations.isDefiner(next.literals.head.concept)) {
            // <-- if new clause with definer was resolved, resolutions should possibly be rechecked
            logger.trace("definer, rechecking old clauses")
            resolutionRule.getDerivations(next, candidates).foreach(addConclusions)
          } else if(next.literals.exists(l => ALCFormulaPreparations.isDefiner(l.concept))) {
            // <-- if new clause with neg definer was resolved, resolutions should possibly be rechecked
            logger.trace("definer, rechecking old clauses")
            roleResolutionRule.getDerivations(next, candidates).foreach(addConclusions)
            <--- 2019-09-12: TODO something strange: previous cases seem to be unneccessary. check what's going on here.
 */
          } else {
            val candidates =
              (clauses++result).filter(isCandidate)

            transferCancelInformation(roleResolutionRule)

            logger.trace(".. checking for role resolutions")
            roleResolutionRule.getDerivations(next, candidates).foreach(addConclusions)
          }
          // rolePropagationRule.getDerivations(next, all).foreach(x => if(x!=next) addClause(x))
          // inverseRolePropagationRule.getDerivations(next, all).foreach(x => if(x!=next) addClause(x))
          // <-- only needed after resolution took place

          cycleCheck(false)
        }
        old += next
      }
      logger.trace("Number of processed clauses: " + clauses.size.toString )
      clauses -= next

      checkCanceled

    }
  }

  def addToResult(_clause: ConceptClause): Boolean = {

    //if(!onlyPropagate && _clause.roleSymbols.exists(nonBaseSymbols))
    //  return true // we do not add to result, but it is still okay to process this clause

    var res = false
    val time = new Date().getTime()

    var clause = _clause

    if(condensing)
      clause = subsumptionChecker.condenseClause(_clause)

    logger.trace("  adding "+clause.toString+".")

    structuralTransformer.transformBack(clause, ordering).foreach { clause =>
      if(result.exists(subsumes(_, clause))) {
        logger.trace(clause.toString + " is subsumed in result.")
        //false
      }
      else {
        result --= result.filter{ x=>
          if(subsumes(clause, x)) {
            logger.trace("filtered out result clause " + x.toString)
            true
          } else
            false
        }
        result += clause
        logger.trace("+ added " + clause.toString + " to result.")
        res = true
      }
    }
    addingTime += new Date().getTime() - time

    res
  }

  def nextLarger(clause: ConceptClause): ConceptClause = {
    val next = ConceptLiteral(true, ordering.next(clause.literals.head.concept))
    new ConceptClause(Set(next), ordering)
  }


  def addConclusions(derivation: Derivation) =
    (derivation.conclusions.toSet--derivation.premisses).foreach(addClause)

  def addClause(_clause: ConceptClause): Unit = {
    //    logger.trace(_ ++= "condensing " ++= _clause.toString)
    val time = new Date().getTime()
    var clause = _clause

    if(condensing)
      clause = subsumptionChecker.condenseClause(_clause)

    // var invSimpl = inverseSimplificationRule.getDerivations(clause, all)
    // if(!invSimpl.isEmpty){
    //   logger.trace("Inverse simplification!")
    //   logger.trace(s"Brought us: ${invSimpl}")
    //   logger.trace(s"Removing ${clause}")
    //   invSimpl.foreach(addClause)
    //   return
    // }

    var exisDeriv = ExistentialRoleRestrictionEliminationRule2.getDerivations(clause, all)
    exisDeriv ++= ExistentialRoleRestrictionEliminationRule1.getDerivations(clause, all)
    if(!exisDeriv.isEmpty){
      logger.trace("Existential role restriction elimination!")
      logger.trace("Brought us "+exisDeriv)
      exisDeriv.foreach(addConclusions)
      return
    }


    logger.trace("+ New clause: " + clause.toString)
    counter += 1

    /**
     * The following is important, since the mapped reasoner does not know the
     * semantics of introduced definers.
     */
     if(!onlyPropagate && clause.literals.exists(l => ALCFormulaPreparations.isDefiner(l.concept))){
       logger.trace("Adding clause to mapped reasoner")
       mappedReasoner.addClause(clause)
     }

    val startFiltering = new Date().getTime()

    if(tautology(clause)) {
      logger.trace("- tautology")
      return
    }

    if(!useSubsumptionTree) {
      if(subsumed(clause)) {
        logger.trace("- subsumed")
        return
      }
    } else if(useSubsumptionTree) {
      if(subsumptionTree.subsume(clause)){
        logger.trace("- subsumed")
        return
      }
    }


    if(!useSubsumptionTree){
      val subsumedOld= old.filter{
        x=>
          if(subsumes(clause, x)) {
            logger.trace("filtered out old clause " + x.toString)
            true
          } else
            false
      }
      subsumedOld.foreach{ sub => old.remove(sub); all.remove(sub) }

      val subsumedProc = clauses.filter{
        x=>
          if(subsumes(clause, x)) {
            logger.trace("filtered out clause " + x.toString)
            true
          } else
            false
      }

      subsumedProc.foreach{ sub => clauses.remove(sub); all.remove(sub) }

      if(method==METHOD2) {
        prefixSortedClauses --= prefixSortedClauses.filter{
          x=>
            if(subsumes(clause, x)) {
              true
            } else
              false
        }
      }

    } else if(useSubsumptionTree){
      val subsumed = subsumptionTree.subsumedBy(clause)
      logger.trace("subsumed clauses: " + subsumed.toString)
      old --= subsumed
      clauses --= subsumed
      all --= subsumed
      if(method==METHOD2)
        prefixSortedClauses --= subsumed
      if(!subsumed.isEmpty)
        subsumptionTree.remove(clause)
    }

    filteringTime += new Date().getTime()-startFiltering

    //    logger.trace(_ ++= "-> added.")

    val startAdding = new Date().getTime()
    if(method==METHOD2) {
      prefixSortedClauses += clause
    }

    if(useSubsumptionTree)
      subsumptionTree.add(clause)


    if(clause.literals.filter(l => ALCFormulaPreparations.isDefiner(l.concept)).size>1){
      logger.trace("More than one definer => Immediate resolution required!")
      // <-- motivated by proof for inverse role propagation
      resolutionRule.getDerivations(clause, all).foreach(addConclusions)
      // adding not necessary if already fully resolved on positive definer
    }
    else {
      clauses += clause
      all += clause
      pureAddingTime += new Date().getTime()-startAdding
      addingTime += new Date().getTime()-time
    }
  }

  // def propagateRoles = {
  //   var rolesSets: Map[Role, Set[ConceptClause]]
  //   clauses.foreach{ clause => clause.literals.foreach{ }}
  // }

  def valid(cc: ConceptClause): Boolean = {
    !cc.literals.exists(x => x.polarity && ALCFormulaPreparations.isDefiner(x.concept)) &&
      cc.literals.filter(x => ALCFormulaPreparations.isDefiner(x.concept)).size<2
  }


  def tautology(clause: ConceptClause): Boolean = (
    clause.literals.contains(ConceptLiteral(true, TopConcept)) ||
      clause.literals.exists(l => clause.literals.contains(ConceptLiteral(!l.polarity, l.concept)))
    )


  def subsumed(clause: ConceptClause): Boolean =
    all.exists(subsumes(_, clause))

  def subsumedWithout(clause: ConceptClause): Boolean =
    (all-clause).exists(subsumes(_, clause))

  def subsumes(clause1: ConceptClause, clause2: ConceptClause) = {
    var time = new Date().getTime()
    var result = clause1.literals.subsetOf(clause2.literals)
    if(useSubsumptionChecker)
      result = result || subsumptionChecker.subsumes(clause1, clause2)
    //clause1.literals.subsetOf(clause2.literals)

    subsumptionTime += new Date().getTime()-time
    result
  }

  def nonBase(cl: ConceptLiteral) = cl.concept match {
    case BaseConcept(n) if nonBaseSymbols.contains(n) => true
    case _ => false
  }

  var countXY: Int = 0

  def cycleCheck(force: Boolean): Unit = {
    return
    if(!checkCycles)
      return

    if(!force && clauses.size<100)
      return

    // if(countXY!=0) {
    //   countXY = (countXY + 1)%50
    //   return
    // }

    if(!force && counter%20!=0)
      return

    logger.info("Doing it!")


    val definerGraph  = DefinerGraphBuilder.buildDefinerGraph(clauses++result++removed)

    // update reachable nb-symbol map for role propagation rule
    rolePropagationRule.reachable =
      definerGraph.updateReachableNonBaseSymbols(nonBaseSymbols, rolePropagationRule.reachable)

    // simplify clause set
    val updated = CycleRemover.removeCycles(definerGraph, nonBaseSymbols)
    clauses = clauses.filter(updated(_))
    //old = old.filter(updated(_))
    result = result.filter(updated(_))

    // update redundancy
    var newSubsumptions =
      CycleRemover.findNewSubsumptions(definerGraph, subsumptionChecker)

    if(!newSubsumptions.isEmpty){
      logger.debug("New subsumptions: " + newSubsumptions.mkString("; "))
      mapBasedSubsumptionChecker.addSubsumptions(newSubsumptions)
    }
  }

  def recheckRedundancies(clauses: mutable.Set[ConceptClause]): Unit =
    clauses.foreach{ cl1 =>
      clauses.foreach{ cl2 =>
        if(cl1!=cl2 && subsumes(cl2, cl1)){
          logger.trace("subsumed "+ cl1.toString)
          clauses.remove(cl1)
        }
      }
    }
}

