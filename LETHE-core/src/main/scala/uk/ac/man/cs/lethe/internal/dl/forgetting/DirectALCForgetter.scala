package uk.ac.man.cs.lethe.internal.dl.forgetting


import java.util.Date
import uk.ac.man.cs.lethe.internal.tools.{CanceledException, Timeoutable}

import scala.concurrent.TimeoutException
import scala.collection.immutable.{Set, SortedSet, TreeSet}
import scala.collection.mutable
import com.typesafe.scalalogging.Logger
import uk.ac.man.cs.lethe.internal.dl.AbstractMappedReasonerFactory
import uk.ac.man.cs.lethe.internal.dl.datatypes._
import uk.ac.man.cs.lethe.internal.dl.forgetting.direct._
import uk.ac.man.cs.lethe.internal.forgetting.{Forgetter, ForgetterWithBackgroundKB}
import uk.ac.man.cs.lethe.internal.tools.{ConsoleProgressBar, ProgressBarAttached}

object Configuration {
  object Method extends Enumeration {
    type Method = Value
    val METHOD1, METHOD2 = Value
  }
  import Method._


  val method = METHOD2

  var useSubsumptionTree: Boolean = false

  var prepareRoleCombinations: Boolean = false

  var checkCycles: Boolean = false

  var optimiseRolePropagation: Boolean = false

  var condensing: Boolean = true

  var useSubsumptionChecker: Boolean = false

  var maxOntologySize = -1//15000000
}


/**
  * Object for forgetting concept symbols from an ALCH ontology
  */
object DirectALCForgetter extends Forgetter[Ontology, String]
  with ForgetterWithBackgroundKB[Ontology, String]
  with ProgressBarAttached
  with Timeoutable{
  import Configuration._
  import Configuration.Method._

  //implicit val (logger, formatter, appender) = ZeroLoggerFactory.newLogger(this)
  //import formatter._


  // used for module computation
  var trackRoleAxioms = false

  def setTrackRoleAxioms(value: Boolean) =
    trackRoleAxioms = value

  val logger = Logger(DirectALCForgetter.getClass)

  // keep definers after forgetting symbols - useful for module extraction
  // causes stack overflow?
  var dontEliminateDefiners = false

  // proportion of symbols that should be eliminated
  // if 1, forget everything given, otherwise, forget
  // only the easiest conpepts until the proportion is reached
  var forgetOnlyEasiest = 1.0

  var inverseRoles = false

  progressBar = new ConsoleProgressBar()

  var undoStructuralTransBeforeAddToResult = true

  var counter = 0

  override def steps = counter

  var abox = false

  var nonBaseSymbols: Set[String] = _

  var clauses: mutable.TreeSet[ConceptClause] = _
  var old: mutable.TreeSet[ConceptClause] = _
  var all: mutable.TreeSet[ConceptClause] = _
  var result: mutable.TreeSet[ConceptClause] = _
  var backgroundClauses: mutable.TreeSet[ConceptClause] = _

  var oldResultClauses: mutable.TreeSet[ConceptClause] = _

  var removed: Iterable[ConceptClause] = _

  // set optimizing application of resolution rules
  var prefixSortedClauses: mutable.TreeSet[ConceptClause] = _

  // data structure optimizing subsumption checking
  var subsumptionTree: SubsumptionTree = _

  var definerFactory: DefinerFactory = _

  var resolutionRule: ResolutionRule = _
  var rolePropagationRule: RolePropagationRule = _

  var subsumptionChecker: SubsumptionChecker = _
  var mapBasedSubsumptionChecker: SubsumptionCheckerWithMap = _

  var ordering: ConceptLiteralOrdering = _

  var subsumptionTime = 0L
  var addingTime = 0L
  var filteringTime = 0L
  var pureAddingTime = 0L

  var structuralTransformer: StructuralTransformer = _

  var roleHierarchy: RoleHierarchy = _

  var largestClauseSet = 0
  var largestResultClauseSet = 0

  def clean() = {
    clauses = null
    oldResultClauses = null
    old = null
    all = null
    result = null
    removed = null
    prefixSortedClauses = null
    definerFactory = null
    rolePropagationRule = null
    subsumptionChecker = null
    mapBasedSubsumptionChecker = null
    structuralTransformer = null
    roleHierarchy = null
    ALCFormulaPreparations.initDefinitions()
  }


  override def forget(_ontology: Ontology, symbols: Set[String]) = {

    val ontology = OntologyFilter.restrictToALCH(_ontology)

    forget(ontology, symbols, new Ontology())
  }

  def forget(ontology: Ontology,
             symbols: Set[String],
             onlyIfSmaller: Boolean): Ontology =
    forget(ontology, symbols, new Ontology(), onlyIfSmaller)

  /**
    * Forgetting with background.
    * Note: the background ontology is assumed to be restricted to ALCH already.
    */
  override def forget(ontology: Ontology, symbols: Set[String], _backgroundKB: Ontology) =
    forget(ontology, symbols, _backgroundKB, false)

  def forget(ontology: Ontology,
             symbols: Set[String],
             _backgroundKB: Ontology,
             onlyIfSmaller: Boolean): Ontology =
  {
    startTiming

    logger.info(s"Ignoring ABox ")
    ontology.abox = ABox(Set())

    //var backgroundKB = OntologyFilter.restrictToALCHI(_backgroundKB)
    var backgroundKB = _backgroundKB // Assume to be done before(!)
    backgroundKB.abox = ABox(Set())
    //    println("Time after restrict to alchi: "+(new Date().getTime-startTime))

    var symbolsAtClauseLevel = !backgroundKB.isEmpty  // process all symbols at clause level?
    // or process symbols on after another
    // at ontology level?

    roleHierarchy = new RoleHierarchy(ontology.rbox)
    //    println("Time after role hierarchy: "+(new Date().getTime-startTime))

    logger.info(s"Forgetting ${symbols.mkString("[",", ","]")} in the following ontology: ")
    logger.info(s"${ontology.toString}")
    logger.info(s"with the following background ontology: ")
    logger.info(s"${backgroundKB.toString}")

    logger.info(s"Preprocessing using QuickForgetter...")

    var currentOnt = ontology
    //    while(currentOnt.signature.exists(symbols)){

    //    println(" Quickforgetter currentOnt... "+new Date())
    currentOnt = QuickConceptForgetter.forget(currentOnt, symbols, backgroundKB)
    //    println("Time after quickforgetting 1: "+(new Date().getTime-startTime))
    // println(" Quickforgetter background..."+new Date())
    // backgroundKB = QuickForgetter.forget(backgroundKB, symbols, currentOnt)//to avoid purified symbols to come back
    // println("Time after quickforgetting 2: "+(new Date().getTime-startTime))
    // Note on quickforgetting on background: can be expensive, might not always be necessary

    logger.info(s"Output of QuickForgetter:")
    logger.info(s"${currentOnt.toString}")
    logger.info(s"Output of QuickForgetter for Background:")
    logger.info(s"${backgroundKB.toString}")


    //    println(" filtering symbols..."+new Date())
    var symbolsToBeProcessed = if(backgroundKB.isEmpty)
      symbols.filter(currentOnt.atomicConcepts++backgroundKB.atomicConcepts)
    else
      symbols // too much overhead?

    logger.info(s"Remaining symbols to be forgotten: ${symbolsToBeProcessed.mkString("[", ", ", "]")}")


    ALCFormulaPreparations.initDefinitions()

    //    println(" ordering symbols..."+new Date())
    var counter = symbolsToBeProcessed.size
    var orderedSymbols =
      SymbolOrderings.orderByNumOfOccurrences(symbolsToBeProcessed, currentOnt).reverse
    //    println("Time after ordering: "+(new Date().getTime-startTime))

    orderedSymbols ++= (symbols--orderedSymbols)

    orderedSymbols = orderedSymbols.take((orderedSymbols.size*forgetOnlyEasiest).toInt)

    progressBar.init(symbols.size, "Concept ")


    //    println("Time used for preprocessing: "+(new Date().getTime-startTime))
    if(symbolsAtClauseLevel){
      currentOnt = _forget(currentOnt, orderedSymbols, backgroundKB)
    } else try {
      for(nextSymbol <- orderedSymbols){
        progressBar.update(symbolsToBeProcessed.size-counter, "Forgetting "+nextSymbol.split("#").last)

        currentOnt = _forget(currentOnt, Some(nextSymbol), backgroundKB, onlyIfSmaller)



        counter -= 1

        if(maxOntologySize!= -1 && currentOnt.size>maxOntologySize)
          throw new Exception("Ontology too huge: "+currentOnt.size)


        logger.debug(s"Remaining symbols to be forgotten: ${counter.toString}")
        logger.debug(s"Ontology size: ${currentOnt.size}")
      }

    } catch {
      case _: TimeoutException =>
        logger.info("timeout occured!")
        logger.info(s"${currentOnt.atomicConcepts.filter(orderedSymbols.toSet).size} symbols where skipped due timeout.")
      case _: CanceledException =>
        logger.info("Execution canceled!")
        logger.info(s"${currentOnt.atomicConcepts.filter(orderedSymbols.toSet).size} symbols where skipped due cancelation.")


      case unknownThrowable: Throwable => throw unknownThrowable
    }

    progressBar.finish()

    assert(onlyIfSmaller || isCanceled || !currentOnt.atomicConcepts.exists(orderedSymbols.toSet), "still there: "+currentOnt.atomicConcepts.filter(orderedSymbols.toSet))

    val definers = currentOnt.atomicConcepts.filter(_.startsWith("_D"))


    if(onlyIfSmaller)
      logger.info(currentOnt.atomicConcepts.filter(orderedSymbols.toSet).size+" symbols where skipped to reduce ontology size.")

    //    println(" Simplify 2(!)..."+new Date())
    //currentOnt = DLHelpers.simplify(currentOnt)
    currentOnt = CheapSimplifier.simplify(currentOnt)

    currentOnt.rbox = ontology.rbox

    // if(currentOnt==null)
    //   println("currentOnt!")
    // else if(currentOnt.atomicConcepts==null)
    //   println("atomic concepts!")
    // else if(currentOnt.atomicConcepts.exists(_==null))
    //   println("exists!")
    // else if(symbols==null)
    //   println("symbols!")


    // <--- some debugging work. not sure whether up to date
    //assert(onlyIfSmaller || !currentOnt.atomicConcepts.exists(c => !c.startsWith("_D") && symbols(c)))


    //    println("Time used overall: "+(new Date().getTime-startTime))

    currentOnt
  }

  def _forget(ontology: Ontology,
              symbols: Iterable[String],
              background: Ontology,
              onlyIfSmaller: Boolean = false): Ontology = {

    nonBaseSymbols = symbols.toSet

    counter = 0

    var ontology2 = ontology

    structuralTransformer = new StructuralTransformer(nonBaseSymbols)

    //ontology2 = structuralTransformer.transform(ontology2)
    // logger.info(s"Result of structural transformation: \n"+ontology2.toString)

    logger.info(s"sort statements")
    //    println(" sort main statements..."+new Date())
    var (base, nonBase, both) =
      DLForgettingPreprocessing.sortStatements(ontology2.statements, nonBaseSymbols)

    val startSort = new Date().getTime
    //    println(" sort background..."+new Date())
    var (baseB, nonBaseB, bothB) =
      DLForgettingPreprocessing.sortStatements(background.statements, nonBaseSymbols)
    //    println("Time sort background "+(new Date().getTime-startSort))

    // logger.info(s"check for purification")
    // val quickie = QuickForgetter.forget(nonBase++both, nonBaseSymbols, nonBaseB++bothB)
    // for(symbol <- nonBaseSymbols){ 
    //   if(!quickie.signature(symbol)){ 
    // 	logger.info(s"purified ${symbol}")
    // 	//return quickie
    //   }
    // }

    // logger.info(s"order symbols")
    // val orderedSymbols = SymbolOrderings.orderByNumOfOccurrences(symbols, ontology2)
    // logger.info(s"Symbols will be processed in this order: "+orderedSymbols.mkString(", "))

    ordering = new ConceptLiteralOrdering(nonBaseSymbols.toSeq)

    definerFactory = new DefinerFactory(ALCFormulaPreparations, ordering, roleHierarchy)

    resolutionRule = new ResolutionRule(ordering)
    rolePropagationRule = new RolePropagationRule(
      ordering,
      nonBaseSymbols,
      definerFactory,
      roleHierarchy,
      trackRoleAxioms = trackRoleAxioms)


    ALCFormulaPreparations.initDefinitions()

    var notAll = false // all axioms processed?


    // Rough heuristics: Having everying in the clauses has advantages 
    // for redundancy elimination, but if the clause set gets to big, 
    // becomes infeasible
    var process =
    if(ontology.size>10000) {
      notAll = true
      nonBase++both
    }
    else
      nonBase++both++base


    //process ++= ontology2.abox.assertions

    //    println(" Clausify "+new Date())
    var clauses =  ALCFormulaPreparations.clauses(process, ordering)
    logger.debug(s"Clauses : ")
    logger.debug(s"${clauses.mkString("\n")}")

    //    println(" Clausify background"+new Date())
    var backgroundClauses = ALCFormulaPreparations.clauses(nonBaseB++bothB, ordering)
    logger.debug(s"Background clauses: ")
    logger.debug(s"${backgroundClauses.mkString("\n")}")


    val numClausesBefore = clauses.size

    logger.trace(s"\nTranslated clauses: \n${clauses.mkString("\n")}")
    old = mutable.TreeSet()(new ConceptClauseOrdering(ordering))


    //    if(!useTimeOut || (new Date()).getTime-started.getTime<timeOut){
    logger.debug(s"")
    //      logger.debug(s"Forgetting "+symbol)
    logger.debug(s"====================================")





    //    println(" inner forget... "+ new Date())
    clauses = innerForget(clauses, symbols, backgroundClauses)

    assert(!clauses.exists(x => x.atomicConcepts.exists(y=> symbols.exists(z => z==y))))

    //    }




    //    println("Number of clauses after forgetting: "+clauses.size)


    //    println(" back translating... "+new Date())
    logger.debug(s"Backtranslating...")

    logger.trace(s"result: \n${clauses.mkString("\n")}")

    def notValid(cl: ConceptClause) = cl.literals.filter(_ match {
      case ConceptLiteral(false, d: BaseConcept) if ALCFormulaPreparations.isDefiner(d) => true
      case _ => false
    }).size>1

    // clauses = clauses.filterNot(notValid)
    // logger.trace(s"result filtered:\n ${clauses.mkString("\n")}")
    var result = translateBack(clauses, backgroundClauses)


    //    logger.debug(s"Result of backtranslation:\n"+result.toString)
    assert(!result.atomicConcepts.exists(symbols.toSet))
    logger.debug(s"Undoing structural transformation...")

    result = structuralTransformer.transformBack(result)
    logger.debug(s"Result of undoing structural transformation: \n${result.toString}")
    assert(!result.atomicConcepts.exists(symbols.toSet), "Still left: "+result.atomicConcepts.filter(symbols.toSet))


    logger.debug(s"Simplifiying...")
    //result = DLHelpers.simplify(result)
    result = CheapSimplifier.simplify(result)

    logger.debug(s"Final result: \n${result.toString}")
    logger.debug(s"<----------------- final result")

    if(notAll)
      base.foreach(result.addStatement)




    assert(!result.atomicConcepts.filter(c => !c.startsWith("_D")).exists(symbols.toSet))

    if(onlyIfSmaller && result.size>=ontology.size)
      return ontology

    //    println(" Done. "+new Date())

    result
  }

  def innerForget(_clauses: Set[ConceptClause],
                  _symbols: Iterable[String],
                  background: Set[ConceptClause]): Set[ConceptClause] = {


    val symbols = _symbols.toSet

    val subsChecker = new SubsumptionCheckerWithMap(Map()) with DefinerSubsumptionChecker with RoleSubsumptionChecker
    subsChecker.setDefinerFactory(definerFactory)
    subsChecker.setRoleHierarchy(roleHierarchy)

    mapBasedSubsumptionChecker = subsChecker
    subsumptionChecker = mapBasedSubsumptionChecker//new CombinedSubsumptionChecker(Set(mapBasedSubsumptionChecker,
    //				  new DefinerSubsumptionChecker(definerFactory)))



    var currentResult = mutable.TreeSet()(new ConceptClauseOrdering(ordering))++_clauses

    oldResultClauses = mutable.TreeSet()(new ConceptClauseOrdering(ordering))
    all = mutable.TreeSet()(new ConceptClauseOrdering(ordering)) // <-- vorsicht! wird nach er schleife verwendet

    var remainingSymbols = symbols.filter(s => currentResult.exists(_.atomicConcepts(s)))
    while(remainingSymbols.size>0){
      logger.debug(s"round with ${remainingSymbols}")

      for(symbol <- remainingSymbols){
        //	println("size background: "+background.size)

        checkCanceled

        if(background.isEmpty)
          structuralTransformer = new StructuralTransformer(Set(symbol))
        //	else
        //	  structuralTransformer = new StructuralTransformer(symbols) // apparently some bug otherwise

        interestingDefiners = Set[String]()
        //	println("sort clauses "+new Date())
        val (withNB, withoutNB) = sortClauses(currentResult.toSet, symbol) // also sets interestingDefiners
        //	println("sort clauses background "+new Date())
        val (bWithNB, bWithoutNB) = sortClauses(background, symbol)

        clauses = mutable.TreeSet()(new ConceptClauseOrdering(ordering))

        //	println("structural transformation "+new Date())
        if(abox)
          clauses ++= withNB ++ withoutNB
        else
          clauses ++= structuralTransformer.transform(withNB.toSet, ordering, interestingDefiners)

        if(inverseRoles)
          clauses.foreach{ _.isResultCandidate = true } // clauses up
        // to now should be added, if they fulfill all requirements
        // (experimental for inverse roles)

        //    println("Interesting definers: "+interestingDefiners.size)

        // Now for the inverse roles
        // (only works since we are in ALCHI and neither have trans. or f. roles)
        if(inverseRoles){
          val inverseRoleProcessor = new InverseRoleProcessor(definerFactory)
          clauses ++= inverseRoleProcessor.process(clauses)
          //	  println("Number of clauses to be processed: "+clauses.size)
        }

        removed = withoutNB

        backgroundClauses = mutable.TreeSet()(new ConceptClauseOrdering(ordering))
        // commented for debuggin: Sun 31 May
        //	if(!abox)
        //	  backgroundClauses ++= structuralTransformer.transform(bWithNB.toSet, ordering, interestingDefiners)
        //	else
        backgroundClauses ++= bWithNB

        if(abox) backgroundClauses ++= bWithoutNB

        //	println("size background clauses "+backgroundClauses.size)

        // if(purifiable(_clauses, symbol)){
        //   logger.debug(symbol+" purifiable!")
        //   return withoutNB.toSet[ConceptClause]
        // }

        old = mutable.TreeSet()(new ConceptClauseOrdering(ordering))
        all ++= clauses ++ backgroundClauses ++ removed ++ bWithoutNB

        //	println("size all "+all.size)

        if(method==METHOD2)
          prefixSortedClauses = mutable.TreeSet()(PrefixConceptClauseOrdering) ++ clauses

        if(useSubsumptionTree) {
          subsumptionTree = new SubsumptionTree()
          clauses.foreach{ subsumptionTree.add(_) }
        }

        logger.trace(s"Processing clauses: \n${clauses.mkString("\n")}")

        result = mutable.TreeSet()(new ConceptClauseOrdering(ordering)) ++ withoutNB

        nonBaseSymbols = Set(symbol)

        rolePropagationRule.updateRoleConnections((withNB++bWithNB).toSet, symbol) // updates connections to current symbol
        rolePropagationRule.computeDistances((withNB++bWithNB).toSet, symbol) // updates connections to current symbol

        logger.info(s"\nResolving on ${symbol}...")
        logger.info(s"--------------------------------\n")
        val startResolving = new Date().getTime()
        subsumptionTime = 0
        addingTime = 0
        //	println("resolving " +new Date())
        resolve()

        //	println("closure under definers... "+new Date())
        result ++= closureUnderDefiners(result.toSet, background).filterNot(oldResultClauses)
        //	println("done "+new Date())

        if(!clauses.isEmpty) {
          println("Execution has not been completed.")
        }
        logger.info(s"Time used for calculus: ${new Date().getTime()-startResolving}")
        logger.info(s"Time used for resolution: ${resolutionRule.timeUsed.toString}")
        logger.info(s"Time used for role-propagation: ${rolePropagationRule.timeUsed.toString}")
        logger.info(s"Time used for subsumption checking: ${subsumptionTime.toString}")
        logger.info(s"Time used for adding clauses: ${addingTime.toString}")
        logger.info(s"Time used for filtering out clauses: ${filteringTime.toString}")
        logger.info(s"Time used for adding to sorted clause sets: ${pureAddingTime.toString}")

        logger.debug(s"result clauses: ")
        logger.debug(s"${result.mkString("\n")}")
        logger.debug(s"<----------- result of innerForget")

        assert(clauses.isEmpty)
        currentResult = mutable.TreeSet()(new ConceptClauseOrdering(ordering))++result

        oldResultClauses ++= result
      }

      // update set in case new symbols came in via background ontology
      remainingSymbols = symbols.filter(s => currentResult.exists(_.atomicConcepts(s)))
    }

    assert(!currentResult.exists(_.atomicConcepts.exists(symbols)))


    if(!undoStructuralTransBeforeAddToResult){
      result = mutable.TreeSet()(new ConceptClauseOrdering(ordering)) ++= currentResult.flatMap(structuralTransformer.transformBack(_,ordering))
    } else
      result = currentResult
    assert(!result.flatMap(_.atomicConcepts).exists(symbols))

    var resultHull = definerHull(result.toSet, all.toSet) // add clauses referring to clauses in the result set
    val resultSet = resultHull.filterNot(cl => cl.atomicConcepts.exists(symbols))

    logger.debug(s"result clauses: ")
    logger.debug(s"${resultSet.mkString("\n")}")
    logger.debug(s"<----------- result of innerForget")


    assert(!resultSet.flatMap(_.atomicConcepts).exists(symbols))
    resultSet
  }

  def closureUnderDefiners(clauses1: Set[ConceptClause], clauses2: Set[ConceptClause]) = {
    val definers = clauses1.flatMap(_.signature).filter(c => ALCFormulaPreparations.isDefiner(BaseConcept(c)))
    clauses2.filter(_.signature.exists(definers))
    // TODO shouldn't this be repeated?
  }

  def purifiable(clauses: Set[ConceptClause], symbol: String): Boolean = {
    val pos = new ConceptLiteral(true, BaseConcept(symbol))
    val neg = new ConceptLiteral(false, BaseConcept(symbol))

    var (foundP, foundN) = (false, false)


    !clauses.exists(_.literals.contains(pos)) || !clauses.exists(_.literals.contains(neg))
    // clauses.foreach{ clause =>
    //   if(!foundP && clause.literals.contains(pos))
    // 	foundP=true
    //   if(!foundN && clause.literals.contains(neg))
    // 	foundN=true
    //   if(foundP && foundN)
    // 	return false
    // }
    // return true
  }

  def filterOutSymbol(clauses: Set[ConceptClause], symbol: String): Set[ConceptClause] = {
    logger.info(s"Purifying ${symbol}!")
    clauses.filterNot(_.atomicConcepts(symbol))
  }

  def translateBack(_result: Iterable[ConceptClause], background: Set[ConceptClause]) = {

    val resultingOntology = new Ontology()

    var result = _result.toSet[ConceptClause]

    try {
      val startTranslating = new Date().getTime()


      val backTranslated =
        if(dontEliminateDefiners){
          _result.map(SimpleDefinerEliminator.toSubsumption)
        }
        else
          SimpleDefinerEliminator.eliminateDefiners(result)

      backTranslated.foreach(resultingOntology.addStatement(_))


      logger.info(s"Time used for back translation: ${(new Date().getTime()-startTranslating).toString}")
      logger.debug(s"Forgetting result:")
      logger.debug(s"${resultingOntology.toString}")

      resultingOntology
    }catch {
      case LoopException(m) =>
        logger.info(s"Definitions form a loop: can't find the uniform interpolant.")
        new Ontology()
      case unknownThrowable: Throwable => throw unknownThrowable
    }
  }

  def sortClauses(clauses: Set[ConceptClause]): (Iterable[ConceptClause], Iterable[ConceptClause]) = {
    var withNB = clauses.filter( clause => clause.atomicConcepts.exists(nonBaseSymbols.contains(_)) )

    var hull = definerHull(withNB, clauses)

    (hull, clauses -- hull)
  }

  def sortClauses(clauses: Set[ConceptClause], symbol: String)
  : (Iterable[ConceptClause], Iterable[ConceptClause]) = {
    var withNB = clauses.filter( clause => clause.atomicConcepts(symbol) )

    var hull = definerHull(withNB, clauses)

    (hull, clauses -- hull)
  }

  var interestingDefiners: Set[String] = Set()

  /** Computes the "transitive hull of definers" - the set of clauses
    * such that for every definer D that occurs in the result all clauses containing D as filler
    * are in the result as well
    * clauses: the clause set to start from
    * reference: clause set to check against
    */
  def definerHull(clauses: Set[ConceptClause], reference: Set[ConceptClause]): Set[ConceptClause] = {
    var processed = Set[String]()
    var toProcess = Set[String]()
    var result = clauses

    do {
      toProcess = result.flatMap(cl => cl.literals.collect(l => l match {
        case ConceptLiteral(false, BaseConcept(d)) if defined(d) => d
      })) -- processed

      logger.trace(s"Processing: ${toProcess.mkString("[", ", ", "]")}")

      result ++= reference.filter( clause => clause.literals.exists(_.concept match {
        case UniversalRoleRestriction(_, BaseConcept(d)) => toProcess.contains(d)
        case ExistentialRoleRestriction(_, BaseConcept(d)) => toProcess.contains(d)
        case _ => false
      }))

      processed ++= toProcess
    } while(!toProcess.isEmpty)

    interestingDefiners ++= processed

    result
  }


  def defined(string: String) = string.startsWith("_D")

  case class LoopException(m: String) extends Exception(m)

  //   def propagateRoles() = {
  //     while(!clauses.isEmpty){
  //       val next = clauses.head
  //       clauses -= next

  //       logger.trace(s"Current clause: " + next.toString)

  //       var derivations = Set[Derivation]()
  //       derivations ++= rolePropagationRule.getDerivations(next, clauses++backgroundClauses)
  // //      derivations ++= inverseRolePropagationRule.getDerivations(next, clauses)

  //       derivations.flatMap(_.conclusions).foreach{ x =>
  //   	logger.trace(s"+ New clause: " + x.toString)
  // 			  if(!tautology(x) && !subsumed(x)){
  //   			    clauses += x
  // 			    all += x
  // 			  }
  //   			  else
  // 			    logger.trace(s"- tautology / subsumed")

  // 			}
  //       cycleCheck(false)
  //       old += next
  //     }
  //   }
  // <---- DON'T USE (background clauses not used properly, but is this method even useful?


  def resolve() = {
    ExtendedPurificationRule.purified=Set()
    //    CutUnusedFillersRule.rolePropagationRule=rolePropagationRule

    logger.trace(s"Number of background clauses: ${backgroundClauses.size}")
    logger.trace(s"Background clauses: \n${backgroundClauses.mkString("\n")}")

    def nextHigher(literal: ConceptLiteral) = literal match {
      case ConceptLiteral(polarity, BaseConcept(name)) => ConceptLiteral(polarity, BaseConcept(name+"'"))
    }

    var cancelExecution = false

    while(!clauses.isEmpty && !cancelExecution){

      checkCanceled


      if(largestClauseSet<all.size){
        largestClauseSet = all.size
        //	println("Largest clause set: "+largestClauseSet)
      }


      if(largestResultClauseSet<result.size){
        largestResultClauseSet = result.size
        //	println("Largest result clause set: "+largestResultClauseSet)
      }

      val next = clauses.head


      if(method==METHOD2)
        prefixSortedClauses -= clauses.head

      logger.trace(s"Current clause: ${next.toString}")
      if(tautology(next))
        logger.trace(s"- Tautology")
      else if(subsumedWithout(next))
        logger.trace(s"- Subsumed")
      else {
        if(!next.isDerivationCandidate && !next.literals.exists(nonBase) && (valid(next)||inverseRoles)){

          // case where we do not apply resolution
          // must be:
          //    1. a valid clause (only one negative definer literal)
          //    2. free of non base symbols
          //    3. no derivation candidate

          if(addToResult(next)){
            logger.trace(s".. checking for role propagations")
            var derivations = Set[Derivation]()
            derivations ++= rolePropagationRule.getDerivations(next, all)
            logger.trace("so far we got "+derivations)
            if(next.literals.exists(_.concept.isInstanceOf[ExistentialRoleRestriction])){
              // since the role propagation rule only checks for universal restrictions in the main clause,
              // and background clauses are not processed directly, we need an inner loop if the current
              // clause contains an existential role restriction
              derivations ++= backgroundClauses.flatMap(rolePropagationRule.getDerivations(_, Set(next)))
            }

            derivations.foreach(addConclusions)
            cycleCheck(false)
          }

        } else {

          // this is for the remaining cases, where resolution is neccessary, if possible


          if(next.isDerivationCandidate && !next.literals.exists(nonBase) && valid(next))
            addToResult(next) // might still be a valid result clause, if it is a derivation candidate


          // role propagations are only neccessary if we don't do any resolution
          if(!next.literals.head.concept.isInstanceOf[BaseConcept])
            rolePropagationRule.getDerivations(next, all).foreach(addConclusions)
          else if(method==METHOD2 && !ALCFormulaPreparations.isDefiner(next.literals.head.concept)){
            logger.trace(s"selecting appropriate clauses")

            var clauses4Resolution = prefixSortedClauses.range(new ConceptClause(Set(next.literals.head.negate), ordering),
              new ConceptClause(Set(nextHigher(next.literals.head.negate)), ordering)) // CHECK: OPTIMISE WITH VIEW?

            logger.trace(s"considering side clauses: ${clauses4Resolution}")
            clauses4Resolution = backgroundClauses ++ clauses4Resolution

            logger.trace(s"considering side clauses: ${clauses4Resolution}")

            //	  println("TY: "+clauses4Resolution.toString)
            //	  resolutionRule.getDerivations(next, clauses4Resolution).foreach(addClause)
            for(clause <- clauses4Resolution if (clauses++backgroundClauses).contains(clause))
              resolutionRule.getDerivations(next, Set(clause)).foreach(addConclusions)

            //} else if(ALCFormulaPreparations.isDefiner(next.literals.head.concept)){
          } else if(next.literals.exists(l => ALCFormulaPreparations.isDefiner(l.concept))) {
            // <-- if new clause with neg definer was resolved, resolutions should possibly be rechecked
            logger.trace(s"definer, rechecking old clauses")
            resolutionRule.getDerivations(next, all).foreach(addConclusions)
          } else {
            logger.trace(s"checking 'clauses'")
            resolutionRule.getDerivations(next, clauses++backgroundClauses).foreach(addConclusions)
          }



          cycleCheck(false)
        }
        old += next
      }
      logger.trace(s"Number of processed clauses: ${clauses.size.toString}" )
      clauses -= next
      // 'all' unchanged

    }
  }

  def addToResult(_clause: ConceptClause): Boolean = {

    logger.trace(s"add to result: ${_clause}")

    var res = false
    val time = new Date().getTime()

    var clause = _clause

    //if(!_clause.isResultCandidate) {
    //  logger.trace(s"not result candidate")
    //  return true
    //}

    if(condensing){
      clause = subsumptionChecker.condenseClause(_clause)
    }

    logger.trace(s"  adding ${clause.toString} to result.")
    //Some(clause).foreach{ clause =>

    var addClauses = if(undoStructuralTransBeforeAddToResult)
      structuralTransformer.transformBack(clause, ordering)
    else
      Set(clause)

    addClauses --= oldResultClauses

    addClauses.foreach { clause =>
      if(result.exists(subsumes(_, clause))) {
        logger.trace(s"${clause.toString} is subsumed in result.")
        //false
      }
      else {
        result --= result.filter{ x=>
          if(subsumes(clause, x)) {
            logger.trace(s"filtered out result clause ${x.toString}")
            true
          } else
            false
        }
        result += clause
        all += clause // important to make redundancy elimination preserve termination when using background ontology
        logger.trace(s"+ added ${clause.toString} to result.")

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

  def addConclusions(derivation: Derivation) = {
    val conclusions = derivation.conclusions.toSet--derivation.premisses
    if(derivation.premisses.exists(_.literals.exists(nonBase)))
      conclusions.foreach(_.isResultCandidate=true)// conclusions are only interesting for the result if
    // one of the premisses contains a non-base symbol
    conclusions.foreach(addClause)
  }

  def addClause(_clause: ConceptClause): Unit = {
    val time = new Date().getTime()
    var clause = _clause

    if(condensing) {
      logger.trace(s"condensing ${_clause.toString}")
      clause = subsumptionChecker.condenseClause(_clause)
      clause.isResultCandidate = _clause.isResultCandidate
    }


    var exisDeriv = ExistentialRoleRestrictionEliminationRule2.getDerivations(clause, all)
    exisDeriv ++= ExistentialRoleRestrictionEliminationRule1.getDerivations(clause, all)
    if(!exisDeriv.isEmpty){
      logger.trace(s"Existential role restriction elimination!")
      logger.trace(s"Brought us ${exisDeriv}")

      exisDeriv.flatMap(_.conclusions).foreach{
        x => // shouldn't miss resultcandidate property
          x.isResultCandidate=clause.isResultCandidate
          addClause(x)
      }

      return // (nothing else to do here, this clause would be subsumed anyway)
    }


    logger.trace(s"+ New clause: ${clause.toString}")
    counter += 1

    val startFiltering = new Date().getTime()

    if(tautology(clause)) {
      logger.trace(s"- tautology")
      return
    }

    if(!useSubsumptionTree) {
      if(subsumed(clause)) {
        logger.trace(s"- subsumed")
        return
      }
    } else if(useSubsumptionTree) {
      if(subsumptionTree.subsume(clause)){
        logger.trace(s"- subsumed")
        return
      }
    }


    if(!useSubsumptionTree){
      val subsumedOld= old.filter{
        x=>
          if(subsumes(clause, x)) {
            logger.trace(s"filtered out old clause ${x.toString}")
            true
          } else
            false
      }
      subsumedOld.foreach{ sub => old.remove(sub); all.remove(sub) }

      val subsumedProc = clauses.filter{
        x=>
          if(subsumes(clause, x)) {
            logger.trace(s"filtered out clause ${x.toString}")
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
      logger.trace(s"subsumed clauses: ${subsumed.toString}")
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
      logger.trace(s"More than one definer => Immediate resolution required!")
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

  // valid in this context does not mean tautology, but syntactically allowed
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

    logger.info(s"Cycle checking")


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
      logger.debug(s"New subsumptions: ${newSubsumptions.mkString("; ")}")
      mapBasedSubsumptionChecker.addSubsumptions(newSubsumptions)
    }
  }

  def recheckRedundancies(clauses: mutable.Set[ConceptClause]): Unit =
    clauses.foreach{ cl1 =>
      clauses.foreach{ cl2 =>
        if(cl1!=cl2 && subsumes(cl2, cl1)){
          logger.trace(s"subsumed ${cl1.toString}")
          clauses.remove(cl1)
        }
      }
    }
}
