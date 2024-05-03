package uk.ac.man.cs.lethe.internal.dl.forgetting.abox

import uk.ac.man.cs.lethe.internal.dl.AbstractMappedReasonerFactory
import uk.ac.man.cs.lethe.internal.dl.forgetting.direct.{SymbolOrderingByNumberOfOccurrences, _}

import java.util.Date
import java.util.concurrent.TimeoutException
import uk.ac.man.cs.lethe.internal.dl.forgetting.{QuickConceptForgetter, QuickRoleForgetter, RoleForgetter}
import uk.ac.man.cs.lethe.internal.dl.proofs.AbstractInferenceLogger
import uk.ac.man.cs.lethe.internal.tools.CanceledException
import uk.ac.man.cs.lethe.internal.tools.Timeoutable

import scala.collection.mutable.{HashMap, HashSet, MultiMap, TreeSet, Set => mutSet}

//import com.dongxiguo.fastring.Fastring.Implicits._

import com.typesafe.scalalogging.Logger

import uk.ac.man.cs.lethe.internal.dl.datatypes._
import uk.ac.man.cs.lethe.internal.dl.proofs.InferenceLogger
import uk.ac.man.cs.lethe.internal.forgetting.Forgetter
import uk.ac.man.cs.lethe.internal.tools.ConsoleProgressBar
import uk.ac.man.cs.lethe.internal.tools.ProgressBar
import uk.ac.man.cs.lethe.internal.tools.ProgressBarAttached


/**
  * Forgetting concept symbols from the ABox of an ontology.
  */
object ABoxForgetter extends Forgetter[Ontology, String] with ProgressBarAttached with Timeoutable {

  var noFiltration: Boolean = false // skip filtering of concepts out
  //of signature as optimisation

  //implicit val (logger, formatter, appender) = ZeroLoggerFactory.newLogger(this)
  //import formatter._
  val logger = Logger(ABoxForgetter.getClass)

  var inferenceLogger = InferenceLogger

  var inverseRoles = false

  override var progressBar: ProgressBar = new ConsoleProgressBar()

  var eliminateDisjunctions: Boolean = true

  var clausifyOnce = false

  var conceptsFirst = false

  private var reasonerFactory: AbstractMappedReasonerFactory = _

  def setReasonerFactory(reasonerFactory: AbstractMappedReasonerFactory) =
    this.reasonerFactory=reasonerFactory


  def forget(inputOntology: Ontology,
             _symbols: Set[String]) = {

    startTiming

    logger.debug(s"Time left: ${timeLeft}")

    ALCFormulaPreparations.initDefinitions()

    var symbols = _symbols

    /*var ontology = if(inverseRoles)
      OntologyFilter.restrictToSHI(inputOntology)
    else
      OntologyFilter.restrictToALC(inputOntology)
     */

    var ontology = OntologyFilter.restrictToSH(inputOntology)
      //OntologyFilter.restrictToALCH(inputOntology)

    var roleHierarchy = new RoleHierarchy(ontology.rbox)


    logger.info(s"Input ontology: \n${ontology}")



    val transToForget = symbols.filter(s => roleHierarchy.transitiveRoles(BaseRole(s)))


    if(!transToForget.isEmpty){
      throw new IllegalArgumentException(s"Forgetting transitive roles is not possible. The following transitive roles should be in the signature: ${transToForget.mkString(", ")}")
      //logger.info(s"Ignoring transitive roles: ${transToForget}")
      //symbols --= transToForget
    }



    if(!noFiltration){
      val quickRoleForgetter =
        new QuickRoleForgetter(ontology, ontology.roleSymbols.filter(symbols).map(BaseRole), roleHierarchy)
      ontology = quickRoleForgetter.purify()

      ontology = QuickConceptForgetter.forget(ontology, symbols.filter(ontology.atomicConcepts))
    }


    logger.trace(s"S1: ${symbols}")
    logger.trace(s"S1.5: ${symbols.filter(ontology.signature)}")

    val symbolOrdering =
      if(!conceptsFirst)
        new SymbolOrderingByNumberOfOccurrences();
      else
        new OrderingConceptsFirst(new SymbolOrderingByNumberOfOccurrences(),ontology.roleSymbols.filter(symbols))

    var orderedSymbols =
      symbolOrdering.order(symbols.filter(ontology.signature), ontology)

    logger.trace(s"S2: ${orderedSymbols}")

    val literalOrdering = new ConceptLiteralOrdering(orderedSymbols.toSeq)

    progressBar.init(orderedSymbols.size)

    var tboxClauses: Set[ConceptClause] = Set[ConceptClause]()
    var aboxClauses: Set[ABoxClause] = Set[ABoxClause]()
    var definitionClauses: Set[ConceptClause] = Set[ConceptClause]()
    var roleAssertionsX: Set[RoleAssertion] = Set[RoleAssertion]()

    if(clausifyOnce){
      tboxClauses =  ALCFormulaPreparations.clauses(ontology.tbox.axioms, literalOrdering)

      val (aboxClauses2, definitionClauses2, roleAssertionsX2) =
        ABoxClausification.clausify(ontology.abox.assertions, literalOrdering)

      aboxClauses = aboxClauses2;
      definitionClauses = definitionClauses2
      roleAssertionsX = roleAssertionsX2


      // TODO: not support for ABox clauses and role assertions yet
      inferenceLogger.sendInputClauses(tboxClauses)
      inferenceLogger.sendInputClauses(definitionClauses)
      //	  roleAssertions = roleAssertionsX2

    }

    var remainingSymbols = Set() ++ orderedSymbols

    val atomicConcepts = ontology.atomicConcepts
    val roleSymbols = ontology.roleSymbols

    try {
      while(!remainingSymbols.isEmpty){

        var orderedSymbols2 = symbolOrdering.order(remainingSymbols, ontology)
        var symbol = orderedSymbols2.head
        remainingSymbols -= symbol
        /*
        while(orderedSymbols2.size>1 && !ontology.atomicConcepts(symbol) && !ontology.roleSymbols(symbol)) {
          orderedSymbols2 = orderedSymbols2.tail
          symbol = orderedSymbols2.head
        }

        if(orderedSymbols2.size>1) {

          // TODO the previous 15 lines look like they can be written more elegant
*/
          checkCanceled


          logger.info(s"\n\n\nForgetting ${symbol}")
          progressBar.update(newMessage = "Forgetting " + symbol.split("#").last)
          logger.debug(s"Current ontology:\n ${ontology}")

          //println("Forgetting " + symbol)

          //quickRoleForgetter.ontology = ontology

          //ontology = quickRoleForgetter.purify()

          val symbolsLeft =
            orderedSymbols.toSet.filter(ontology.atomicConcepts)

          if (!noFiltration && !clausifyOnce)
            ontology = QuickConceptForgetter.forget(ontology, orderedSymbols.toSet)

          if (!clausifyOnce) {
            ALCFormulaPreparations.initDefinitions()
          }

          var (nbTBox, keepTBox) = ontology.tbox.axioms.partition(_.signature.contains(symbol))

          nbTBox = DefinerTools.definerClosureAxioms(keepTBox, nbTBox)

          var (nbABox, keepABox) = ontology.abox.assertions.partition(_ match {
            case _: RoleAssertion => true // role assertions can introduce new derivations in
            // connection with TBox axioms
            case d => d.signature.contains(symbol)
          })

          logger.trace(s"nbTBox:\n ${nbTBox.mkString("\n")}")
          logger.trace(s"keepTBox:\n ${keepTBox.mkString("\n")}")

          // apply structural transformation to decrease number of inferences
          val structuralTransformer = new
              StructuralTransformer(Set(symbol))

          inferenceLogger.structuralTransformer = structuralTransformer


          var tboxResult: Iterable[DLStatement] = Set[DLStatement]()
          var aboxResult: Iterable[DLStatement] = Set[DLStatement]()
          var roleAssertions = Set[RoleAssertion]()


          if (atomicConcepts(symbol)) {
            //   nbTBox = nbTBox.map(structuralTransformer.transform)

            if (!clausifyOnce) {
              tboxClauses = ALCFormulaPreparations.clauses(nbTBox, literalOrdering)

              logger.trace(s"clausification result:\n ${tboxClauses.mkString("\n")}")

              val (aboxClauses2, definitionClauses2, roleAssertionsX2) =
                ABoxClausification.clausify(nbABox, literalOrdering)
              aboxClauses = aboxClauses2
              definitionClauses = definitionClauses2
              roleAssertions = roleAssertionsX2
              // TODO: not support for ABox clauses and role assertions yet
              inferenceLogger.sendInputClauses(tboxClauses)
              inferenceLogger.sendInputClauses(definitionClauses)
            }

            progressBar.update(newMessage =
              "Forgetting " +
                // "("+tboxClauses.size+":"+
                // 		 aboxClauses.size+":"+
                // 		 definitionClauses.size+":"+
                // 		 roleAssertions.size+") "+
                symbol.split("#").last)

            logger.info("Forgetting concept")

            val aboxForgetter = new ABoxSingleConceptForgetter(roleHierarchy, inverseRoles)
            transferCancelInformation(aboxForgetter)
            aboxForgetter.inferenceLogger = inferenceLogger


            aboxForgetter.setForgetConcept(BaseConcept(symbol))
            aboxForgetter.setRoleAssertions(roleAssertions)

            val (_aboxClauses, _definitionClauses, _roleAssertions) =
              aboxForgetter.forget(aboxClauses, definitionClauses ++ tboxClauses)

            aboxClauses = _aboxClauses.toSet
            tboxClauses = _definitionClauses.toSet
            roleAssertions = _roleAssertions

            assert(!aboxClauses.exists(_.atomicConcepts(symbol)), aboxClauses.mkString("\n"))
            assert(!tboxClauses.exists(_.atomicConcepts(symbol)), tboxClauses.mkString("\n"))

            aboxResult = ABoxClausification.declausify(aboxClauses, tboxClauses)
            tboxResult = SimpleDefinerEliminator.eliminateDefiners(tboxClauses)

            // undo structural transformation
            tboxResult = tboxResult.map(structuralTransformer.transformBack)
            aboxResult = aboxResult.map(structuralTransformer.transformBack)
            logger.debug(s"\nResult clauses:\n" + aboxClauses.mkString("\n") + "\n" + tboxClauses.mkString("\n") + "\n")


          } else if (roleSymbols(symbol)) {

            // can't do structural transformation step if forgetting roles!

            if (!clausifyOnce) {
              tboxClauses = ALCFormulaPreparations.clauses(nbTBox, literalOrdering)

              val (aboxClauses2, definitionClauses2, roleAssertions2) =
                ABoxClausification.clausify(nbABox, literalOrdering)
              aboxClauses = aboxClauses2
              definitionClauses = definitionClauses2
              roleAssertions = roleAssertions2
            }

            progressBar.update(newMessage =
              "Forgetting " +
                // "("+tboxClauses.size+":"+
                // 		 aboxClauses.size+":"+
                // 		 definitionClauses.size+":"+
                // 		 roleAssertions.size+") "+
                symbol.split("#").last)

            logger.info("Forgetting role")

            def forget(role: BaseRole) = {
              val aboxForgetter = new ABoxSingleRoleForgetter(roleHierarchy, inverseRoles, reasonerFactory)
              aboxForgetter.inferenceLogger = inferenceLogger

              transferCancelInformation(aboxForgetter)


              aboxForgetter.setForgetRole(role)
              aboxForgetter.setRoleAssertions(roleAssertions)
              if (!clausifyOnce)
                aboxForgetter.setBackground(keepTBox)

              val (_aboxClauses, _definitionClauses, _roleAssertions) =
                aboxForgetter.forget(aboxClauses, definitionClauses ++ tboxClauses)

              RoleForgetter.roleHierarchy = roleHierarchy
              roleHierarchy = new RoleHierarchy(RoleForgetter.forgetInRBox(roleHierarchy.rbox, Set(role)))

              aboxClauses = _aboxClauses.toSet
              tboxClauses = _definitionClauses.toSet
              roleAssertions = _roleAssertions


              aboxResult = ABoxClausification.declausify(aboxClauses, tboxClauses)
              tboxResult = SimpleDefinerEliminator.eliminateDefiners(tboxClauses)

              tboxResult = tboxResult.map(structuralTransformer.transformBack)
              aboxResult = aboxResult.map(structuralTransformer.transformBack)
            }

            forget(BaseRole(symbol))
            logger.debug(s"\nResult clauses:\n" + aboxClauses.mkString("\n") + "\n" + tboxClauses.mkString("\n") + "\n")

          }
          ontology = Ontology.buildFrom(tboxResult)

          ontology.addStatements(aboxResult ++ roleAssertions)

          logger.trace("current ontology before definer purification: ")
          logger.trace(s"${ontology}")

          ontology = DefinerPurification.purifyRemainingDefiners(ontology)


          logger.trace("current ontology before simplification: ")
          logger.trace(s"${ontology}")

          ontology = CheapSimplifier.simplify(ontology)
          if (!clausifyOnce) {
            ontology.tbox.axioms ++= keepTBox
            ontology.abox.assertions ++= keepABox
          }
          ontology.rbox = roleHierarchy.rbox
          //      ontology = DLHelpers.simplify(ontology) <---- check again. should it be possible that this makes it slower?
          //      println("Disjunctive Assertions: "+aboxResult.filter(_.isInstanceOf[DisjunctiveConceptAssertion]).size)

          assert(!ontology.signature(symbol), symbol)

          OntologyBeautifier.makeNice(ontology)
          //println(uk.ac.man.cs.lethe.internal.tools.formatting.SimpleDLFormatter.format(ontology))
          //println(uk.ac.man.cs.lethe.internal.tools.formatting.SimpleDLFormatter.format(ontology))

          progressBar.increment()
          if(clausifyOnce)
            remainingSymbols = remainingSymbols.filter(s =>
              tboxClauses.exists(_.signature(s)) ||
                definitionClauses.exists(_.signature(s))  ||
                aboxClauses.exists(_.signature(s)) ||
                roleAssertions.exists(_.signature(s)) ||
                roleHierarchy.rbox.signature(s)
            )
          else
            remainingSymbols = remainingSymbols.filter(s => ontology.atomicConcepts(s) || ontology.roleSymbols(s))
        }

    } catch {
      case _: TimeoutException =>
        logger.info("Timeout occured!")
      case _: CanceledException =>
        logger.info("Execution canceled!")
        cancel
    }



    var result = ontology


    def countDefiners(ontology: Ontology) = {
      ontology.atomicConcepts.filter(_.startsWith("_D")).size
    }

    def countNumberRestrictions(ontology: Ontology) = {
      ontology.subConcepts.filterKeys(k => k.isInstanceOf[MinNumberRestriction]
        || k.isInstanceOf[MaxNumberRestriction]).sizeSum
    }


    if(eliminateDisjunctions &&
      ontology.abox.assertions.exists(_.isInstanceOf[DisjunctiveConceptAssertion]))
    {
      progressBar.setIndeterminate("Eliminating Disjunctions")

      ALCFormulaPreparations.initDefinitions()

      println("\n\nEliminating disjunctions ("+new Date()+")")
      println("Ontology before elimination:")
      println("Size of the output: "+result.size)
      println("Number of axioms: "+result.tbox.axioms.size)
      println("Max axiom size (output): "+(if(result.statements.isEmpty) 0 else result.statements.map(_.size).max))
      result = DLHelpers.splitConjunctions(result)
      println("Size of the output (split conjunctions): "+result.size)
      println("Number of axioms (split conjunctions): "+result.tbox.axioms.size)
      println("Max axiom size (split conjunctions): "+(if(result.statements.isEmpty) 0 else result.statements.map(_.size).max))
      println("Output ABox size: "+result.abox.size)
      println("Output ABox assertions: "+result.abox.assertions.size)
      println("Cyclic definers left: "+countDefiners(result))
      val caDisjunctions = result.abox.assertions.filter(_.isInstanceOf[DisjunctiveConceptAssertion]).size
      println("Disjunctive Concept Assertions: "+caDisjunctions)
      println()

      var roleAssertions = Set[RoleAssertion]()

      if(!clausifyOnce){
        tboxClauses =  ALCFormulaPreparations.clauses(ontology.tbox.axioms, literalOrdering)

        val (aboxClauses2, definitionClauses2, roleAssertions2) =
          ABoxClausification.clausify(ontology.abox, literalOrdering)
        aboxClauses = aboxClauses2
        definitionClauses = definitionClauses2
        roleAssertions = roleAssertions2
      }

      val aboxApproximator = new ABoxApproximator(roleHierarchy, literalOrdering, reasonerFactory)
      aboxApproximator.setRoleAssertions(roleAssertions)

      val (_aboxClauses, _definitionClauses, _) =
        aboxApproximator.forget(aboxClauses, definitionClauses++tboxClauses)
      aboxClauses = _aboxClauses.toSet
      tboxClauses = _definitionClauses.toSet
      logger.debug(s"\nResult clauses:\n"+aboxClauses.mkString("\n")+"\n"+tboxClauses.mkString("\n")+"\n")


      var aboxResult = ABoxClausification.declausify(aboxClauses, tboxClauses)
      aboxResult = ABoxDisjunctionEliminator.process(aboxResult, roleAssertions)
      val tboxResult = SimpleDefinerEliminator.eliminateDefiners(tboxClauses)

      //      println("Disjunctive Assertions: "+aboxResult.filter(_.isInstanceOf[DisjunctiveConceptAssertion]).size)

      ontology = Ontology.buildFrom(tboxResult)
      ontology.addStatements(aboxResult++roleAssertions)
      ontology = DefinerPurification.purifyRemainingDefiners(ontology)
      ontology = CheapSimplifier.simplify(ontology)

      println()

      logger.info("Eliminated disjunctions.")
      progressBar.finish()
    }

    //    println("Done!")

    logger.info("Canceled: "+isCanceled)
    logger.info("Timeout: "+timeoutOccurred)
    assert(isCanceled || timeoutOccurred || !ontology.signature.exists(symbols), ontology.signature.filter(symbols))

    logger.trace("Result before beautification:")
    logger.trace(s"${ontology}")

    OntologyBeautifier.makeNice(ontology)
    // if inverse roles are involved, the simplifications can make new definer elimination applications possible
    ontology = DefinerPurification.purifyRemainingDefiners(ontology)
    OntologyBeautifier.makeNice(ontology)

    logger.trace("Result after beautification:")
    logger.trace(s"${ontology}")


    ontology.rbox = roleHierarchy.rbox

    ontology
  }

  override def steps = 0
}
