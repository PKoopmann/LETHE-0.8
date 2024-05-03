package uk.ac.man.cs.lethe.internal.dl.abduction.forgetting

import uk.ac.man.cs.lethe.internal.dl.datatypes.extended.ExtendedABoxClause
import uk.ac.man.cs.lethe.internal.dl.forgetting.direct._

import java.util.Date
import java.util.concurrent.TimeoutException
import uk.ac.man.cs.lethe.internal.dl.forgetting.{QuickConceptForgetter, QuickRoleForgetter, RoleForgetter}
import uk.ac.man.cs.lethe.internal.dl.proofs.AbstractInferenceLogger
import uk.ac.man.cs.lethe.internal.forgetting.ForgetterWithBackgroundKB
import uk.ac.man.cs.lethe.internal.tools.CanceledException
import uk.ac.man.cs.lethe.internal.tools.Timeoutable

import scala.collection.mutable
import scala.collection.mutable.{HashMap, HashSet, MultiMap, TreeSet, Set => mutSet}

//import com.dongxiguo.fastring.Fastring.Implicits._

import com.typesafe.scalalogging.Logger

import uk.ac.man.cs.lethe.internal.dl.datatypes._
import uk.ac.man.cs.lethe.internal.dl.proofs.InferenceLogger
import uk.ac.man.cs.lethe.internal.forgetting.Forgetter
import uk.ac.man.cs.lethe.internal.tools.ConsoleProgressBar
import uk.ac.man.cs.lethe.internal.tools.ProgressBar
import uk.ac.man.cs.lethe.internal.tools.ProgressBarAttached


object ExtendedABoxForgetter extends ForgetterWithBackgroundKB[Ontology, String]
  with ProgressBarAttached
  with Timeoutable {

  var steps = 0
  var maxSteps = -1

  var noFiltration: Boolean = false // skip filtering of concepts out

  val logger = Logger(ExtendedABoxForgetter.getClass)

  var inferenceLogger: AbstractInferenceLogger = InferenceLogger

  override var progressBar: ProgressBar = new ConsoleProgressBar()

  var backgroundTBox: Set[ConceptClause] = _
  var backgroundABox: Set[ExtendedABoxClause] = _

  var allTBox: mutable.Set[ConceptClause] = _
  var allABox: mutable.Set[ExtendedABoxClause] = _

  var conceptForgetter: ExtendedABoxSingleConceptForgetter = _
  var roleForgetter: ExtendedABoxSingleRoleForgetter = _

  def setMaxSteps(maxSteps: Int) =
    this.maxSteps = maxSteps


  override def forget(ontology: Ontology,
                      _symbols: Set[String],
                      background: Ontology) = {

    startTiming

    steps = 0

    ALCFormulaPreparations.initDefinitions()

    var symbols = _symbols

    logger.info(s"Input ontology: \n${ontology}")


    logger.trace(s"S1: ${symbols}")
    logger.trace(s"S1.5: ${symbols.filter(ontology.signature)}")

    var orderedSymbols =
      AdvancedSymbolOrderings.orderByNumOfOccurrences(symbols.filter(ontology.signature), ontology).reverse

    logger.trace(s"S2: ${orderedSymbols}")

    val ordering = new ConceptLiteralOrdering(orderedSymbols.toSeq)

    val definerFactory = new DefinerFactory(ALCFormulaPreparations, ordering)

    progressBar.init(orderedSymbols.size)

    var tboxClauses: Set[ConceptClause] = Set[ConceptClause]()
    var aboxClauses: Set[ExtendedABoxClause] = Set[ExtendedABoxClause]()
    var definitionClauses: Set[ConceptClause] = Set[ConceptClause]()
    var roleAssertionsX: Set[RoleAssertion] = Set[RoleAssertion]()

    tboxClauses =  ALCFormulaPreparations.clauses(ontology.tbox.axioms, ordering)

    val (aboxClauses2, definitionClauses2) =
      ExtendedABoxClausification.clausify(ontology.abox.assertions, ordering)

    aboxClauses = aboxClauses2;
    tboxClauses = definitionClauses2++tboxClauses

    logger.trace("Clauses: ")
    logger.trace(s"${tboxClauses.mkString("\n")}")
    logger.trace(s"${aboxClauses.mkString("\n")}")

    val (backgroundABox2, backgroundTBox2) = ExtendedABoxClausification.clausify(background.abox, ordering)
    backgroundABox=backgroundABox2
    backgroundTBox=backgroundTBox2

    backgroundTBox ++= ALCFormulaPreparations.clauses(background.tbox.axioms)

    logger.trace("Background clauses: ")
    logger.trace(s"${backgroundTBox.mkString("\n")}")
    logger.trace(s"${backgroundABox.mkString("\n")}")

    var remainingSymbols = Set() ++ orderedSymbols

    allABox = new HashSet[ExtendedABoxClause] ++ aboxClauses ++ backgroundABox
    allTBox = new HashSet[ConceptClause] ++ tboxClauses ++ backgroundTBox

    conceptForgetter=
      new ExtendedABoxSingleConceptForgetter()
    conceptForgetter.setDefinerFactory(definerFactory)
    roleForgetter =
      new ExtendedABoxSingleRoleForgetter()
    roleForgetter.setDefinerFactory(definerFactory)

    Set(conceptForgetter, roleForgetter).foreach { f =>
      transferCancelInformation(f)
      f.inferenceLogger = inferenceLogger
    }

    //aboxForgetter.setRoleAssertions(roleAssertions)

    try {
      while(!remainingSymbols.isEmpty && (maxSteps == -1 || steps<maxSteps)) {

        val orderedSymbols2 = AdvancedSymbolOrderings
          .orderByNumOfOccurrences(remainingSymbols, ontology)
          .reverse

        val symbol = orderedSymbols2.head

        remainingSymbols -= symbol

        checkCanceled

        logger.info(s"\n\n\nForgetting ${symbol}")
        progressBar.update(
          newMessage = "Forgetting " + symbol.split("#").last)


        if (ontology.atomicConcepts(symbol) || background.atomicConcepts(symbol)) {
          //   nbTBox = nbTBox.map(structuralTransformer.transform)


          progressBar.update(newMessage =
            "Forgetting " +
              // "("+tboxClauses.size+":"+
              // 		 aboxClauses.size+":"+
              // 		 definitionClauses.size+":"+
              // 		 roleAssertions.size+") "+
              symbol.split("#").last)

          logger.info("Forgetting concept")

          conceptForgetter.setForgetConcept(BaseConcept(symbol))

          // need to initialise before setting the background KB, to make sure all datastructures are present
          conceptForgetter.init()

          // needs to reset background each time after setting forget symbol since
          // ordering in clauses changed
          conceptForgetter.setBackground(backgroundTBox, backgroundABox)

          logger.debug("Background:")
          logger.debug(s"${backgroundTBox.mkString("\n")}")
          logger.debug(s"${backgroundABox.mkString("\n")}")

          logger.debug("Main clauses:")
          logger.debug(s"${aboxClauses.mkString("\n")}")
          logger.debug(s"${tboxClauses.mkString("\n")}")

          val (_aboxClauses, _definitionClauses) =
            conceptForgetter.forget(aboxClauses, tboxClauses)

          aboxClauses = _aboxClauses.toSet
          tboxClauses = _definitionClauses.toSet

          // the following assertions, from the ABox forgetter, do not work here, as our methdo for
          // forgetting with background knowledge may reintroduce even the currently forgotten name.
          // (see code there for an explanation)
          //assert(!aboxClauses.exists(_.atomicConcepts(symbol)), aboxClauses.mkString("\n"))
          //assert(!tboxClauses.exists(_.atomicConcepts(symbol)), tboxClauses.mkString("\n"))

          // since the background changes during forgetting, we need to grab it again
          backgroundTBox = conceptForgetter.backgroundTBox.toSet
          backgroundABox = conceptForgetter.backgroundABox.toSet



          // aboxResult = ExtendedABoxClausification.declausify(aboxClauses, tboxClauses)
          // tboxResult = SimpleDefinerEliminator.eliminateDefiners(tboxClauses)

          // undo structural transformation
          // tboxResult = tboxResult.map(structuralTransformer.transformBack)
          // aboxResult = aboxResult.map(structuralTransformer.transformBack)
          logger.debug(s"\nResult clauses:\n" + aboxClauses.mkString("\n") + "\n" + tboxClauses.mkString("\n") + "\n")

        }
        if (ontology.roleSymbols(symbol) || background.roleSymbols(symbol)) {

          // can't do structural transformation step if forgetting roles!

          progressBar.update(newMessage =
            "Forgetting " +
              // "("+tboxClauses.size+":"+
              // 		 aboxClauses.size+":"+
              // 		 definitionClauses.size+":"+
              // 		 roleAssertions.size+") "+
              symbol.split("#").last)

          logger.info("Forgetting role")

          def forget(role: BaseRole) = {


            roleForgetter.setForgetRole(role)
            roleForgetter.init()

            // needs to set background each time, as the literal ordering changes after setting
            // the forget role
            roleForgetter.setBackground(backgroundTBox, backgroundABox)

            val (_aboxClauses, _definitionClauses) =
              roleForgetter.forget(aboxClauses, tboxClauses)

            aboxClauses = _aboxClauses.toSet
            tboxClauses = _definitionClauses.toSet

            // no role hierarchy for now
            //RoleForgetter.roleHierarchy = roleHierarchy
            //roleHierarchy = new RoleHierarchy(RoleForgetter.forgetInRBox(roleHierarchy.rbox, Set(role)))

            //aboxResult = ExtendedABoxClausification.declausify(aboxClauses, tboxClauses)
            //tboxResult = SimpleDefinerEliminator.eliminateDefiners(tboxClauses)

            //tboxResult = tboxResult.map(structuralTransformer.transformBack)
            //aboxResult = aboxResult.map(structuralTransformer.transformBack)

            // since the background changes during forgetting, we need to grab it again
            backgroundTBox = roleForgetter.backgroundTBox.toSet
            backgroundABox = roleForgetter.backgroundABox.toSet
          }

          forget(BaseRole(symbol))
          logger.debug(s"\nResult clauses:\n" + aboxClauses.mkString("\n") + "\n" + tboxClauses.mkString("\n") + "\n")

        }

        progressBar.increment()

        val currentSignature = aboxClauses.flatMap(_.signature)++tboxClauses.flatMap(_.signature)

        remainingSymbols = _symbols.filter(currentSignature)
        logger.trace(s"Remaining symbols to forget: ${remainingSymbols}")
        steps+=1
      }
      logger.debug("Done with forgetting.")
    } catch {
      case _: TimeoutException =>
        logger.info("Timeout occured!")
        logger.info(s"Symbols left: ${remainingSymbols}")
      case _: CanceledException =>
        logger.info("Execution canceled!")
        cancel
    }

    if(maxSteps > -1 && steps>maxSteps)
      logger.info("Canceled due max number of steps")

    var aboxResult = (new ExtendedABoxDeclausification).declausify(aboxClauses, tboxClauses)
    var tboxResult = SimpleDefinerEliminator.eliminateDefiners(tboxClauses)

    var result = Ontology.buildFrom(tboxResult)

    result.addStatements(aboxResult)

    result = DefinerPurification.purifyRemainingDefiners(result)




    logger.trace("current ontology before simplification: ")
    logger.trace(s"${result}")


    assert(isCanceled || !result.signature.exists(symbols), result.signature.filter(symbols))

    logger.trace("Result before beautification:")
    logger.trace(s"${result}")

    OntologyBeautifier.makeNice(result)
    // if inverse roles are involved, the simplifications can make new definer elimination applications possible
    result = DefinerPurification.purifyRemainingDefiners(result)
    OntologyBeautifier.makeNice(result)


    result
  }


  override def cancel: Unit = {
    super.cancel
    transferCancelInformationToDelegates()
  }

  override def uncancel: Unit = {
    super.uncancel
    transferCancelInformationToDelegates()
  }

  override def useTimeout(value: Long) = {
    super.useTimeout(value)
    transferCancelInformationToDelegates()
  }

  override def useTimeout(value: Boolean = true) = {
    super.useTimeout(value)
    transferCancelInformationToDelegates()
  }

  override def startTiming = {
    super.startTiming
    transferCancelInformationToDelegates()
  }

  private def transferCancelInformationToDelegates(): Unit = {
    if (conceptForgetter != null) {
      transferCancelInformation(conceptForgetter)
    }
    if (roleForgetter != null) {
      transferCancelInformation(roleForgetter)
    }
  }
}
