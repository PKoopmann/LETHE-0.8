package uk.ac.man.cs.lethe.internal.dl.forgetting

import java.util.Date
import com.typesafe.scalalogging.Logger
import uk.ac.man.cs.lethe.internal.dl.AbstractMappedReasonerFactory
import uk.ac.man.cs.lethe.internal.dl.forgetting.direct.AdvancedSymbolOrderings
import uk.ac.man.cs.lethe.internal.tools.Timeoutable

//import com.dongxiguo.fastring.Fastring.Implicits._
import scala.collection.mutable.{ HashMap, MultiMap, Set => MutSet, HashSet }
import uk.ac.man.cs.lethe.internal.dl.analysis.ConceptSymbolAnalysis
import uk.ac.man.cs.lethe.internal.dl.analysis.DeepSymbolAnalyser
import uk.ac.man.cs.lethe.internal.dl.analysis.RoleSymbolAnalysis
import uk.ac.man.cs.lethe.internal.dl.analysis.SymbolAnalysis


import uk.ac.man.cs.lethe.internal.dl.datatypes._
import uk.ac.man.cs.lethe.internal.dl.forgetting.direct.DefinerPurification
import uk.ac.man.cs.lethe.internal.dl.forgetting.direct.RoleHierarchy
import uk.ac.man.cs.lethe.internal.dl.forgetting.direct.SimpleDefinerEliminator
import uk.ac.man.cs.lethe.internal.dl.forgetting.direct.SymbolOrderings
import uk.ac.man.cs.lethe.internal.forgetting.Forgetter
import uk.ac.man.cs.lethe.internal.tools.{ ProgressBar, ProgressBarAttached, ConsoleProgressBar, MockProgressBar }


object ConceptAndRoleForgetter extends Forgetter[Ontology, String] with ProgressBarAttached with Timeoutable {

  var maxOccurrencesConcepts = 25
  var maxUnivConcepts = 25
  var maxOccurrencesRoles = 11
  var maxUnivRoles = 7
  var maxUnivRRoles = 10

  var ignore = Set[String]()

  //implicit val (logger, formatter, appender) = ZeroLoggerFactory.newLogger(this)

  val logger = Logger(ConceptAndRoleForgetter.getClass)

  var filter=false
  
  var thisProgressBar: ProgressBar = new ConsoleProgressBar()


  var reasonerFactory: AbstractMappedReasonerFactory = _

  def setReasonerFactory(reasonerFactory: AbstractMappedReasonerFactory) = {
    this.reasonerFactory=reasonerFactory
    RoleForgetter.reasonerFactory=reasonerFactory
  }

  override def progressBar_=(newProgressBar: ProgressBar) = { 
    //super.progressBar = newProgressBar
    //DirectALCForgetter.progressBar = newProgressBar
    //RoleForgetter.progressBar = newProgressBar

    thisProgressBar = newProgressBar
    DirectALCForgetter.progressBar = MockProgressBar
    RoleForgetter.progressBar = MockProgressBar
  }

  override def steps = RoleForgetterS.steps+DirectALCForgetter.steps

  override def forget(_ontology: Ontology, _symbols: Set[String]) = {

    startTiming

    logger.info("Removing non ALCH axioms")
    val ontology = OntologyFilter.restrictToALCH(_ontology)

    logger.info("Removing ABox axioms")
    ontology.abox = new ABox(Set())

    var result = ontology

    logger.debug(s"ontology after filtering: \n${result}")

    val allRoles = ontology.roleSymbols
//    println("Forgetting "+_symbols.size+" symbols")
//    println("Forgetting "+_symbols.filter(allRoles).size+" role symbols")


    // quickforget concepts
    result = QuickConceptForgetter.forget(result, _symbols)

    logger.debug(s"ontology after quick forgetting concepts: \n${result}")

    // quickforget roles
    var quickRoleForgetter = new QuickRoleForgetter(result, 
    						    _symbols.filter(allRoles).map(BaseRole), 
    						    new RoleHierarchy(result))
   result = quickRoleForgetter.purify()

    var symbols = _symbols //-- quickRoleForgetter.removed.map(_.name)
    symbols = if(filter) { 
      ignore ++= symbols--filter(symbols, result) 
      symbols -- ignore
    }else { 
//      println("No filtering")
      symbols
    }

    logger.debug(s"ontology after quickforgetting: \n${result}")

    symbols --= ignore


    // New Idea: Forget ordered by occurrences, not roles at the end


    // Don't mess with individual progressbars anymore
    DirectALCForgetter.progressBar = MockProgressBar
    RoleForgetter.progressBar = MockProgressBar

    val concepts = result.atomicConcepts
    var roles = result.roleSymbols

    
    thisProgressBar.init(symbols.size)

    var remainingSymbols = symbols.filter(ontology.signature)

    while(!remainingSymbols.isEmpty) {

      logger.trace(s"ordering and selecting next symbol")

      val orderedSymbols =
          AdvancedSymbolOrderings.orderByNumOfOccurrences(remainingSymbols, result).reverse

      val symbol = orderedSymbols.head
      remainingSymbols -= symbol

      if(!isCanceled){
        thisProgressBar.update(newMessage = "Forgetting "+symbol.split("#").last)
        thisProgressBar.increment()
//        thisProgressBar.increment(symbol.split("#").last)

        if(concepts(symbol)){
          logger.trace(s"forget concept name ${symbol}")

          transferCancelInformation(DirectALCForgetter)

          result = DirectALCForgetter.forget(result, Set(symbol))

        }
        // no else in case a role name is also used as a concept name
        if(roles(symbol)) {
          logger.trace(s"forget role name ${symbol}")

          transferCancelInformation(RoleForgetter)

          result = RoleForgetterS.forget(result, Set(symbol))
        }
      }

      logger.debug(s"Current ontology: \n ${result}")
    }

    logger.trace(s"done with eliminating all symbols")
    

    /*

    logger.info("Forgetting concepts...("+(new Date())+")")
    result = DirectALCForgetter.forget(result, symbols.filter(result.atomicConcepts))

    quickRoleForgetter = new QuickRoleForgetter(result, 
    					    _symbols.filter(allRoles).map(BaseRole), 
    					    new RoleHierarchy(result)) // reset everything

    result = quickRoleForgetter.purify()

    logger.info("Forgetting roles... ("+(new Date())+")")
    // result = quickRoleForgetter.quickForget() <--- UNSOUND ! ! ! (code commented)

    result = RoleForgetterS.forget(result, symbols.filter(result.roleSymbols))
    */


    logger.trace(s"purifying remaining definers")
    DefinerPurification.purifyRemainingDefiners(result)

    logger.trace(s"making nice")
    OntologyBeautifier.makeNice(result)

    assert(isCanceled || !result.signature.exists(_symbols), "symbol not supposed to be here:"+result.signature.filter(_symbols))

    if(isCanceled){
      println(s"Execution stopped: ${result.signature.filter(_symbols).size} symbols not eliminated")
    }

    logger.trace(s"done with all")
    result
  }

  def filter(symbols: Set[String], ontology: Ontology) = { 
    val analysisMap = DeepSymbolAnalyser.analyseSymbols(ontology)

    val result = symbols.filter{ symbol =>
      if(!analysisMap.contains(BaseConcept(symbol)) && !analysisMap.contains(BaseRole(symbol))){ 
//	println("Not found: "+symbol)
	true
      } else { 
	
	val analysis = if(analysisMap.contains(BaseConcept(symbol)))
	  analysisMap(BaseConcept(symbol))
		       else
	  analysisMap(BaseRole(symbol))
	
	var result = false

	analysis match { 
	  case cs: ConceptSymbolAnalysis => result = (cs.onlyNegative||cs.onlyPositive)
	  case rs: RoleSymbolAnalysis => result = false //(rs.onlyExistentiallyRestricted||rs.onlyExistentiallyRestricted)
	}
	result = result || analysisOkay(analysis)
	// if(!result)
	//   println("Ignoring the following: "+analysis)
	result
      } 
    }
    println("Ignoring "+(symbols.size-result.size)+" symbols.")
    result
  }

  def analysisOkay(analysis: SymbolAnalysis) = analysis match { 
    case cs: ConceptSymbolAnalysis => 
      (analysis.occurrences<=maxOccurrencesConcepts && analysis.underUniversalRestrictions<=maxUnivConcepts)
    case rs: RoleSymbolAnalysis => 
      (analysis.occurrences<=maxOccurrencesRoles && analysis.underUniversalRestrictions<=maxUnivRoles)
  }

  def clean() { 
    RoleForgetter.clean()
    DirectALCForgetter.clean()
  }
}
