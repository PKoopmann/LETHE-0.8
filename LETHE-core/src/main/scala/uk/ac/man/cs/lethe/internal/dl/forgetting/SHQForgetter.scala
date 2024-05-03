package uk.ac.man.cs.lethe.internal.dl.forgetting

import  uk.ac.man.cs.lethe.internal.dl.forgetting.direct._

import scala.collection.mutable.{ Set => mutSet, HashSet, HashMap, MultiMap, TreeSet }

//import com.dongxiguo.fastring.Fastring.Implicits._
import com.typesafe.scalalogging.Logger

import uk.ac.man.cs.lethe.internal.dl.datatypes._
import uk.ac.man.cs.lethe.internal.forgetting.Forgetter
import uk.ac.man.cs.lethe.internal.tools.ConsoleProgressBar
import uk.ac.man.cs.lethe.internal.tools.{ ProgressBar, ProgressBarAttached }


object SHQForgetter extends Forgetter[Ontology, String] with ProgressBarAttached { 

//  implicit val (logger, formatter, appender) = ZeroLoggerFactory.newLogger(this)
//  import formatter._

  val logger = Logger(SHQForgetter.getClass)

  override var progressBar: ProgressBar = new ConsoleProgressBar()

  def forget(inputOntology: Ontology, symbols: Set[String]) = { 
    inputOntology.abox = new ABox(Set())
//    println(inputOntology)
    forgetVariant2(OntologyFilter.restrictToSHQ(inputOntology), symbols)
  }

  // test on one example made variant 2 at least twice faster than variant 1 (variant 1 killed after 3 minutes, variant 2 took 1:30)
  def forgetVariant1(inputOntology: Ontology, symbols: Set[String]) = { 
    ALCFormulaPreparations.initDefinitions()

    val roleHierarchy = new RoleHierarchy(inputOntology.rbox)

    val ordering = new ConceptLiteralOrdering(symbols.toSeq)

    val definerFactory = new DefinerFactory(ALCFormulaPreparations, ordering, roleHierarchy)

    var clauses =  ALCFormulaPreparations.clauses(inputOntology.tbox.axioms, ordering)

    logger.info(s"\nInput Clauses:\n${clauses.mkString("\n")}")

    val singleConceptForgetter = new SHQSingleConceptForgetter(roleHierarchy, Set[BaseRole]())

    progressBar.init(symbols.size)

    symbols.foreach{ symbol => 
      logger.debug(s"\n\n\nForgetting ${symbol}")

      progressBar.update(newMessage=symbol.split("#").last)

      clauses = singleConceptForgetter.forget(clauses, BaseConcept(symbol))

      progressBar.increment()
    }

    var tboxStatements = SimpleDefinerEliminator.eliminateDefiners(clauses)    

    var result = Ontology.buildFrom(tboxStatements)
    result = DLHelpers.simplify(result)
    result.rbox = inputOntology.rbox

    DefinerPurification.purifyRemainingDefiners(result)

    result
  } 

  def forgetVariant2(inputOntology: Ontology, symbols: Set[String]) = { 

    val roleHierarchy = new RoleHierarchy(inputOntology.rbox)

    val orderedSymbols = //symbols
      SymbolOrderings.orderByNumOfOccurrences(symbols, inputOntology).reverse


    val ordering = new ConceptLiteralOrdering(orderedSymbols.toSeq)

    val singleConceptForgetter = new SHQSingleConceptForgetter(roleHierarchy, Set[BaseRole]())

    var ontology = inputOntology

    progressBar.init(symbols.size)

    orderedSymbols.foreach{ symbol => 
      logger.debug(s"\n\n\nForgetting ${symbol}")

//      var structuralTransformer = new StructuralTransformer(Set(symbol))
//      ontology = structuralTransformer.transform(ontology)

      val message = "Forgetting "+symbol.split("#").last//+" ("+SymbolOrderings.countOccurrences(symbol, ontology)+" occ.)"
      progressBar.update(newMessage=message)

      ALCFormulaPreparations.initDefinitions()
//      progressBar.update(newMessage=message+" 1")

      var (nb, keep) = ontology.tbox.axioms.partition(_.atomicConcepts.contains(symbol))
      var clauses =  ALCFormulaPreparations.clauses(nb, ordering)

      // get rid of ex and universal restrictions
      clauses = clauses.map(c => new ConceptClause(CardinalityHelpers.onlyNumberRestrictions(c).literals, 
					      ordering)
)

//      progressBar.update(newMessage=message+" 3 "+clauses.size+" "+keep.size+"  ")
      logger.info(s"\nInput Clauses:\n${clauses.mkString("\n")}")

      clauses = singleConceptForgetter.forget(clauses, BaseConcept(symbol))
      logger.info(s"\nOutput Clauses:\n${clauses.mkString("\n")}")


//      progressBar.update(newMessage=message+" 4")
      var tboxStatements = SimpleDefinerEliminator.eliminateDefiners(clauses)    
      logger.trace(s"elimated definers: \n${tboxStatements.mkString("\n")}")
//      progressBar.update(newMessage=message+" 5")
      ontology = Ontology.buildFrom(tboxStatements)
      logger.trace(s"first ontology: \n${ontology}") 
//      ontology = DLHelpers.simplify(ontology)
      ontology = DefinerPurification.purifyRemainingDefiners(ontology)
      logger.trace(s"definers purified: \n${ontology}")
//      progressBar.update(newMessage=message+" 6")
      ontology = CheapSimplifier.simplify(ontology)
      logger.trace(s"simplified: \n${ontology}")
//      progressBar.update(newMessage=message+" 7")
      ontology.tbox.axioms ++= keep
//      progressBar.update(newMessage=message+" 8")

//      ontology = structuralTransformer.transformBack(ontology)

      progressBar.increment()
    }
    progressBar.finish()

//    ontology = DLHelpers.simplify(ontology)


    ontology = DLHelpers.splitConjunctions(ontology)

    ontology = DefinerPurification.purifyRemainingDefiners(ontology)

    ontology = CheapSimplifier.simplify(ontology)

    OntologyBeautifier.makeNice(ontology)


    ontology.rbox = inputOntology.rbox

    ontology
  } 
  

  // variant 3: using structural transformation
  // not much faster than variant 2: killed same example after 3 minutes
  def forgetVariant3(inputOntology: Ontology, symbols: Set[String]) = { 
    ALCFormulaPreparations.initDefinitions()

    val roleHierarchy = new RoleHierarchy(inputOntology.rbox)

    val ordering = new ConceptLiteralOrdering(symbols.toSeq)

    val singleConceptForgetter = new SHQSingleConceptForgetter(roleHierarchy, Set[BaseRole]())

    var ontology = inputOntology


    symbols.foreach{ symbol => 
      logger.debug(s"\n\n\nForgetting ${symbol}")

      var structuralTransformer = new StructuralTransformer(Set(symbol))
      ontology = structuralTransformer.transform(ontology)


      ALCFormulaPreparations.initDefinitions()

      var clauses =  ALCFormulaPreparations.clauses(ontology.tbox.axioms, ordering)

      logger.info(s"\nInput Clauses:\n${clauses.mkString("\n")}")

      clauses = singleConceptForgetter.forget(clauses, BaseConcept(symbol))

      logger.info(s"\nOutput Clauses:\n${clauses.mkString("\n")}")

      var tboxStatements = SimpleDefinerEliminator.eliminateDefiners(clauses)    

      ontology = Ontology.buildFrom(tboxStatements)
      ontology = structuralTransformer.transformBack(ontology)
      ontology = DLHelpers.simplify(ontology)
    }



    ontology.rbox = inputOntology.rbox

    

    ontology
  } 
  

  override def steps = 0 // mock
}
