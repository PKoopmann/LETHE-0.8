package uk.ac.man.cs.lethe.internal.dl.proofs

import com.typesafe.scalalogging.Logger

import java.io.{File, PrintWriter}
import uk.ac.man.cs.lethe.internal.dl.datatypes._
import uk.ac.man.cs.lethe.internal.dl.forgetting.direct.{ALCFormulaPreparations, AbstractDerivation, ConceptClause, DefinerFactory, Derivation, SimpleDefinerEliminator, StructuralTransformer}
import uk.ac.man.cs.lethe.internal.tools.formatting.SimpleDLFormatter

case class TranslatedDerivation(
  premisses: Set[DLStatement],
  conclusions: Set[DLStatement])

trait AbstractInferenceLogger {
  def sendInference(derivation: AbstractDerivation)
  def sendInference(derivations: Iterable[AbstractDerivation])
  def sendInputClauses(clauses: Iterable[ConceptClause])
  def notifyDefinerFactory(definerFactory: DefinerFactory)
  var derivations: List[AbstractDerivation]
  var inputClauses: Set[ConceptClause]
  def clear(): Unit
}

object InferenceLogger extends AbstractInferenceLogger {

  val logger = Logger(InferenceLogger.getClass)

  val dummyLogger = new AbstractInferenceLogger {
    override def sendInference(derivation: AbstractDerivation): Unit = {}
    override def sendInference(derivations: Iterable[AbstractDerivation]): Unit = {}
    override def sendInputClauses(clauses: Iterable[ConceptClause]): Unit = {}
    override def notifyDefinerFactory(definerFactory: DefinerFactory): Unit = {}
    override var derivations = List[AbstractDerivation]()
    override var inputClauses = Set()
    override def clear() = {}
  }

  var inputClauses = Set()

  var derivations = List[AbstractDerivation]()

  var translatedDerivations = Set[TranslatedDerivation]()

  var structuralTransformer = new StructuralTransformer(Set())

  var definerFactories:Set[DefinerFactory] = Set[DefinerFactory]()

  //val printWriter = new PrintWriter(new File("inferences.txt"))

  override def clear(): Unit = {
    this.inputClauses = Set()
    this.derivations = List()
    this.translatedDerivations = Set()
    structuralTransformer = new StructuralTransformer(Set())
    definerFactories = Set()
  }

  override def sendInference(derivation: AbstractDerivation): Unit = {

    logger.trace(s"receive: ${derivation}")

    derivations ::= derivation.copy

   /* val trans =
      TranslatedDerivation(derivation.premisses.map(translate).toSet,
        derivation.conclusions.map(translate).toSet)

    //val trans =
    //  TranslatedDerivation(derivation.premisses.map(toStatement).toSet,
     //   derivation.conclusions.map(toStatement).toSet)

    translatedDerivations += trans
*/
    //println(format(trans))

    //printWriter.println(format(trans))
  }

  override def sendInference(derivations: Iterable[AbstractDerivation]): Unit = {
    derivations.foreach(sendInference)
  }

  override def sendInputClauses(inputClauses: Iterable[ConceptClause]) = {
    this.inputClauses++=inputClauses
  }

  override def notifyDefinerFactory(definerFactory: DefinerFactory) =
    definerFactories += definerFactory

  def printProofFor(axiom: Axiom) {
    println()
    println()
    println("Proof for "+formatAxiom(axiom))
    println("=======================================================")
    println()
    println()
    val trace = retrace(axiom)
    trace.map(format).foreach(println)
  }


  def retrace(axiom: DLStatement): List[TranslatedDerivation] = {
    retrace(axiom, Set[TranslatedDerivation]())
  }

  def retrace(axiom: DLStatement, visited: Set[TranslatedDerivation])
      : List[TranslatedDerivation] = {
    var trace = List[TranslatedDerivation]()

    translatedDerivations.find(_.conclusions.contains(axiom)).foreach{ der =>
      if(!visited.contains(der)){
        trace = der :: trace
        der.premisses.foreach{ prem =>
          trace = retrace(prem, visited+der) ++ trace;
        }
      }
    }

    return trace
  }


  def format(derivation: TranslatedDerivation): String = {
    
    return derivation.premisses.map(formatAxiom).mkString("\n")+
    "\n----------------------------------------------------------\n"+
    derivation.conclusions.map(formatAxiom).mkString("\n")+"\n"
  }

  def toStatement(clause: ConceptClause): DLStatement = {
    Subsumption(TopConcept, clause.convertBack)
  }

  def translate(clause: ConceptClause): DLStatement = {
    var gci: Axiom = SimpleDefinerEliminator.toSubsumption(clause)

    gci = weakTransformBack(gci)

    gci = structuralTransformer.transformBack(gci)

    gci = weakTransformBack(gci)

    OntologyBeautifier.nice(gci)
  }

  def formatAxiom(axiom: DLStatement) = {
    //    structuralTransformer.transformBack(clause)
    SimpleDLFormatter.format(axiom)
  }


  def weakTransformBack(axiom: Axiom): Axiom = axiom match {
    case Subsumption(c1, c2) => Subsumption(weakTransformBack(c1), weakTransformBack(c2))
    case ConceptEquivalence(c1, c2) => ConceptEquivalence(weakTransformBack(c1), weakTransformBack(c2))
  }
  
  def weakTransformBack(assertion: Assertion): Assertion = assertion match { 
    case ConceptAssertion(c, i) => ConceptAssertion(weakTransformBack(c), i)
    case r: RoleAssertion => r
  }

  def weakTransformBack(concept: Concept): Concept = concept match { 
//    case l: ConceptLiteral => weakTransformBack(l.convertBack)
    case TopConcept => TopConcept
    case BottomConcept => BottomConcept
    case d: BaseConcept => d //translateDefiner(d)
    case ConceptComplement(c) => ConceptComplement(weakTransformBack(c))
    case ConceptConjunction(cs) => ConceptConjunction(cs.map(weakTransformBack))
    case ConceptDisjunction(ds) => ConceptDisjunction(ds.map(weakTransformBack))
    case ExistentialRoleRestriction(r,c) => ExistentialRoleRestriction(r, weakTransformBack(c))
    case UniversalRoleRestriction(r, c) => UniversalRoleRestriction(r, weakTransformBack(c))
    case MinNumberRestriction(n,r,c) => MinNumberRestriction(n,r,weakTransformBack(c))
    case MaxNumberRestriction(n,r,c) => MaxNumberRestriction(n,r,weakTransformBack(c))
  }

  def translateDefiner(concept: BaseConcept): Concept = {
    var defs = Set(concept)

    definerFactories.foreach{df =>
      val bd = df.getBaseDefiners(concept)
      if(bd.size>1)
        defs = bd
    }

    val conjuncts = defs.map(d => 
      if(ALCFormulaPreparations.definerMap.contains(d))
        ALCFormulaPreparations.definerMap(d)
      else
        d)

    return DLHelpers.conjunction(conjuncts)
  }

}
