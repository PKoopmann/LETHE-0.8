package uk.ac.man.cs.lethe.internal.dl.filters


import java.io.{File, FileOutputStream, OutputStreamWriter, PrintWriter}
import org.semanticweb.owlapi.model.parameters.Imports

import scala.collection.JavaConverters._
import scala.collection.convert.ImplicitConversions.`collection AsScalaIterable`
import scala.collection.mutable.{HashMap, MultiMap, Set => MutSet}
import scala.io.Source
import com.typesafe.scalalogging.Logger
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model._
import uk.ac.man.cs.lethe.internal.dl.datatypes._
import uk.ac.man.cs.lethe.internal.dl.forgetting.direct.ALCFormulaPreparations
import uk.ac.man.cs.lethe.internal.dl.forgetting.direct.ConceptClause
import uk.ac.man.cs.lethe.internal.dl.forgetting.direct.RoleHierarchy
import uk.ac.man.cs.lethe.internal.dl.owlapi._


object ExpansionDepth {

  def restrictExpansionDepth(owlOntology: OWLOntology, level: Int): OWLOntology = {
    var ontology = OWLApiConverter.convert(owlOntology)
    ontology = restrictExpansionDepth(ontology, level)

    (new OWLExporter).toOwlOntology(ontology)
  }


  def restrictExpansionDepth(ontology: Ontology, level: Int): Ontology = {

    ontology.tbox.axioms = ontology.tbox.axioms.filterNot(_ match {
      case Subsumption(TopConcept,_) => true
      case _ => false
    })

    println("Axioms to process: "+ontology.tbox.axioms.size.toString)


    val result = new Ontology()
    var toProcess = ontology.tbox.axioms
    var resulting = Set[Axiom]()

    var depths = Map[Concept,Int]()


    def restrictExpansionDepthA(axiom: Axiom, level: Int, processed: Set[Concept]): Int = axiom match {
      case Subsumption(a,b) =>
        restrictExpansionDepthC(b, level, processed)
      case ConceptEquivalence(a, b) =>
        restrictExpansionDepthC(b, level, processed)
    }

    def axiomsFor(concept: Concept): Iterable[Axiom] = {
      val result = (toProcess++resulting).collect( _ match {
        case Subsumption(a, c) if a==concept => Subsumption(a,c)
        case ConceptEquivalence(a,c) if a==concept => ConceptEquivalence(a,c)
      })

      result
    }

    def restrictExpansionDepthC(concept: Concept, level: Int, processed: Set[Concept]): Int = {

      if(depths.contains(concept)){
        depths(concept)
      }
      concept match {
        case TopConcept => 0
        case BottomConcept => 0
        case c: BaseConcept => {
          if(processed(c)){
            println("cyclic (ignored): "+c)
            0
          }
          else {
            val values = axiomsFor(c).map{ a =>
              val value = restrictExpansionDepthA(a,level,processed+c)
              toProcess -= a
              if(value>=level){
                if(resulting(a)){
                  value
                }
                else {
                  println("Removing "+a)
                  0
                }
              } else {
                if(!resulting(a)){
                  println("Adding "+a)
                  resulting += a
                }
                value
              }
            }
            val result = if(values.isEmpty) 0 else values.max
            depths = depths.updated(c, result)
            result
          }
        }
        case ConceptComplement(c) => restrictExpansionDepthC(c, level, processed)
        case ConceptConjunction(cs) => cs.map(restrictExpansionDepthC(_, level, processed)).max
        case ConceptDisjunction(ds) => ds.map(restrictExpansionDepthC(_, level, processed)).max
        case UniversalRoleRestriction(r, c) =>
          // if(level==0)
          //   1
          // else
          restrictExpansionDepthC(c, level-1, processed)+1
        case ExistentialRoleRestriction(r, c) =>
          // if(level==0)
          //   1
          // else
          restrictExpansionDepthC(c, level-1, processed)+1
      }
    }

    val ignored = toProcess.collect(_ match {
      case Subsumption(a:BaseConcept, ConceptComplement(b: BaseConcept)) =>
        Subsumption(a,ConceptComplement(b))
    })

    println("ignored: "+ignored.size)

    toProcess --= ignored

    println("to process: "+toProcess.size)

    while(!toProcess.isEmpty){
      val axiom = toProcess.head
      toProcess = toProcess.tail

      val okay = restrictExpansionDepthA(axiom, level, Set())<level

      if(okay) {
        println("Adding "+axiom)
        resulting += axiom
      } else
        println("Removing "+axiom)
    }

    (resulting++ignored).foreach(result.addStatement)

    result
  }


  def main(args: Array[String]) = {

    if(args.size!=3){
      println("Usage: ExpansionDepth INPUTOWL LEVEL OUTPUTOWL")
    }

    val input = args(0)
    val level = args(1).toInt
    val output = args(2)

    var owlOntology = OWLApiInterface.getOWLOntology(new File(input))

    OWLOntologyFilters.restrictToALC(owlOntology)

    owlOntology = restrictExpansionDepth(owlOntology, level)

    (new OWLExporter).save(owlOntology, new File(output))
  }


}

object OntologyAnalysis2 {
  def main(args: Array[String]) = {
    val file = new File(args(0))
    val ontology = OWLApiInterface.getOWLOntology(file)

    println(file.getName + " & " +
      ontology.getTBoxAxioms(Imports.INCLUDED).size + " & " +
      ontology.getABoxAxioms(Imports.INCLUDED).size + " & " +
      ontology.getClassesInSignature(Imports.INCLUDED).size + " & " +
      ontology.getDataPropertiesInSignature(Imports.INCLUDED).size)

  }
}


object AxiomAnalysis {
  import OWLOntologyFilters._

  def main(args: Array[String]) = {
    val file = new File(args(0))

    println(file.getName)

    val ontology: OWLOntology = OWLApiInterface.getOWLOntology(file)

    analyse(ontology)
  }

  def analyse(ontology: OWLOntology) = {
    val full = ontology.getTBoxAxioms(Imports.INCLUDED).size

    val simple = (simpleAxioms(ontology).size.toDouble/full)*100
    val el = (elAxioms(ontology).size.toDouble/full)*100
    val alc = (alcAxioms(ontology).size.toDouble/full)*100
    val shq = (shqAxioms(ontology).size.toDouble/full)*100
    val alci = (alciAxioms(ontology).size.toDouble/full)*100
    val rbox = ontology.getRBoxAxioms(Imports.INCLUDED).size
    val abox = ontology.getABoxAxioms(Imports.INCLUDED).size
    val completeSize = full + ontology.getRBoxAxioms(Imports.INCLUDED).size + ontology.getABoxAxioms(Imports.INCLUDED).size


    println("Simple: %.2f%%, EL: %.2f%%, ALC: %.2f%%, ALCI: %.2f%%, SHQ: %.2f%%, axioms: %d, TBox: %d, RBox: %d, ABox: %d Signature: %d" format(
      simple, el, alc, alci, shq, full+rbox+abox, full, rbox,abox, ontology.getSignature(Imports.INCLUDED).size))
  }

  def simpleAxioms(owlOntology: OWLOntology) =
    owlOntology.getTBoxAxioms(Imports.INCLUDED).iterator().asScala.filter(alcAxiom).filter(_.getNestedClassExpressions.iterator.asScala.forall(simple))
  def elAxioms(owlOntology: OWLOntology) =
    owlOntology.getTBoxAxioms(Imports.INCLUDED).iterator().asScala.filter(alcAxiom).filter(_.getNestedClassExpressions.iterator.asScala.forall(elClass))
  def alcAxioms(owlOntology: OWLOntology) =
    owlOntology.getTBoxAxioms(Imports.INCLUDED).iterator().asScala.filter(alcAxiom).filter(_.getNestedClassExpressions.iterator.asScala.forall(alcClass))
  def shqAxioms(owlOntology: OWLOntology) =
    owlOntology.getTBoxAxioms(Imports.INCLUDED).iterator().asScala.filter(alcAxiom).filter(_.getNestedClassExpressions.iterator.asScala.forall(shqClass))
  def alciAxioms(owlOntology: OWLOntology) =
    owlOntology.getTBoxAxioms(Imports.INCLUDED).iterator().asScala.filter(alcAxiom).filter(_.getNestedClassExpressions.iterator.asScala.forall(alciClass))
}

/**
 * Filters that work on the OWL-API objects
 */
object OWLOntologyFilters {

  //  implicit val (logger,formatter,appender) =
  //  ZeroLoggerFactory.newLogger(this)
  val logger = Logger(OWLOntologyFilters.getClass)

  def alcAxiom(axiom: OWLAxiom): Boolean = axiom match {
    case _: OWLSubClassOfAxiom => true
    case _: OWLEquivalentClassesAxiom => true
    case _: OWLDisjointClassesAxiom => true
    case _: OWLDisjointUnionAxiom => true
    case _: OWLObjectPropertyDomainAxiom => true
    case _: OWLObjectPropertyRangeAxiom => true
    case _: OWLClassAssertionAxiom => true
    case _: OWLObjectPropertyAssertionAxiom => true
    case _: OWLIrreflexiveObjectPropertyAxiom => false
    case a: OWLSubClassOfAxiomShortCut => {
      val subClassAx = a.asOWLSubClassOfAxiom
      alcAxiom(subClassAx) && subClassAx.getNestedClassExpressions.iterator.asScala.forall(alcClass)
    }
    case _ => false
  }

  def alchAxiom(axiom: OWLAxiom) = axiom match {
    case ax: OWLSubObjectPropertyOfAxiom => alcRole(ax.getSubProperty) && alcRole(ax.getSuperProperty)
    case ax: OWLEquivalentObjectPropertiesAxiom => ax.getProperties.iterator.asScala.forall(alcRole)
    case _ => alcAxiom(axiom)
  }

  def alchiAxiom(axiom: OWLAxiom) = {
    alchAxiom(axiom) || axiom.isInstanceOf[OWLInverseObjectPropertiesAxiom]
  }


  def shqAxiom(axiom: OWLAxiom) = {
    alchAxiom(axiom) || axiom.isInstanceOf[OWLTransitiveObjectPropertyAxiom] || axiom.isInstanceOf[OWLFunctionalObjectPropertyAxiom]
  }

  def shiqAxiom(axiom: OWLAxiom) = {
    alchiAxiom(axiom) || axiom.isInstanceOf[OWLTransitiveObjectPropertyAxiom] || axiom.isInstanceOf[OWLFunctionalObjectPropertyAxiom]
  }

  def simple(_class: OWLClassExpression): Boolean = _class match {
    case _: OWLClass => true
    case _: OWLObjectIntersectionOf => true
    case _ => false
  }

  def elClass(_class: OWLClassExpression): Boolean = _class match {
    case _: OWLClass => true
    case _: OWLObjectIntersectionOf => true
    case c: OWLObjectSomeValuesFrom => alcRole(c.getProperty)
    case _ => false
  }

  def alcClass(_class: OWLClassExpression): Boolean = _class match {
    case _: OWLClass => true
    case _: OWLObjectComplementOf => true
    case _: OWLObjectIntersectionOf => true
    case _: OWLObjectUnionOf => true
    case c: OWLObjectSomeValuesFrom => alcRole(c.getProperty)
    case c: OWLObjectAllValuesFrom => alcRole(c.getProperty)
    case _ => false
  }

  def alciClass(_class: OWLClassExpression) = _class match {
    case _: OWLClass => true
    case _: OWLObjectComplementOf => true
    case _: OWLObjectIntersectionOf => true
    case _: OWLObjectUnionOf => true
    case c: OWLObjectSomeValuesFrom => alciRole(c.getProperty)
    case c: OWLObjectAllValuesFrom => alciRole(c.getProperty)
    case _ => false
  }

  def shqClass(_class: OWLClassExpression) = _class match {
    case c: OWLObjectMinCardinality => alcRole(c.getProperty)
    case c: OWLObjectMaxCardinality => alcRole(c.getProperty)
    case c: OWLObjectExactCardinality => alcRole(c.getProperty)
    case c => alcClass(c)
  }

  def shiqClass(_class: OWLClassExpression) = shqClass(_class) || alciClass(_class)

  def alcRole(role: OWLPropertyExpression) = role match {
    case r: OWLObjectProperty => !r.isOWLBottomObjectProperty && !r.isOWLTopObjectProperty
    case _ => false
  }

  def alciRole(role: OWLPropertyExpression): Boolean = role match {
    case _: OWLObjectProperty => true
    case r: OWLObjectInverseOf => true //alciRole(r)
    case _ => false
  }

  /**
   * Removes all non-ALC axioms from ontology.
   *
   * WARNING: changes passed argument
   *
   */
  def restrictToALC(owlOntology: OWLOntology): Unit = {
    val manager = owlOntology.getOWLOntologyManager()
    //    val manager = OWLManager.createOWLOntologyManager()

    addImportsClosure(owlOntology)

    val axioms = owlOntology.getAxioms(Imports.INCLUDED).iterator.asScala.toSet

    var nonLogical = axioms.filter(!_.isLogicalAxiom)


    val alc =
      axioms.filter(alcAxiom).filter(_.getNestedClassExpressions.iterator.asScala.forall(alcClass))

    val nonAlc = axioms -- alc

//    print(nonAlc.size+"\t"+axioms.size)


    manager.removeAxioms(owlOntology, (owlOntology.getAxioms(Imports.INCLUDED).iterator.asScala.toSet--alc).asJava)
    manager.addAxioms(owlOntology,nonLogical.asJava)
  }

  /**
   * Removes all non-ALCH axioms from ontology.
   *
   * WARNING: changes passed argument
   *
   */
  def restrictToALCH(owlOntology: OWLOntology): Unit = {
    val manager = owlOntology.getOWLOntologyManager()
    addImportsClosure(owlOntology)
    //  val manager = OWLManager.createOWLOntologyManager()

    var all = owlOntology.getAxioms(Imports.EXCLUDED).iterator().asScala.toSet

    //owlOntology.getTBoxAxioms(true)++owlOntology.getABoxAxioms(true)++owlOntology.getRBoxAxioms(true)
    var nonLogical = all.filter(!_.isLogicalAxiom)

//    manager.removeAxioms(owlOntology, nonLogical.asJava)
    all = owlOntology.getAxioms(Imports.EXCLUDED).iterator.asScala.toSet

    var nonAlch = all.filterNot(ax =>
      alchAxiom(ax) && ax.getNestedClassExpressions.iterator.asScala.forall(alcClass))
    //    print(" removing "+nonAlch.size+" ")
    logger.trace("Removing: "+nonAlch.toString)
    logger.info("Removing "+nonAlch)
    logger.info("Removing "+nonAlch.size+" non ALCH-axioms out of "+all.size+" axioms in the ontology.")
    manager.removeAxioms(owlOntology, nonAlch.asJava)
    //    assert(owlOntology.getAxiomCount==(all.size-nonAlch.size),
    //      "not same: "+owlOntology.getLogicalAxiomCount+", "+(all.size-nonAlch.size))
    logger.debug(s"Restricted with ${owlOntology}")
  
    logger.debug(s"Adding non-logical axioms again")
    manager.addAxioms(owlOntology,nonLogical.asJava)
  }

  def clearABox(owlOntology: OWLOntology): Unit = {
    val manager = OWLManager.createOWLOntologyManager()
    addImportsClosure(owlOntology)

    manager.removeAxioms(owlOntology, owlOntology.getABoxAxioms(Imports.INCLUDED))
  }

  /**
   * Removes all non-ALCHI axioms from ontology.
   *
   * WARNING: changes passed argument
   *
   */
  def restrictToALCHI(owlOntology: OWLOntology): Unit = {
    val manager = OWLManager.createOWLOntologyManager();
    addImportsClosure(owlOntology)

    val nonAlchi =
      owlOntology.getAxioms(Imports.INCLUDED).iterator.asScala.toSet.filterNot(alchiAxiom).filterNot(_.getNestedClassExpressions.iterator.asScala.forall(alciClass))

    logger.trace("Removing: "+nonAlchi.toString)

    manager.removeAxioms(owlOntology, nonAlchi.asJava) // 
    logger.debug(s"Restricted with ${owlOntology}")
  }

  def restrictToSHQ(owlOntology: OWLOntology): Unit = {
    val manager = OWLManager.createOWLOntologyManager();
    addImportsClosure(owlOntology)

    val non =
      owlOntology.getLogicalAxioms(Imports.INCLUDED).iterator.asScala.toSet.filterNot(ax => shqAxiom(ax) && ax.getNestedClassExpressions.iterator.asScala.forall(shqClass))

    logger.trace("Removing: "+non.toString)

    manager.removeAxioms(owlOntology, non.asJava) // 
    logger.debug(s"Restricted with ${owlOntology}")
  }


  def restrictToSHIQ(owlOntology: OWLOntology): Unit = {
    val manager = OWLManager.createOWLOntologyManager();
    addImportsClosure(owlOntology)

    val non =
      owlOntology.getLogicalAxioms().iterator.asScala.toSet.filterNot(ax => shiqAxiom(ax) && ax.getNestedClassExpressions.iterator.asScala.forall(shiqClass))

    logger.trace("Removing: "+non.toString)

    manager.removeAxioms(owlOntology, non.asJava) //
    logger.debug(s"Restricted with ${owlOntology}")
  }


  def removeABox(owlOntology: OWLOntology): Unit = {
    var manager = OWLManager.createOWLOntologyManager();
    addImportsClosure(owlOntology)

    manager.removeAxioms(owlOntology, owlOntology.getABoxAxioms(Imports.INCLUDED))

  }

  def addImportsClosure(ontology: OWLOntology): Unit = {
    val imports = ontology.getImports
    ontology.getImportsDeclarations.forEach(
      importDec => ontology.getOWLOntologyManager.applyChange(new RemoveImport(ontology, importDec))
    )
    imports.forEach(imp => ontology.getOWLOntologyManager.addAxioms(ontology, imp.getAxioms()))
  }

}



object TopSubsumptionFilter {
  def main(args: Array[String]) = {

    def file = new File(args(0))

    //      print(file.getName+" --> ")

    val owlOntology: OWLOntology = try{
      OWLApiInterface.getOWLOntology(file)
    } catch {
      case _: Throwable => null
    }

    if(owlOntology!=null){
      OWLOntologyFilters.restrictToALC(owlOntology)

      val ontology = OWLApiConverter.convert(owlOntology)

      ontology.tbox.axioms = ontology.tbox.axioms.filterNot(_ match {
        case Subsumption(TopConcept,_) => true
        case _ => false
      })

      (new OWLExporter).exportOntology(ontology, new File(args(1)))

    } else
      println("Exception: "+file.getName)
  }

}


object TBoxForms {
  abstract class TBoxForm

  object GENERAL_TBOX extends TBoxForm {
    override def toString = "General TBox"
  }

  case class AcyclicTBox(expansionDepth: Int) extends TBoxForm {
    override def toString = "Acyclic TBox (ExpDepth = "+expansionDepth+")"
  }

  def disjointnessAxiom(axiom: Axiom): Boolean = axiom match {
    case Subsumption(_: BaseConcept, ConceptComplement(_: BaseConcept)) => true
    case _ => false
  }

  def domainAxiom(axiom: Axiom): Boolean = axiom match {
    case Subsumption(ExistentialRoleRestriction(_, TopConcept), _) => true
    case _ => false
  }

  def tboxForm(ontology: Ontology): TBoxForm = {

    var map = new HashMap[Concept, MutSet[Concept]]() with MultiMap[Concept, Concept]

    ontology.tbox.axioms = ontology.tbox.axioms.filterNot(a => disjointnessAxiom(a)||domainAxiom(a))


    if(!ontology.tbox.axioms.forall(_ match {
      case Subsumption(a: BaseConcept,b) => {
        map.addBinding(a,b)
        true
      }
      case ConceptEquivalence(a: BaseConcept, b) => {
        map.addBinding(a,b)
        true
      }
      case Subsumption(TopConcept, b) => {
        map.addBinding(TopConcept, b)
        true
      }
      case a => println(a.toString); false
    }))
      return GENERAL_TBOX

    println("Passed test 1")

    var expDepthMap = new HashMap[BaseConcept, Int]()

    var cyclic = false

    def expDepth(concept: Concept, processed: Set[BaseConcept]): Int =
      concept match {
        case baseConcept: BaseConcept =>
          if(processed(baseConcept)){
            cyclic = true
            0 // treat later
          }
          else
            expDepthMap.get(baseConcept) match {
              case Some(value) => value
              case None => {
                val value = (map.get(baseConcept).toSet.flatten.map(expDepth(_, processed+baseConcept))+1).max
                expDepthMap.put(baseConcept, value)
                value
              }
            }
        case TopConcept => 0
        case BottomConcept => 0
        case ConceptComplement(c) => expDepth(c, processed)
        case ConceptDisjunction(ds) => ds.map(expDepth(_, processed)).max
        case ConceptConjunction(cs) => cs.map(expDepth(_, processed)).max
        case ExistentialRoleRestriction(_,c) => expDepth(c, processed)+1
        case UniversalRoleRestriction(_, c) => expDepth(c, processed)+1
      }

    map.get(TopConcept) match {
      case Some(concepts) =>
        if(concepts.flatMap(_.atomicConcepts).exists(c => map.contains(BaseConcept(c))))
          cyclic = true
      case None => ;
    }

    if(cyclic)
      return GENERAL_TBOX

    var depth = 0
    for(c <- map.keySet){
      depth = Math.max(depth, expDepth(c, Set()))
      if(cyclic)
        return GENERAL_TBOX
    }

    return AcyclicTBox(depth)
  }


  def main(args: Array[String]) = {
    var files = new File(args(0)).listFiles
    files = files.filter(f => f.getName.endsWith(".owl")||f.getName.endsWith(".obo"))
    files = files.map(_.getAbsoluteFile)

    if(args.size==2){
      val ignoreList = Source.fromFile(new File(args(1))).getLines.flatMap{ _.split(": ").toList match {
        case List(_,b) => Some(b)
        case _ => None
      }}.toSet[String]

      //     println("Ignore: "+ignoreList)
      files = files.filterNot(f => ignoreList(f.getName))
    }

    files.toSeq.sortBy(_.length).foreach{ file =>
      println("proc "+file.getName)
      //      print(file.getName+" --> ")

      val owlOntology: OWLOntology = try{
        OWLApiInterface.getOWLOntology(file)
      } catch {
        case _: Throwable => null
      }

      if(owlOntology!=null){
        OWLOntologyFilters.restrictToALC(owlOntology)

        val ontology = OWLApiConverter.convert(owlOntology)

        println(tboxForm(ontology)+": "+file.getName)
      } else
        println("Exception: "+file.getName)
    }
  }

}

object Sizes {
  def main(args: Array[String]) = {
    var files = new File(args(0)).listFiles
    files = files.filter(f => f.getName.endsWith(".owl")||f.getName.endsWith(".obo"))

    if(args.size==2){
      val ignoreList = Source.fromFile(new File(args(1))).getLines.flatMap{ _.split(": ").toList match {
        case List(b,_) => Some(b)
        case _ => None
      }}.toSet[String]

      //     println("Ignore: "+ignoreList)
      files = files.filterNot(f => ignoreList(f.getName))
    }

    var ontologies = 0
    var statements = 0L
    var signatures = 0L

    var (minSig, maxSig,  minStat, maxStat) = (0,0,0,0)

    files.toSeq.sortBy(_.length).foreach{ file =>
      print(file.getName+": ")

      val owlOntology: OWLOntology = try{
        OWLApiInterface.getOWLOntology(file)
      } catch {
        case _: Throwable => null
      }

      if(owlOntology!=null){
        OWLOntologyFilters.restrictToALC(owlOntology)

        val ontology = OWLApiConverter.convert(owlOntology)

        val sig = ontology.signature.size
        val stats = ontology.statements.size
        statements += stats
        minSig = Math.min(minSig,sig)
        maxSig = Math.max(maxSig,sig)
        minStat = Math.min(minStat,stats)
        maxStat = Math.max(maxStat, stats)
        signatures += sig
        ontologies +=1

        println(stats+" Statements, "+sig+ " symbols")
      } else
        println("Exception")
    }
    println("Signature: "+minSig+" "+maxSig+" "+(signatures.toDouble/ontologies))
    println("Statements: "+minStat+" "+maxStat+" "+(statements.toDouble/ontologies))
  }

}

object OntologyURLs {
  def main(args: Array[String]) = {
    val fileWithNames = args(0)
    val path = args(1)

    val files = Source.fromFile(new File(fileWithNames)).getLines.map(l => new File(path+"/"+l))

    files.toSeq.foreach{ file =>

      try{
        println("File: "+file.getName)
        val owlOntology = OWLApiInterface.getOWLOntology(file)
        println(owlOntology.getOntologyID)
      } catch {
        case _: Throwable => null
      }

    }
  }
}

object CheckForTransitiveRoles {
  def main(args: Array[String]) = {
    new File(args(1)).listFiles.foreach{ file =>
      if(file.getName.endsWith("owl")||file.getName.endsWith("xml")){
        val ont = OWLParser.parseFile(file)
        print(file.getName+": ")
        if(ont.rbox.axioms.exists(_.isInstanceOf[TransitiveRoleAxiom])){
          val transRoles = ont.rbox.axioms.filter(_.isInstanceOf[TransitiveRoleAxiom]).map(_.asInstanceOf[TransitiveRoleAxiom].role)
          print("Contains transitive roles: "+transRoles)
          ALCFormulaPreparations.initDefinitions()
          var count = 0
          var interesting = Set[ConceptClause]()
          var roleHierarchy = new RoleHierarchy(ont.rbox)
          ALCFormulaPreparations.clauses(ont.tbox.axioms.toSet[DLStatement]).foreach{ clause =>
            if(clause.literals.size>1 && clause.literals.exists(_.concept match {
              case UniversalRoleRestriction(r,_) if (transRoles(r) ||
                roleHierarchy.getSubRoles(r).exists(transRoles)) => true
              case MaxNumberRestriction(0,r,_) if (transRoles(r) ||
                roleHierarchy.getSubRoles(r).exists(transRoles)) => true
              case _ => false
            })){
              count += 1
              interesting += clause
            }
          }
          if(count==0)
            println(", but no interesting clause with it.")
          else{
            println(", and "+count+" interesting clauses with it.")
            interesting.foreach(println)
          }
        }
        else
          println("No transitive roles")
      }
    }
  }
}
