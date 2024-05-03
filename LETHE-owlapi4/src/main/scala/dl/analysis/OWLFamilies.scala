package uk.ac.man.cs.lethe.internal.dl.filters

import java.io.File
import org.semanticweb.owlapi.model.OWLOntology
import uk.ac.man.cs.lethe.internal.dl.datatypes.{BaseConcept, BaseRole, BottomConcept, Concept, ConceptComplement, ConceptConjunction, ConceptDisjunction, ConceptEquivalence, ExistentialRoleRestriction, InverseRole, MaxNumberRestriction, MinNumberRestriction, Ontology, Role, RoleSubsumption, Subsumption, TopConcept, TransitiveRoleAxiom, UniversalRoleRestriction}
import uk.ac.man.cs.lethe.internal.dl.filters.OWLFamilies.Extras.H
import uk.ac.man.cs.lethe.internal.dl.owlapi.{OWLApiConverter, OWLApiInterface}

import scala.collection.JavaConverters._
import scala.runtime.java8.JFunction0$mcZ$sp

object OWLFamilies {
  trait Family

  // object BaseFamily extends Enumeration  {
  //   type BaseFamily = Value

  //   val SIMPLEST = Value("SIMPLEST")
  //   val EL = Value("EL")
  //   val FL = Value("FL")
  //   val AL = Value("AL")
  //   val ALU = Value("ALU")
  //   val ALE = Value("ALE")
  //   val ALC = Value("ALC")
  // }
  // import BaseFamily._

  object Operator extends Enumeration {
    type Operator = Value
    val ATOMIC_COMPL, CONJ, DISJ, UNIV, EXIS, QUAL_EXIS, COMPLEX_COMPL, ROLE_SUB, INV_ROLE, TRANS, CARD, UNKNOWN = Value
  }

  import Operator._

  case class BaseFamily(operators: Set[Operator]) extends Family

  object SIMPLEST extends BaseFamily(Set()) { override def toString="SIMPLEST" }
  object EL extends BaseFamily(Set(ATOMIC_COMPL, CONJ, EXIS, QUAL_EXIS)) { override def toString="EL" }
  object FLzero extends BaseFamily(Set(CONJ, UNIV)) { override def toString="FL" }
  object AL extends BaseFamily(Set(ATOMIC_COMPL, CONJ, UNIV, EXIS)) { override def toString="AL" }
  object ALU extends BaseFamily(Set(ATOMIC_COMPL, CONJ, UNIV, EXIS, DISJ)) { override def toString="ALU" }
  object ALE extends BaseFamily(Set(ATOMIC_COMPL, CONJ, UNIV, EXIS, QUAL_EXIS)) { override def toString="ALE" }
  object ALC extends BaseFamily(Set(ATOMIC_COMPL, CONJ, DISJ, UNIV, EXIS, QUAL_EXIS, COMPLEX_COMPL)) { override def toString="ALC" }
  object S extends BaseFamily(Set(ATOMIC_COMPL, CONJ, DISJ, UNIV, EXIS, QUAL_EXIS, COMPLEX_COMPL, TRANS)) { override def toString="S" }
  object MORE extends BaseFamily(Operator.values) { override def toString=">SHIQ" }

  /**
   * ASSUMPTION: sorted by expressivity
   */
  val baseFamilies = List(SIMPLEST,EL,FLzero,AL,ALU,ALE,ALC,S,MORE)

  object Extras extends Enumeration {
    type Extras = Value
    val H = Value("H")
    val I = Value("I")
    val Q = Value("Q")
  }
  import Extras._

  case class ExtFamily(baseFamily: BaseFamily, extras: Set[Extras]) extends Family {
    override def toString = baseFamily.toString+extras.toSeq.sorted.mkString("")
  }


  //  val MOST_EXPRESSIVE = ExtFamily(ALC, Set(H,I))
  val MOST_EXPRESSIVE = ExtFamily(S, Set(H, I, Q))

  /**
   * checks whether f2 is at least as expressive as f1. Note that this is not a total order - it is possible that
   * smallerThan(f1,f2)=smallerThan(f2,f1)=false. Note also that smallerThan(a,a)=true
   */
  def smallerThan(f1: Family, f2: Family): Boolean = (f1,f2) match {
    case(BaseFamily(o1), BaseFamily(o2)) => o1.forall(o2)
    case(b1: BaseFamily, ExtFamily(b2,_)) => smallerThan(b1,b2)
    case(ExtFamily(b1,extras),b2: BaseFamily) => extras.isEmpty && smallerThan(b1,b2)
    case(ExtFamily(b1,e1), ExtFamily(b2,e2)) => smallerThan(b1,b2) && e1.forall(e2)
    case other => throw new IllegalArgumentException("Unsupported families: "+f1+", "+f2)
  }

  def withinFragment(ontology: Ontology, givenFamily: Family) =
    smallerThan(family(ontology), givenFamily)

  def family(ontology: Ontology): Family = {
    var result: Family = AL

    var axioms = ontology.tbox.axioms.toSeq
    var ops = Set[Operator]()

    if(!ontology.rbox.axioms.isEmpty){
      ops += ROLE_SUB
      ontology.rbox.axioms.foreach{ _ match{
        case RoleSubsumption(r1, r2) => ops ++= operators(r1)++operators(r2)
        case TransitiveRoleAxiom(r) => ops ++= Set(TRANS)++operators(r)
        case r => ops += UNKNOWN
      }}
    }

    while(!Operator.values.forall(ops) && !axioms.isEmpty){
      val axiom = axioms.head
      axioms = axioms.tail

      val (c1, c2) = axiom match {
        case Subsumption(a,b) => (a,b)
        case ConceptEquivalence(a,b) => (a,b)
      }
      ops ++= operators(c1)++operators(c2)


      result = family(ops)
    }

    result
  }

  def family(ops: Set[Operator]): Family = {
    if(ops.isEmpty)
      return SIMPLEST

    var extras = Set[Extras]()

    if(ops.contains(UNKNOWN))
      return MORE

    if(ops.contains(ROLE_SUB))
      extras += H
    if(ops.contains(INV_ROLE))
      extras += I
    if(ops.contains(CARD))
      extras += Q

    val baseOps = ops--Set(INV_ROLE, ROLE_SUB, CARD)

    /* IMPORTANT: we assume here that baseFamilies is sorted by expressivity */
    val base = baseFamilies.find{f: BaseFamily => baseOps.forall(f.operators)} match {
      case Some(base) => base
      case None =>
        assert(baseOps.forall(Set(ATOMIC_COMPL, CONJ, DISJ, UNIV, EXIS, QUAL_EXIS, COMPLEX_COMPL, TRANS)))
        S
    }
 /*   var base = if(baseOps.forall(Set(DISJ, UNIV, EXIS, QUAL_EXIS)))
      FL
    else if(baseOps.forall(Set(ATOMIC_COMPL, CONJ, EXIS, QUAL_EXIS)))
      EL
    else if(baseOps.forall(Set(ATOMIC_COMPL, CONJ, UNIV, EXIS)))
      AL
    else if(baseOps.forall(Set(ATOMIC_COMPL, CONJ, UNIV, EXIS, DISJ)))
      ALU
    else if(baseOps.forall(Set(ATOMIC_COMPL, CONJ, UNIV, EXIS, QUAL_EXIS)))
      ALE
    else if(baseOps.forall(Set(ATOMIC_COMPL, CONJ, DISJ, UNIV, EXIS, QUAL_EXIS, COMPLEX_COMPL)))
      ALC
    else //if(baseOps.forall(Set(ATOMIC_COMPL, CONJ, DISJ, UNIV, EXIS, QUAL_EXIS, COMPLEX_COMPL, TRANS)))
    {
      assert(baseOps.forall(Set(ATOMIC_COMPL, CONJ, DISJ, UNIV, EXIS, QUAL_EXIS, COMPLEX_COMPL, TRANS)))
      S
    }*/

    ExtFamily(base,extras)
  }

  def operators(c: Concept): Set[Operator] = c match {
    case TopConcept => Set(DISJ, ATOMIC_COMPL)
    case BottomConcept => Set(CONJ, ATOMIC_COMPL)
    case _:BaseConcept => Set()
    case ConceptComplement(b: BaseConcept) => Set(ATOMIC_COMPL)
    case ConceptComplement(c) => operators(c) + COMPLEX_COMPL
    case ConceptDisjunction(ds) => Set(DISJ)++ds.flatMap(operators)
    case ConceptConjunction(cs) => Set(CONJ)++cs.flatMap(operators)
    case UniversalRoleRestriction(r: Role, c) => Set(UNIV) ++ operators(c) ++ operators(r)
    case ExistentialRoleRestriction(r: Role, TopConcept) => Set(EXIS) ++ operators(r)
    case ExistentialRoleRestriction(r: Role, c) => Set(QUAL_EXIS) ++ operators(c) ++ operators(r)
    case MinNumberRestriction(_, r, c) => Set(CARD) ++ operators(c) ++ operators(r)
    case MaxNumberRestriction(_, r, c) => Set(CARD) ++ operators(c) ++ operators(r)
    case _ => Set(UNKNOWN) //assert(false, "not supported: "+c); Set()
  }

  def operators(r: Role): Set[Operator] = r match {
    case _: BaseRole => Set()
    case InverseRole(r) => Set(INV_ROLE)++operators(r)
    case _ => Set(UNKNOWN) // assert(false, "not supported: "+r); Set()
  }

  def main(args: Array[String]) = {
    var files = new File(args(0)).listFiles
    files = files.filter(f => f.getName.endsWith(".owl")||f.getName.endsWith(".obo"))
    files = files.map(_.getAbsoluteFile)

    files.toSeq.sorted.foreach {  file =>
      print(file.getName)
      try{
        val owlOntology: OWLOntology = OWLApiInterface.getOWLOntology(file)
        val full = owlOntology.getLogicalAxioms().iterator.asScala.size
        print(" Full: "+full+", ")
        print(" Family: " + family(OWLApiConverter.convert(owlOntology)))
        OWLOntologyFilters.restrictToSHIQ(owlOntology)
        if (full>owlOntology.getLogicalAxioms().iterator.asScala.size)
          print("+X")
        println(", ")
        OWLOntologyFilters.restrictToSHQ(owlOntology)
        val shq = owlOntology.getLogicalAxioms().iterator.asScala.size
        println(" SHQ: "+shq+ " ("+(shq.toDouble/full*100)+"%)")
      } catch {
        case c: Throwable => println(" Exception.")
      }

    }

    // if(args.size==2){
    //   val ignoreList = Source.fromFile(new File(args(1))).getLines.map(_.split(": ")(1)).toSet[String]
    //   files = files.filterNot(f => ignoreList(f.getName))
    // }

    // files.toSeq.sorted.foreach{ file =>
    //   println("Trying "+file.getName)
    //   val owlOntology: OWLOntology = try{
    // 	OWLApiInterface.getOWLOntology(file)
    //   } catch {
    // 	case _: Throwable => null
    //   }

    //   if(owlOntology!=null){

    // 	OWLOntologyFilters.restrictToALCHI(owlOntology)

    // 	val ontology = OWLApiConverter.convert(owlOntology)

    // 	println(family(ontology)+": "+file.getName)
    //   }
    // }
  }
}