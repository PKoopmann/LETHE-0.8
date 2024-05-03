package uk.ac.man.cs.lethe.internal.tools.formatting

import uk.ac.man.cs.lethe.internal.dl.datatypes.extended.{ConceptVariable, ConjunctiveAssertion, ConjunctiveDLStatement, DisjunctiveAssertion, DisjunctiveDLStatement, GreatestFixpoint, LeastFixpoint, NegatedRoleAssertion}
import uk.ac.man.cs.lethe.internal.dl.datatypes._


trait Formatter[A] { 
  def format(a: A): String
}



object SimpleDLFormatter extends SimpleDLFormatterCl

class SimpleDLFormatterCl extends Formatter[Expression] { 
//  def main(args: Array[String]) = {
//    val file = new File(args(0))
//    val ontology = OWLParser.parseFile(file)
//
//    println(format(ontology))
//  }

  def format(ontology: Ontology): String = { 
        val tboxAxioms = ontology.tbox.axioms.map(_ match { 
      case ConceptEquivalence(c: Concept, b: BaseConcept) => ConceptEquivalence(b, c)
      case c => c
    })

    var statements = " TBox: \n"
    statements += tboxAxioms.toSeq.map(format).sorted.mkString("\n")
    statements += "\n\n RBox: \n" 
    statements += ontology.rbox.axioms.toSeq.map(format).sorted.mkString("\n")
    statements += "\n\n ABox: \n" 
    statements += ontology.abox.assertions.toSeq.map(format).sorted.mkString("\n")

    statements
  }

  override def format(stat: Expression) = stat match { 
    case TopConcept => ""+TOP
    case BottomConcept => ""+BOT
    case BaseConcept(name) => getName(name) 
    case ConceptComplement(c) => NEG + format(c)
    case ConceptConjunction(cs) => cs.map(format).mkString("(", " "+SQ_AND+" ", ")")
    case ConceptDisjunction(cs) => cs.map(format).mkString("(", " "+SQ_OR+" ", ")")
    case Individual(name) => name.split('#').last
    case NominalSet(ns) => ns.map(format).mkString("{", ", ", "}")

    case TopRole => ""+TOP_ROLE
    case BaseRole(name) => name.split('#').last
    case InverseRole(r) => format(r)+INVERSE
    case RoleConjunction(cs) => cs.map(format).mkString("(", " "+SQ_AND+" ", ")")
    case RoleDisjunction(cs) => cs.map(format).mkString("(", " "+SQ_OR+" ", ")")
    case RoleComplement(c) => NEG + format(c)

    case ExistentialRoleRestriction(r, c) => EXISTS + format(r) + "." + format(c)
    case UniversalRoleRestriction(r, c) => FORALL + format(r) + "." + format(c)
    case MinNumberRestriction(n, r, c) => GEQ + n.toString + format(r) + "." + format(c)
    case MaxNumberRestriction(n, r, c) => LEQ + n.toString + format(r) + "." + format(c)

    case Subsumption(c, d) => format(c) + " " + SQ_SUBSETEQ + " " + format(d)
    case ConceptEquivalence(c, d) => format(c) + " " + SQ_EQUIV + " " + format(d)
    
    case RoleSubsumption(r, q) => format(r) + " " + SQ_SUBSETEQ + " " + format(q)
    case TransitiveRoleAxiom(r) => "trans(" + format(r) + ")"

    case ConceptAssertion(c, i) => format(c) + "(" + format(i) + ")"
    case RoleAssertion(r, i, j) => format(r) + "(" + format(i) + ", " + format(j) + ")"
    case DisjunctiveConceptAssertion(ds) => ds.map(format).mkString(" "+VEE+" ")

    // statements added for abduction
    case DisjunctiveAssertion(assertions) => assertions.map(format).mkString(" "+VEE+" ")
    case ConjunctiveAssertion(assertions) => assertions.map(format).mkString("(", " "+WEDGE+" ", ")")
    case NegatedRoleAssertion(r, a, b) => NEG+format(RoleAssertion(r,a,b))
    case DisjunctiveDLStatement(statements) =>
      if(statements.isEmpty)
        BOT.toString
      else
        statements.map(format).mkString(" "+VEE+" ")
    case ConjunctiveDLStatement(statements) =>
      if(statements.isEmpty)
        TOP.toString
      else
        statements.map(format).mkString("(", " "+WEDGE+" ", ")")

    // fixpoint stuff
    case variable:ConceptVariable => variable.name
    case GreatestFixpoint(variable, concept) => NU+format(variable)+".["+format(concept)+"]"
    case LeastFixpoint(variable, concept) => MU+format(variable)+".["+format(concept)+"]"
  }

  def getName(iri: String) = { 
    iri.split('#').last
  }

  // Symbols
  val TOP = 0x22A4.toChar
  val TOP_ROLE = 0x2207.toChar
  val BOT = 0x22A5.toChar
  val NEG = 0x00AC.toChar

  val SQ_AND = 0x2293.toChar
  val SQ_OR = 0x2294.toChar
  val EXISTS = 0x2203.toChar
  val FORALL = 0x2200.toChar
  val INVERSE = 0x207B.toChar
  val LEQ = 0x2A7D.toChar
  val GEQ = 0x2A7E.toChar

  val SQ_SUBSETEQ = 0x2291.toChar
  val SQ_EQUIV = 0x2261.toChar

  val VEE = 0x2228.toChar
  val WEDGE = 0x2227.toChar

  val NU = 0x03BD.toChar
  val MU = 0x03BC.toChar
} 
