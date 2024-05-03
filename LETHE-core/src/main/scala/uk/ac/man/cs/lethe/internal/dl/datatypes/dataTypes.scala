package uk.ac.man.cs.lethe.internal.dl.datatypes

//import com.dongxiguo.fastring.Fastring.Implicits._


import com.typesafe.scalalogging.Logger
import uk.ac.man.cs.lethe.internal.dl.datatypes.extended.{ConjunctiveAssertion, ConjunctiveDLStatement, DisjunctiveAssertion, DisjunctiveDLStatement, GreatestFixpoint, LeastFixpoint, NegatedRoleAssertion}
import uk.ac.man.cs.lethe.internal.dl.datatypes
import uk.ac.man.cs.lethe.internal.dl.forgetting.QuickConceptForgetter
import uk.ac.man.cs.lethe.internal.dl.forgetting.abox.ABoxForgetter
import uk.ac.man.cs.lethe.internal.dl.forgetting.direct.SimpleDefinerEliminator
import uk.ac.man.cs.lethe.internal.tools.MultiSet


abstract class Expression {
  def signature: Set[String]
  def atomicConcepts: Set[String]
  def roleSymbols: Set[String]
  def roles: Set[Role] = Set()
  def size: Int
  def subConcepts: MultiSet[Concept]

  def foreachNested(function: Expression => Unit): Unit = {
    function(this)
  }
}

abstract class Concept extends Expression

object TopConcept extends Concept {
  override def toString = "TOP"
  override def signature = Set() //Set("TOP")
  override def atomicConcepts = Set()
  override def roleSymbols = Set()
  override def size = 1
  override def subConcepts = MultiSet(TopConcept)

}

object BottomConcept extends Concept {
  override def toString = "BOTTOM"
  override def signature = Set() //Set("BOTTOM")
  override def atomicConcepts = Set()
  override def roleSymbols = Set()
  override def size = 1
  override def subConcepts = MultiSet(BottomConcept)
}

trait Symbol

case class BaseConcept(name: String) extends Concept with Symbol {
  override def toString = name
  override def signature = Set(name)
  override def atomicConcepts = Set(name)
  override def roleSymbols = Set()
  override def size = 1
  override def subConcepts = MultiSet(this)
}

case class ConceptComplement(concept: Concept) extends Concept {
  override def toString = "¬"+concept.toString
  override def signature = concept.signature
  override def atomicConcepts = concept.atomicConcepts
  override def roleSymbols = concept.roleSymbols
  override def size = concept.size+1
  override def subConcepts = concept.subConcepts+this

  override def foreachNested(function: Expression => Unit) = {
    super.foreachNested(function)
    concept.foreachNested(function)
  }

  override def roles = concept.roles
}

case class ConceptConjunction(var conjuncts: Set[Concept]) extends Concept {
  override def signature = conjuncts.flatMap(_.signature)
  override def atomicConcepts = conjuncts.flatMap(_.atomicConcepts)
  override def roleSymbols = conjuncts.flatMap(_.roleSymbols)
  override def size = conjuncts.toSeq.map(_.size).sum + conjuncts.size - 1
  override def subConcepts = conjuncts.toSeq.map(_.subConcepts).foldLeft(MultiSet[Concept])(_++_) + this

  override def foreachNested(function: Expression => Unit) = {
    super.foreachNested(function)
    conjuncts.foreach(_.foreachNested(function))
  }

  override def roles = conjuncts.flatMap(_.roles)


  override def toString =
    if(conjuncts.isEmpty)
      "EMPTY_CONJ"
    else
      conjuncts.mkString("c(", " n ", ")")
}

case class ConceptDisjunction(var disjuncts: Set[Concept]) extends Concept {
  override def signature = disjuncts.flatMap(_.signature)
  override def atomicConcepts = disjuncts.flatMap(_.atomicConcepts)
  override def roleSymbols = disjuncts.flatMap(_.roleSymbols)
  override def size = disjuncts.toSeq.map(_.size).sum + disjuncts.size - 1
  override def subConcepts = disjuncts.map(_.subConcepts).foldLeft(MultiSet[Concept])(_++_) + this

  override def foreachNested(function: Expression => Unit) = {
    super.foreachNested(function)
    disjuncts.foreach(_.foreachNested(function))
  }

  override def roles = disjuncts.flatMap(_.roles)

  override def toString =
    if(disjuncts.isEmpty)
      "EMPTY_DISJ"
    else
      disjuncts.mkString("d(", " u ", ")")
}

case class NominalSet(nominals: Set[Individual]) extends Concept {

  def this(nominal: Individual) = this(Set(nominal))

  override def toString = nominals.mkString("{", ", ", "}")
  override def signature = Set()
  override def atomicConcepts = Set()
  override def roleSymbols = Set()
  override def size = nominals.size*2-1
  override def subConcepts = MultiSet(this)
}

abstract class Role extends Expression {
  override def roleSymbols = signature
  override def atomicConcepts = Set()
  override def subConcepts = MultiSet()
  override def roles = Set(this)
}

case class BaseRole(name: String) extends Role with Symbol {
  override def toString = name
  override def signature = Set(name)
  override def size = 1
}

object TopRole extends BaseRole("TOP-ROLE") {
  override def signature = Set()
  override def roleSymbols = Set()
}

case class InverseRole(role: Role) extends Role {
  override def toString = role.toString+"^-1"
  override def signature = role.signature
  override def size = role.size+1

  override def foreachNested(function: Expression => Unit) = {
    super.foreachNested(function)
    role.foreachNested(function)
  }
}

case class RoleConjunction(cs: Iterable[Role]) extends Role {
  override def toString = cs.mkString("(", " n ", ")")
  override def signature = cs.flatMap(_.signature).toSet[String]
  override def size = cs.map(_.size).reduce((a,b)=> a+b) + cs.size - 1

  override def foreachNested(function: Expression => Unit) = {
    super.foreachNested(function)
    cs.foreach(_.foreachNested(function))
  }
}

case class RoleDisjunction(ds: Iterable[Role]) extends Role {
  override def toString = ds.mkString("(", " u ", ")")
  override def signature = ds.flatMap(_.signature).toSet[String]
  override def size = ds.map(_.size).reduce((a,b)=> a+b) + ds.size - 1

  override def foreachNested(function: Expression => Unit) = {
    super.foreachNested(function)
    ds.foreach(_.foreachNested(function))
  }
}

case class RoleComplement(role: Role) extends Role {
  override def toString = "¬"+role.toString
  override def signature = role.signature
  override def size = role.size + 1

  override def foreachNested(function: Expression => Unit) = {
    super.foreachNested(function)
    role.foreachNested(function)
  }
}

abstract class RoleRestriction(val role: Role, val filler: Concept) extends Concept {
}

case class ExistentialRoleRestriction(override val role: Role,
                                      override val filler: Concept) extends RoleRestriction(role, filler) {
  override def toString = "E" + role + "." + filler
  override def signature = role.signature ++ filler.signature
  override def atomicConcepts = filler.atomicConcepts
  override def roleSymbols = role.signature ++ filler.roleSymbols
  override def size = 1 + role.size + filler.size
  override def subConcepts = filler.subConcepts + this

  override def foreachNested(function: Expression => Unit) = {
    super.foreachNested(function)
    role.foreachNested(function)
    filler.foreachNested(function)
  }

  override def roles = filler.roles + role
}

case class UniversalRoleRestriction(override val role: Role,
                                    override val filler: Concept)  extends RoleRestriction(role, filler)  {
  override def toString = "A" + role + "." + filler
  override def signature = role.signature ++ filler.signature
  override def atomicConcepts = filler.atomicConcepts
  override def roleSymbols = role.signature ++ filler.roleSymbols
  override def size = 1 + role.size + filler.size
  override def subConcepts = filler.subConcepts + this

  override def foreachNested(function: Expression => Unit) = {
    super.foreachNested(function)
    role.foreachNested(function)
    filler.foreachNested(function)
  }

  override def roles = filler.roles + role
}

case class MinNumberRestriction(number: Int, role: Role, filler: Concept) extends Concept {
  assert(number>=1)
  override def toString = ">=" + number.toString + role + "." + filler
  override def signature = role.signature ++ filler.signature
  override def atomicConcepts = filler.atomicConcepts
  override def roleSymbols = role.signature ++ filler.roleSymbols
  override def size = 1 + role.size + filler.size + number
  override def subConcepts = filler.subConcepts + this

  override def foreachNested(function: Expression => Unit) = {
    super.foreachNested(function)
    role.foreachNested(function)
    filler.foreachNested(function)
  }

  override def roles = filler.roles + role
}

case class MaxNumberRestriction(number: Int, role: Role, filler: Concept) extends Concept {
  assert(number>=0)
  override def toString = "=<" + number.toString + role + "." + filler
  override def signature = role.signature ++ filler.signature
  override def atomicConcepts = filler.atomicConcepts
  override def roleSymbols = role.signature ++ filler.roleSymbols
  override def size = 1 + role.size + filler.size + number // unary encoding
  override def subConcepts = filler.subConcepts + this

  override def foreachNested(function: Expression => Unit) = {
    super.foreachNested(function)
    role.foreachNested(function)
    filler.foreachNested(function)
  }

  override def roles = filler.roles + role
}

case class EqNumberRestriction(number: Int, role: Role, filler: Concept) extends Concept {
  assert(number>=0)
  override def toString = "=" + number.toString + role + "." + filler
  override def signature = role.signature ++ filler.signature
  override def atomicConcepts = filler.atomicConcepts
  override def roleSymbols = role.signature ++ filler.roleSymbols
  override def size = 1 + role.size + filler.size + number // unary encoding
  override def subConcepts = filler.subConcepts + this

  override def foreachNested(function: Expression => Unit) = {
    super.foreachNested(function)
    role.foreachNested(function)
    filler.foreachNested(function)
  }

  override def roles = filler.roles + role
}



abstract class DLStatement extends Expression

abstract class Axiom extends DLStatement

case class Subsumption(subsumer: Concept, subsumee: Concept) extends Axiom {
  override def toString = subsumer + " <= " + subsumee
  override def signature = subsumer.signature ++ subsumee.signature
  override def atomicConcepts = subsumer.atomicConcepts ++ subsumee.atomicConcepts
  override def roleSymbols = subsumer.roleSymbols ++ subsumee.roleSymbols
  override def size = subsumer.size + subsumee.size + 1
  override def subConcepts = subsumer.subConcepts ++ subsumee.subConcepts

  override def foreachNested(function: Expression => Unit) = {
    super.foreachNested(function)
    subsumer.foreachNested(function)
    subsumee.foreachNested(function)
  }

  override def roles = subsumer.roles ++ subsumee.roles
}

case class ConceptEquivalence(leftConcept: Concept, rightConcept: Concept) extends Axiom {
  override def toString = leftConcept + " = " + rightConcept
  override def signature = leftConcept.signature ++ rightConcept.signature
  override def atomicConcepts = leftConcept.atomicConcepts ++ rightConcept.atomicConcepts
  override def roleSymbols = leftConcept.roleSymbols ++ rightConcept.roleSymbols
  override def size = leftConcept.size + rightConcept.size + 1
  override def subConcepts = leftConcept.subConcepts ++ rightConcept.subConcepts

  override def foreachNested(function: Expression => Unit) = {
    super.foreachNested(function)
    leftConcept.foreachNested(function)
    rightConcept.foreachNested(function)
  }

  override def roles = leftConcept.roles ++ rightConcept.roles
}



case class TBox(var axioms: Set[Axiom]) extends DLStatement {
  def add(axiom: Axiom) = axioms = axioms + axiom

  override def toString = axioms.mkString("\n")
  override def signature = axioms.flatMap(_.signature).toSet[String]
  override def atomicConcepts = axioms.flatMap(_.atomicConcepts).toSet[String]
  override def roleSymbols = axioms.flatMap(_.roleSymbols).toSet[String]

  def isEmpty = axioms.isEmpty

  override def size =
    axioms.toSeq.map(_.size).foldLeft(0)((a,b) => a+b)

  override def subConcepts = axioms.map(_.subConcepts).foldLeft(MultiSet[Concept])(_++_)

  override def foreachNested(function: Expression => Unit) = {
    super.foreachNested(function)
    axioms.foreach(_.foreachNested(function))
  }

  override def roles = axioms.flatMap(_.roles)
}

abstract class RoleAxiom extends DLStatement {
  override def atomicConcepts = Set()
  override def subConcepts = MultiSet()
}

case class RoleSubsumption(subsumer: Role, subsumee: Role) extends RoleAxiom {
  override def toString = subsumer.toString+ " <= "+subsumee.toString
  override def signature = subsumer.signature ++ subsumee.signature
  override def roleSymbols = subsumer.signature ++ subsumee.signature
  override def size = subsumer.size + subsumee.size + 1

  override def foreachNested(function: Expression => Unit) = {
    super.foreachNested(function)
    subsumer.foreachNested(function)
    subsumee.foreachNested(function)
  }

  override def roles = subsumer.roles ++ subsumee.roles
}

case class TransitiveRoleAxiom(role: Role) extends RoleAxiom {
  override def toString = "trans("+role+")"
  override def signature = role.signature
  override def roleSymbols = role.signature
  override def size = role.size+1

  override def foreachNested(function: Expression => Unit) = {
    super.foreachNested(function)
    role.foreachNested(function)
  }

  override def roles = role.roles
}

case class FunctionalRoleAxiom(role: Role) extends RoleAxiom {
  override def toString = "func("+role+")"
  override def signature = role.signature
  override def roleSymbols = role.signature
  override def size = role.size+1

  override def foreachNested(function: Expression => Unit) = {
    super.foreachNested(function)
    role.foreachNested(function)
  }

  override def roles = role.roles
}


case class RBox(var axioms: Set[RoleAxiom]) extends DLStatement {
  def add(axiom: RoleAxiom) = axioms = axioms + axiom

  override def toString = axioms.mkString("\n")
  override def signature = axioms.flatMap(_.signature).toSet[String]
  override def roleSymbols = axioms.flatMap(_.roleSymbols).toSet[String]
  override def atomicConcepts = Set()
  override def subConcepts = MultiSet()

  def isEmpty = axioms.isEmpty

  override def size =
    axioms.toSeq.map(_.size).foldLeft(0)((a,b) => a+b)

  override def foreachNested(function: Expression => Unit) = {
    super.foreachNested(function)
    axioms.foreach(_.foreachNested(function))
  }

  override def roles = axioms.flatMap(_.roles)
}

case class Individual(name: String) extends Expression {
  override def toString = name

  override def signature = Set()
  override def atomicConcepts = Set()
  override def roleSymbols = Set()
  override def size = 1
  override def subConcepts = MultiSet()
}


abstract class Assertion extends DLStatement

case class ConceptAssertion(concept: Concept, individual: Individual) extends Assertion {
  override def toString = concept.toString+"("+individual.toString+")"
  override def signature = concept.signature ++ individual.signature
  override def atomicConcepts = concept.atomicConcepts ++ individual.atomicConcepts
  override def roleSymbols = concept.roleSymbols ++ individual.roleSymbols
  override def size = concept.size + individual.size
  override def subConcepts = concept.subConcepts ++ individual.subConcepts

  override def foreachNested(function: Expression => Unit) = {
    super.foreachNested(function)
    concept.foreachNested(function)
    individual.foreachNested(function)
  }

  override def roles = concept.roles
}

/**
 * For the results of ABox forgetting
 */
case class DisjunctiveConceptAssertion(cas: Set[ConceptAssertion]) extends Assertion {
  override def toString = cas.mkString(" v ")
  override def signature = cas.flatMap(_.signature)
  override def atomicConcepts = cas.flatMap(_.atomicConcepts)
  override def roleSymbols = cas.flatMap(_.roleSymbols)
  override def size = cas.toSeq.map(_.size).sum + cas.size-1
  assert(cas.size>0, "unsupported dca: "+toString+" inconsistent ontology?")
  override def subConcepts = cas.map(_.subConcepts).toSet[MultiSet[Concept]].foldLeft(MultiSet[Concept])(_++_)

  override def foreachNested(function: Expression => Unit) = {
    super.foreachNested(function)
    cas.foreach(_.foreachNested(function))
  }

  override def roles = cas.flatMap(_.roles)
}

case class RoleAssertion(role: Role, individual1: Individual, individual2: Individual) extends Assertion {
  override def toString = role.toString+"("+individual1+", "+individual2+")"
  override def signature = role.signature
  override def atomicConcepts = Set()
  override def subConcepts = MultiSet()
  override def roleSymbols = role.signature
  override def size = role.size + 2

  override def foreachNested(function: Expression => Unit) = {
    super.foreachNested(function)
    role.foreachNested(function)
  }

  override def roles = role.roles
}

case class ABox(var assertions: Set[Assertion]) extends DLStatement {
  override def toString = assertions.mkString("\n")
  override def signature = assertions.flatMap(_.signature)
  override def atomicConcepts = assertions.flatMap(_.atomicConcepts)
  override def roleSymbols = assertions.flatMap(_.roleSymbols)
  override def subConcepts = assertions.map(_.subConcepts).foldLeft(MultiSet[Concept])(_++_)

  def isEmpty = assertions.isEmpty

  override def size =
    assertions.toSeq.map(_.size).foldLeft(0)((a,b) => a+b)

  override def foreachNested(function: Expression => Unit) = {
    super.foreachNested(function)
    assertions.foreach(_.foreachNested(function))
  }

  override def roles = assertions.flatMap(_.roles)
}

object Ontology {
  def buildFrom(statements: Iterable[DLStatement]): Ontology = {
    val result = new Ontology()

    statements.flatMap(_ match {
      case ConjunctiveDLStatement(statements) => statements.toSet[DLStatement]
      case ConjunctiveAssertion(assertions) => assertions.toSet[DLStatement]
      case other => Set(other)
    }).foreach(result.addStatement)

    result
  }
}

class Ontology(var tbox:TBox = new TBox(Set()),
               var abox: ABox = new ABox(Set()),
               var rbox: RBox = new RBox(Set())) extends DLStatement {

  def this(tbox: TBox, abox: ABox) = this(tbox, abox, new RBox(Set()))

  def this() = this(new TBox(Set()), new ABox(Set()))

  def isEmpty = tbox.isEmpty && abox.isEmpty && rbox.isEmpty


  def addStatement(statement: DLStatement) = statement match {
    case a: Assertion => abox.assertions = abox.assertions + a
    case a: Axiom => tbox.axioms = tbox.axioms + a
    case a: RoleAxiom => rbox.axioms = rbox.axioms + a
  }

  def addStatements(statements: Iterable[DLStatement]) =
    statements.foreach(addStatement)

  def statements: Iterable[DLStatement] = tbox.axioms ++ rbox.axioms ++ abox.assertions

  override def toString = "TBox:\n" + tbox.toString + "\n\nRBox:\n" + rbox.toString+"\n\n"+ "\n\nABox:\n" + abox.toString+"\n\n"
  override def signature = tbox.signature ++ rbox.signature ++ abox.signature
  override def atomicConcepts = tbox.atomicConcepts ++ abox.atomicConcepts
  override def roleSymbols = tbox.roleSymbols ++ rbox.roleSymbols ++ abox.roleSymbols
  override def subConcepts = tbox.subConcepts ++ rbox.subConcepts ++ abox.subConcepts

  override def foreachNested(function: Expression => Unit) = {
    super.foreachNested(function)
    tbox.foreachNested(function)
    abox.foreachNested(function)
    rbox.foreachNested(function)
  }

  override def roles = tbox.roles ++ rbox.roles ++ abox.roles

  override def equals(other: Any) = other match {
    case other: Ontology =>    tbox==other.tbox && abox==other.abox && rbox==other.rbox
    case _ => false
  }

  override def hashCode =
    13*tbox.hashCode+abox.hashCode+rbox.hashCode

  def size: Int = tbox.size+abox.size+rbox.size


  def remove(dlStatement: DLStatement) = dlStatement match {
    case a: Axiom => tbox.axioms -= a
    case a: Assertion => abox.assertions -= a
    case ra: RoleAxiom => rbox.axioms -= ra
  }
}

// Convenience methods for certain axiom shortforms 
object DLHelpers {

  //implicit val (logger, formatter, appender) = ZeroLoggerFactory.newLogger(this)
  //import formatter._
  val logger = Logger(DLHelpers.getClass)

  def nnf(ontology: Ontology): Ontology =
    new Ontology(tbox = nnf(ontology.tbox),
      abox = nnf(ontology.abox),
      rbox = ontology.rbox)

  def nnf(tbox: TBox): TBox = new TBox(tbox.axioms.flatMap(nnf))

  def nnf(axiom: Axiom): Set[Subsumption] = axiom match {
    case ConceptEquivalence(a, b) => nnf(Subsumption(a,b)) ++ nnf(Subsumption(b,a))
    case Subsumption(a, b) =>
      Set(Subsumption(TopConcept, nnf(ConceptDisjunction(Set(ConceptComplement(a),
        b)))))
  }

  def nnf(abox: ABox): ABox = new ABox(abox.assertions.flatMap(nnf))

  def nnf(assertion: Assertion): Set[Assertion] = assertion match {
    case _: RoleAssertion => Set(assertion)
    case ConceptAssertion(c, a) => Set(ConceptAssertion(nnf(c), a))
    case DisjunctiveConceptAssertion(cas) =>
      Set(DisjunctiveConceptAssertion(cas.flatMap(ca => nnf(ca).map(_.asInstanceOf[ConceptAssertion]))))
  }


  def nnf(concept: Concept): Concept = concept match {
    case ConceptComplement(TopConcept) => BottomConcept
    case ConceptComplement(BottomConcept) => TopConcept
    case ConceptComplement(ExistentialRoleRestriction(r, f)) =>
      UniversalRoleRestriction(r, nnf(ConceptComplement(f)))
    case ConceptComplement(UniversalRoleRestriction(r, f)) =>
      ExistentialRoleRestriction(r, nnf(ConceptComplement(f)))
    case ConceptComplement(MinNumberRestriction(n, r, f)) =>
      MaxNumberRestriction(n-1, r, ConceptComplement(nnf(ConceptComplement(f))))
    case ConceptComplement(MaxNumberRestriction(n, r, f)) =>
      MinNumberRestriction(n+1, r, nnf(f))
    case ConceptComplement(ConceptConjunction(cs)) =>
      ConceptDisjunction(cs.map(f => nnf(ConceptComplement(f))))
    case ConceptComplement(ConceptDisjunction(ds)) =>
      ConceptConjunction(ds.map(f => nnf(ConceptComplement(f))))
    case ConceptComplement(GreatestFixpoint(v,c)) =>
      LeastFixpoint(v, nnf(
        QuickConceptForgetter.replaceCBy(c, Set(v), ConceptComplement(v))
      ))
    case ConceptComplement(LeastFixpoint(v,c)) =>
      GreatestFixpoint(v, nnf(
        QuickConceptForgetter.replaceCBy(c, Set(v), ConceptComplement(v))
      ))
    case ConceptComplement(ConceptComplement(f)) => nnf(f)
    case ConceptComplement(f) => ConceptComplement(nnf(f))
    case ExistentialRoleRestriction(r, f) => ExistentialRoleRestriction(r, nnf(f))
    case UniversalRoleRestriction(r, f) => UniversalRoleRestriction(r, nnf(f))
    case MinNumberRestriction(n, r, f) => MinNumberRestriction(n, r, nnf(f))
    case MaxNumberRestriction(n, r, f) =>
      MaxNumberRestriction(n, r, ConceptComplement(nnf(ConceptComplement(f))))
    case GreatestFixpoint(v,c) => GreatestFixpoint(v,nnf(c))
    case LeastFixpoint(v,c) => LeastFixpoint(v,nnf(c))
      // special case of one conjunct/disjunct w
    case ConceptConjunction(cs) if cs.size==1 => cs.head
    case ConceptDisjunction(ds) if ds.size==1 => ds.head

      // the following two cases are sound due to the previous two
    case ConceptConjunction(cs) => ConceptConjunction(cs.map(nnf)-TopConcept)
    case ConceptDisjunction(ds) => ConceptDisjunction(ds.map(nnf)-BottomConcept)
    case b: BaseConcept => b
    case TopConcept => TopConcept
    case BottomConcept => BottomConcept
    case _: NominalSet => concept
  }

  def disjoint(c1: Concept, c2: Concept) =
    new Subsumption(ConceptConjunction(Set(c1, c2)), BottomConcept)
  //    new Subsumption(c1, new ConceptComplement(c2))



  def neg(concept: Concept): Concept = concept match {
    case TopConcept => BottomConcept
    case BottomConcept => TopConcept
    case c: BaseConcept => ConceptComplement(c)
    case ConceptComplement(f) => f
    case ConceptDisjunction(fs) => ConceptConjunction(fs.map(neg))
    case ConceptConjunction(fs) => ConceptDisjunction(fs.map(neg))
    case UniversalRoleRestriction(r, f) => ExistentialRoleRestriction(r, neg(f))
    case ExistentialRoleRestriction(r, f) => UniversalRoleRestriction(r, neg(f))
    case MinNumberRestriction(n, r, f) => MaxNumberRestriction(n-1, r, f)
    case MaxNumberRestriction(n, r, f) => MinNumberRestriction(n+1, r, f)
    case GreatestFixpoint(v,c) =>
      LeastFixpoint(v, neg(
        QuickConceptForgetter.replaceCBy(c, Set(v), ConceptComplement(v))
      ))
    case LeastFixpoint(v,c) =>
      LeastFixpoint(v, neg(
        QuickConceptForgetter.replaceCBy(c, Set(v), ConceptComplement(v))
      ))
    case f: NominalSet => ConceptComplement(f);
  }

  def conjunction(concepts: Iterable[Concept]): Concept = {

    if(concepts.exists(c1 => concepts.exists(c2 => c1==neg(c2))))
      return BottomConcept
    if(concepts.exists(_==BottomConcept))
      return BottomConcept

    var set = concepts.toSet[Concept] - TopConcept
    if(set.isEmpty)
      TopConcept
    else if(set.size==1)
      set.head
    else {
      // flatten
      while(set.exists(_.isInstanceOf[ConceptConjunction])) {
        val nested = (set.collectFirst{ case c:ConceptConjunction => c }).get.asInstanceOf[ConceptConjunction]
        set -= nested
        set ++= nested.conjuncts
      }

      ConceptConjunction(set)
    }
  }

  def conjunction(roles: Iterable[Role]): Role = {
    if(roles.size==1)
      roles.head
    else if(roles.size>1)
      new RoleConjunction(roles)
    else {
      assert(false, "Not implemented yet!")
      null
    }
  }


  def disjunction(c1: Concept, c2: Concept): Concept =
    disjunction(Set(c1, c2))

  def disjunction(concepts: Iterable[Concept]): Concept = {
    if(concepts.toSet(TopConcept))
      return TopConcept
    if(concepts.exists(c1 => concepts.exists(c2 => c1==neg(c2))))
      return TopConcept

    var set = concepts.toSet[Concept] - BottomConcept
    if(set.isEmpty)
      BottomConcept
    else if(set.size==1)
      set.head
    else {
      //flatten
      while(set.exists(_.isInstanceOf[ConceptDisjunction])) {
        val nested = (set.collectFirst{ case c:ConceptDisjunction => c }).get.asInstanceOf[ConceptDisjunction]
        set -= nested
        set ++= nested.disjuncts
      }

      ConceptDisjunction(set)
    }
  }  // <---- looks correct

  def disjunction(roles: Iterable[Role]): Role = {
    if(roles.size==1)
      roles.head
    else if(roles.size>1)
      new RoleDisjunction(roles)
    else {
      assert(false, "Not implemented yet!")
      null
    }
  }

  def inverse(role: Role): Role = role match {
    case r: BaseRole => InverseRole(r)
    case InverseRole(r: BaseRole) => r
    case InverseRole(InverseRole(r)) => inverse(r)
    case _ => assert(false, "complex roles not supported"); null
  }

  def inverseOf(role1: Role, role2: Role): Boolean =
    inverse(role1)==inverse(inverse(role2))

  // General simplifications
  def simplify(ont: Ontology): Ontology = {
    logger.info(s"Simplifying ontology of ${ont.statements.size} statements.")
    new Ontology(simplify(ont.tbox), simplify(ont.abox), ont.rbox)
  }
  def simplify(tbox: TBox): TBox = {
    var axioms = tbox.axioms.map(simplify)
    axioms = axioms.filterNot(_ match {
      case Subsumption(BottomConcept, _) => true
      case Subsumption(_, TopConcept) => true
      case _ => false
    })
    new TBox(axioms)
  }
  def simplify(abox: ABox): ABox = new ABox(abox.assertions.map(simplify))
  def simplify(axiom: Axiom): Axiom = {
    logger.trace(s"Simplifying ${axiom}")
    axiom match {
      case Subsumption(c1, c2) => Subsumption(simplify(c1), simplify(c2))
      case ConceptEquivalence(c1, c2) => ConceptEquivalence(simplify(c1), simplify(c2)) match {
        case ConceptEquivalence(TopConcept, c) => Subsumption(TopConcept, c)
        case ConceptEquivalence(c, TopConcept) => Subsumption(TopConcept, c)
        case ConceptEquivalence(BottomConcept, c) => Subsumption(c, BottomConcept)
        case ConceptEquivalence(c, BottomConcept) => Subsumption(c, BottomConcept)
        case ce => ce
      }
    }
  }

  def simplify(assertion: Assertion): Assertion = assertion match {
    case ConceptAssertion(c, i) => ConceptAssertion(simplify(c), i)
    case DisjunctiveConceptAssertion(ds) => DisjunctiveConceptAssertion(ds.map((simplify(_).asInstanceOf[ConceptAssertion])))
    case r: RoleAssertion => r
  }

  def simplify(concept: Concept): Concept = concept match {
    case ConceptComplement(c) => neg(simplify(c))
    case ConceptConjunction(cs) => {
      if(cs.size==1)
        simplify(cs.head)
      else if(cs.isEmpty)
        BottomConcept
      else {
        val csX = cs.filter{ _ match {
          case ConceptDisjunction(ds) => !ds.exists(cs) // redundant then
          case _ => true
        }}
        if(csX.forall(d => d.isInstanceOf[ConceptDisjunction])){
          // (A u B u C) n (A u B u D) => (A u B u (C n D)), if reasonable

          val diss = csX.map(_.asInstanceOf[ConceptDisjunction])
          var overlap = diss.toSeq(0).disjuncts
          diss.foreach { dis =>
            overlap = overlap.filter(dis.disjuncts.contains)
          }
          if(overlap.isEmpty)
            conjunction(csX.map(simplify))
          else disjunction(overlap.map(simplify) + conjunction(
            diss.map(dis => simplify(ConceptDisjunction(dis.disjuncts--overlap)))))
        }
        else
          conjunction(csX.map(simplify))
      }
    }
    case ConceptDisjunction(ds) => {
      if(ds.size==1)
        simplify(ds.head)
      else
        disjunction(ds.map(simplify))
    }
    case UniversalRoleRestriction(r1, ExistentialRoleRestriction(r2, c)) if inverseOf(r1, r2) =>
      simplify(c)

    case ExistentialRoleRestriction(r, c) if r.equals(TopRole) =>
      val filler = simplify(c)
      if(filler.equals(TopConcept))
        TopConcept
      else if(filler.equals(BottomConcept))
        BottomConcept
      else
        ExistentialRoleRestriction(r, filler)
    case ExistentialRoleRestriction(r, c) =>
      val filler = simplify(c)
      if(filler==BottomConcept)
        BottomConcept
      else
        ExistentialRoleRestriction(r, filler)

    case UniversalRoleRestriction(r, c) =>
      val filler = simplify(c)
      if(filler==TopConcept)
        TopConcept
      else
        UniversalRoleRestriction(r, filler)
    case MinNumberRestriction(1, r, c) => simplify(ExistentialRoleRestriction(r,c))
    case MaxNumberRestriction(0, r, ConceptComplement(c)) => simplify(UniversalRoleRestriction(r,c))
    case MinNumberRestriction(n, r, c) => {
      val filler = simplify(c)
      if(filler==BottomConcept)
        BottomConcept
      else
        MinNumberRestriction(n, r, filler)
    }
    case MaxNumberRestriction(n, r, c) => {
      val filler = simplify(c)
      if(filler==BottomConcept)
        TopConcept
      else
        MaxNumberRestriction(n, r, simplify(c))
    }
    case c => c
  }

  /**
   * Splits subsumptions of the kind C <= (C1 n C2) to subsumptions C <= C1, C <= C2
   */
  def splitConjunctions(ontology: Ontology) = {
    val result = new Ontology(abox=ontology.abox, rbox=ontology.rbox)
    val axioms = ontology.tbox.axioms.flatMap{_ match {
      case Subsumption(c, ConceptConjunction(cs)) => cs.map(Subsumption(c,_)).toSet[Axiom]
      case other => Set(other)
    }}
    result.tbox = TBox(axioms)
    result
  }
}

object CheapSimplifier {

  var expensive = false

  def simplify(ontology: Ontology): Ontology = {
    new Ontology(tbox = new TBox(ontology.tbox.axioms.map(simplify)),
      abox = new ABox(ontology.abox.assertions.map(simplify)),
      rbox = ontology.rbox)
  }

  def simplify(statement: DLStatement): DLStatement = statement match {
    case axiom: Axiom => simplify(axiom)
    case assertion: Assertion => simplify(assertion)
    case DisjunctiveDLStatement(statements) =>
      val simplified = statements.map(simplify)
      if(simplified.size==1)
        simplified.head
      else
        DisjunctiveDLStatement(simplified)
    case ConjunctiveDLStatement(statements) =>
      val simplified = statements.map(simplify)
      if(simplified.size==1)
        simplified.head
      else
        ConjunctiveDLStatement(simplified)
    case _ => statement
  }

  def simplify(axiom: Axiom): Axiom = {
    axiom match {
      case Subsumption(a, b) => Subsumption(simplify(a), simplify(b))
      case ConceptEquivalence(a, b) => ConceptEquivalence(simplify(a), simplify(b))
    }
  }

  def simplify(assertion: Assertion): Assertion = assertion match {

    case ConceptAssertion(c, i) =>
      simplify(c) match {
        case ExistentialRoleRestriction(r: BaseRole, NominalSet(nominals)) if nominals.size==1 =>
          RoleAssertion(r, i, nominals.head)

        case UniversalRoleRestriction(r: BaseRole, ConceptComplement(NominalSet(nominals))) if nominals.size==1 =>
          NegatedRoleAssertion(r, i, nominals.head)

        case ExistentialRoleRestriction(InverseRole(r: BaseRole), NominalSet(nominals)) if nominals.size==1 =>
          RoleAssertion(r, nominals.head, i)

        case UniversalRoleRestriction(InverseRole(r: BaseRole), ConceptComplement(NominalSet(nominals))) if nominals.size==1 =>
          NegatedRoleAssertion(r, nominals.head, i)

        case other =>
          ConceptAssertion(other, i)

      }
      ConceptAssertion(simplify(c), i)
    case DisjunctiveConceptAssertion(cas) => {
      var cas2 = cas.map(simplify(_).asInstanceOf[ConceptAssertion])
      cas2 = cas2.filterNot(_.concept==BottomConcept)
      DisjunctiveConceptAssertion(cas2)
    }
    case DisjunctiveAssertion(cas) => {
      var cas2 = cas
        .map(simplify(_)).filterNot(_ match {
        case ConceptAssertion(BottomConcept,_) => true
        case other => false
      })
      if(cas2.exists(_ match {
        case ConceptAssertion(TopConcept, _) => true
        case _ => false
      }))
        ConceptAssertion(TopConcept, Individual("a"))
      else
        DisjunctiveAssertion(cas2)
    }
    case other => other
  }

  def tautologicalDisjunction(disjuncts: Set[Concept]) = {
    disjuncts.exists(_ match {
      case TopConcept => true
      case UniversalRoleRestriction(role,_) =>
        disjuncts(ExistentialRoleRestriction(role,TopConcept))
/*      case ExistentialRoleRestriction(role, TopConcept) => disjuncts.exists(_ match {
        case UniversalRoleRestriction(role2, _) if role.equals(role2) => true
        case _ => false
      })*/
      case _ => false
    }) ||
      (//expensive &&
        disjuncts.exists(d => disjuncts(ConceptComplement(d)))
        )
  }

  def contradictoryConjunction(conjuncts: Set[Concept]) = {
    conjuncts(BottomConcept) ||
      (//expensive &&
        conjuncts.exists(d => conjuncts(ConceptComplement(d)))
        )
  }

  def simplify(concept: Concept): Concept = concept match {
    case TopConcept => TopConcept
    case BottomConcept => BottomConcept
    case b :BaseConcept => b
    case ns: NominalSet => ns
    case ConceptComplement(c) => c match {
      case TopConcept => BottomConcept
      case BottomConcept => TopConcept
      case a: BaseConcept => ConceptComplement(a)
      case ns: NominalSet => ConceptComplement(ns)
      case ConceptComplement(c) => c
      case UniversalRoleRestriction(r, d) =>
        simplify(ExistentialRoleRestriction(r, ConceptComplement(d)))
      case ExistentialRoleRestriction(r, d) =>
        simplify(UniversalRoleRestriction(r, ConceptComplement(d)))
      case ConceptDisjunction(ds) =>
        simplify(ConceptConjunction(ds.map(ConceptComplement)))
      case ConceptConjunction(cs) =>
        simplify(ConceptDisjunction(cs.map(ConceptComplement)))
      case MinNumberRestriction(n,r,c) =>
        simplify(MaxNumberRestriction(n-1, r, c))
      case MaxNumberRestriction(n,r,c) =>
        simplify(MinNumberRestriction(n+1, r, c))
      case LeastFixpoint(variable, c) => GreatestFixpoint(variable,
        simplify(
          ConceptComplement(
            QuickConceptForgetter.replaceCBy(c, Set(variable), ConceptComplement(variable)))))
      case GreatestFixpoint(variable, c) => LeastFixpoint(variable,
        simplify(
          ConceptComplement(
            QuickConceptForgetter.replaceCBy(c, Set(variable), ConceptComplement(variable)))))
    }
    // the following commented simplification would be unsound
    // case UniversalRoleRestriction(r1, ExistentialRoleRestriction(r2, c)) if DLHelpers.inverseOf(r1, r2) =>
    //  simplify(c)
    case UniversalRoleRestriction(r, d) => {
      simplify(d) match {
        case TopConcept => TopConcept
        //	case ConceptConjunction(cs) => ConceptConjunction(cs.map(UniversalRoleRestriction(r,_)))
        case d2 => UniversalRoleRestriction(r, d2)
      }}
    case ExistentialRoleRestriction(r, d) if r.equals(TopRole) => { simplify(d) match {
      case BottomConcept => BottomConcept
      case TopConcept => TopConcept
      //      case ConceptDisjunction(ds) => ConceptDisjunction(ds.map(ExistentialRoleRestriction(r,_)))
      case d2 => ExistentialRoleRestriction(r, d2)
    } }
    case ExistentialRoleRestriction(r, d) => { simplify(d) match {
      case BottomConcept => BottomConcept
      //      case ConceptDisjunction(ds) => ConceptDisjunction(ds.map(ExistentialRoleRestriction(r,_)))
      case d2 => ExistentialRoleRestriction(r, d2)
    } }
    case ConceptDisjunction(ds) => {
      var ds2 = ds.map(simplify)
      ds2 = ds2.flatMap(_ match {
        case ConceptDisjunction(ds) => ds
        case BottomConcept => Set[Concept]()
        case c => Set(c)
      })
      if(ds2.isEmpty)
        BottomConcept
      else if(ds2.size==1)
        ds2.head
      else if(tautologicalDisjunction(ds2))//if(ds2.contains(TopConcept))
        TopConcept
      else
        ConceptDisjunction(ds2)
    }
    case ConceptConjunction(cs) => {
      var cs2 = cs.map(simplify)
      cs2 = cs2.flatMap(_ match {
        case ConceptConjunction(cs) => cs
        case TopConcept => Set[Concept]()
        case c => Set(c)
      })
      if(cs2.isEmpty)
        TopConcept
      else if(cs2.size==1)
        cs2.head
      else if(contradictoryConjunction(cs2))
        BottomConcept
      else
        ConceptConjunction(cs2)
    }

    case MinNumberRestriction(1,r,c) => simplify(ExistentialRoleRestriction(r,c))
    case MaxNumberRestriction(0,r,c) => simplify(UniversalRoleRestriction(r, ConceptComplement(c)))

    case MinNumberRestriction(n,r,c) => simplify(c) match {
      case BottomConcept => BottomConcept
      case c2 => MinNumberRestriction(n,r,c2)
    }
    case MaxNumberRestriction(n,r,c) => simplify(c) match {
      case BottomConcept => TopConcept
      case c2 => MaxNumberRestriction(n,r,c2)
    }

    case _: NominalSet => concept

    case GreatestFixpoint(variable, concept) => GreatestFixpoint(variable, simplify(concept))
    case LeastFixpoint(variable, concept) => LeastFixpoint(variable, simplify(concept))
  }}


object OntologyFilter {

  def restrictToSH(ontology: Ontology): Ontology =
    new Ontology(
      tbox = new TBox(ontology.tbox.axioms.map(toALC).flatten),
      abox = new ABox(ontology.abox.assertions.map(toALC).flatten),
      rbox = new RBox(ontology.rbox.axioms.map(toSH).flatten))


  def restrictToSHQ(ontology: Ontology): Ontology = {
    new Ontology(tbox=new TBox(ontology.tbox.axioms.map(toSHQ).flatten),
      abox=new ABox(ontology.abox.assertions.map(toSHQ).flatten),
      rbox=new RBox(ontology.rbox.axioms.map(toSH).flatten))
  }

  def toSHQ(axiom: Axiom): Option[Axiom] = axiom match {
    case Subsumption(a,b) => toSHQ(a).flatMap(a2 => toSHQ(b).map(b2 => Subsumption(a2,b2)))
    case ConceptEquivalence(a,b) =>
      toSHQ(a).flatMap(a2 => toSHQ(b).map(b2 => ConceptEquivalence(a2,b2)))
  }

  def toSH(axiom: RoleAxiom): Set[RoleAxiom] = axiom match {
    case RoleSubsumption(r1: BaseRole, r2: BaseRole) => Set(axiom)
    case TransitiveRoleAxiom(_: BaseRole) => Set(axiom)
    case _=> Set()
  }

  def toSHQ(assertion: Assertion): Option[Assertion] = assertion match {
    case ConceptAssertion(c,a) => toSHQ(c).map(ConceptAssertion(_,a))
    case RoleAssertion(r: BaseRole, _, _) => Some(assertion)
  }

  def toSHQ(concept: Concept): Option[Concept] = concept match {
    case TopConcept => Some(TopConcept)
    case BottomConcept => Some(BottomConcept)
    case _: BaseConcept => Some(concept)
    case ConceptComplement(c) => toSHQ(c).map(ConceptComplement)
    case ConceptDisjunction(ds) => {
      val ds2 = ds.map(toSHQ)
      if(!ds2.contains(None))
        Some(ConceptDisjunction(ds2.flatten))
      else
        None
    }
    case ConceptConjunction(cs) => {
      val cs2 = cs.map(toSHQ)
      if(!cs2.contains(None))
        Some(ConceptConjunction(cs2.flatten))
      else
        None
    }
    case ExistentialRoleRestriction(r: BaseRole, c) => toSHQ(c).map(ExistentialRoleRestriction(r,_))
    case UniversalRoleRestriction(r: BaseRole, c) => toSHQ(c).map(UniversalRoleRestriction(r,_))
    case MinNumberRestriction(n, r: BaseRole, d) => toSHQ(d).map(MinNumberRestriction(n,r,_))
    case MaxNumberRestriction(n, r: BaseRole, d) => toSHQ(d).map(MaxNumberRestriction(n,r,_))
    case _ => None
  }



  def restrictToSHI(ontology: Ontology): Ontology = {
    new Ontology(tbox=new TBox(ontology.tbox.axioms.map(toALCI).flatten),
      abox=new ABox(ontology.abox.assertions.map(toALCI).flatten),
      rbox=new RBox(ontology.rbox.axioms.map(toSHI).flatten))
  }


  def restrictToALCH(ontology: Ontology): Ontology = {
    new Ontology(tbox=new TBox(ontology.tbox.axioms.map(toALC).flatten),
      abox=new ABox(ontology.abox.assertions.map(toALC).flatten),
      rbox=new RBox(ontology.rbox.axioms.map(toALCH).flatten))
  }

  def restrictToALCHI(ontology: Ontology): Ontology = {
    new Ontology(tbox=new TBox(ontology.tbox.axioms.map(toALCI).flatten),
      abox=new ABox(ontology.abox.assertions.map(toALCI).flatten),
      rbox=new RBox(ontology.rbox.axioms.map(toALCHI).flatten))
  }

  def restrictToALC(ontology: Ontology): Ontology = {
    new Ontology(tbox=new TBox(ontology.tbox.axioms.map(toALC).flatten),
      abox=new ABox(ontology.abox.assertions.map(toALC).flatten),
      rbox=new RBox(Set()))
  }

  def restrictToALCI(ontology: Ontology): Ontology = {
    new Ontology(tbox=new TBox(ontology.tbox.axioms.map(toALCI).flatten),
      abox=new ABox(ontology.abox.assertions.map(toALCI).flatten),
      rbox=new RBox(Set()))
  }

  def toALC(axiom: Axiom): Option[Axiom] = axiom match {
    case Subsumption(a, b) => toALC(a).flatMap(a2 => toALC(b).map(b2 => Subsumption(a2,b2)))
    case ConceptEquivalence(a,b) =>
      toALC(a).flatMap(a2 => toALC(b).map(b2 => ConceptEquivalence(a2,b2)))
    case _ => None
  }

  def toALCI(axiom: Axiom): Option[Axiom] = axiom match {
    case Subsumption(a, b) => toALCI(a).flatMap(a2 => toALCI(b).map(b2 => Subsumption(a2,b2)))
    case ConceptEquivalence(a,b) =>
      toALCI(a).flatMap(a2 => toALCI(b).map(b2 => ConceptEquivalence(a2,b2)))
    case _ => None
  }

  def toALCH(axiom: RoleAxiom): Option[RoleAxiom] = axiom match {
    case RoleSubsumption(r1: BaseRole, r2: BaseRole) => Some(axiom)
    case _=> None
  }

  def toALCHI(axiom: RoleAxiom): Option[RoleAxiom] = axiom match {
    case RoleSubsumption(r1, r2) if inALCI(r1) && inALCI(r2) => Some(axiom)
    case _=> None
  }

  def toSHI(axiom: RoleAxiom): Option[RoleAxiom] = axiom match {
    case _: TransitiveRoleAxiom => Some(axiom)
    case _ => toALCHI(axiom)
  }

  def toALC(assertion: Assertion): Option[Assertion] = assertion match {
    case ConceptAssertion(c, a) => toALC(c).map(ConceptAssertion(_, a))
    case RoleAssertion(_: BaseRole, _, _) => Some(assertion)
    case _ => None
  }

  def toALCI(assertion: Assertion): Option[Assertion] = assertion match {
    case ConceptAssertion(c, a) => toALCI(c).map(ConceptAssertion(_, a))
    case a: RoleAssertion => Some(a)
    case _ => None
  }

  def toALC(concept: Concept): Option[Concept] = { concept match {
    case TopConcept => Some(TopConcept)
    case BottomConcept => Some(BottomConcept)
    case b: BaseConcept => Some(b)
    case ConceptComplement(c) => toALC(c).map(ConceptComplement)
    case ConceptDisjunction(ds) => {
      val ds2 = ds.map(toALC)
      if(ds2.contains(None))
        None
      else
        Some(ConceptDisjunction(ds2.flatten))
    }
    case ConceptConjunction(cs) => {
      val cs2 = cs.map(toALC)
      if(cs2.contains(None))
        None
      else
        Some(ConceptConjunction(cs2.flatten))
    }
    case ExistentialRoleRestriction(r: BaseRole, c) => toALC(c).map(ExistentialRoleRestriction(r,_))
    case UniversalRoleRestriction(r: BaseRole, c) => toALC(c).map(UniversalRoleRestriction(r,_))
    case _ => None
  }}

  def toALCI(concept: Concept): Option[Concept] = concept match {
    case TopConcept => Some(TopConcept)
    case BottomConcept => Some(BottomConcept)
    case _: BaseConcept => Some(concept)
    case ConceptComplement(c) => toALCI(c).map(ConceptComplement)
    case ConceptDisjunction(ds) => {
      val ds2 = ds.map(toALCI)
      if(!ds2.contains(None))
        Some(ConceptDisjunction(ds2.flatten))
      else
        None
    }
    case ConceptConjunction(cs) => {
      val cs2 = cs.map(toALCI)
      if(!cs2.contains(None))
        Some(ConceptConjunction(cs2.flatten))
      else
        None
    }
    case ExistentialRoleRestriction(r, c) if inALCI(r) => toALCI(c).map(ExistentialRoleRestriction(r,_))
    case UniversalRoleRestriction(r, c) if inALCI(r) => toALC(c).map(UniversalRoleRestriction(r,_))
    case _ => None
  }

  def inALCI(role: Role): Boolean = role match {
    case _: BaseRole => true
    case InverseRole(r) => inALCI(r)
    case _ => false
  }
}

object OntologyBeautifier {

  var expensive = false

  val logger = Logger(OntologyBeautifier.getClass)

  import DLHelpers._
  import uk.ac.man.cs.lethe.internal.dl.forgetting.direct.ALCFormulaPreparations

  def makeNice(ontology: Ontology): Unit = {
    val nnfTBox = nnf(ontology.tbox)
    ontology.tbox = new TBox(Set())
    ontology.addStatements(nnfTBox.axioms.map(nice))
    ontology.tbox.axioms = ontology.tbox.axioms.filterNot(_ match {
      case Subsumption(ConceptConjunction(cs), concept) if cs.contains(concept) => true
      case Subsumption(concept, ConceptDisjunction(ds)) if ds.contains(concept) => true
      case Subsumption(ConceptConjunction(cs), ConceptDisjunction(ds)) if ds.exists(cs) => true
      case Subsumption(_, TopConcept) => true
      case Subsumption(c1, c2) if c1 == c2 => true
      case _ => false
    })
    ontology.abox.assertions = ontology.abox.assertions.map(CheapSimplifier.simplify)
    ontology.abox.assertions = ontology.abox.assertions.filterNot(_ match {
      case ConceptAssertion(TopConcept, individual) => true
      case other => false
    })
  }

  def nice(axiom: DLStatement): DLStatement = {
    logger.trace(s"entering nice")
    logger.trace(s"before cheap simplify: ${axiom}")
    val simplified = CheapSimplifier.simplify(axiom)
    logger.trace(s"after cheap simplify: ${simplified}")
    simplified match {
      case Subsumption(TopConcept, UniversalRoleRestriction(TopRole, c)) =>
        nice(Subsumption(TopConcept, c))
      case Subsumption(TopConcept, ConceptDisjunction(ds)) if ds.size == 1 => {
        logger.trace(s"one disjunct: ${simplified}")
        nice(Subsumption(TopConcept, ds.head))
      }
      case Subsumption(TopConcept, ConceptDisjunction(ds)) => {
        logger.trace(s"more disjuncts: ${simplified}")
        ds.collectFirst {
          case NominalSet(nominals) if nominals.size == 1 =>
            val individual = nominals.head
            CheapSimplifier.simplify(
              ConceptAssertion(ConceptDisjunction(ds - NominalSet(nominals)), individual))
        } match {
          case Some(assertion) => assertion
          case None =>
            logger.trace(s"disjuncts: ${ds}")
            val negDisjuncts = negPart(ds)
            val posDisjuncts = posPart(ds)

            logger.trace(s"neg part: ${negDisjuncts}")
            logger.trace(s"pos part: ${posDisjuncts}")
            val lhs = CheapSimplifier.simplify(neg(ConceptDisjunction(negDisjuncts)))
            val rhs = CheapSimplifier.simplify(ConceptDisjunction(posDisjuncts))
            logger.trace(s"lhs becomes: ${lhs}")
            logger.trace(s"rhs becomes: ${rhs}")
            Subsumption(lhs,rhs)
        }
      }
      case Subsumption(a: BaseConcept, definition) if ALCFormulaPreparations.isDefiner(a) => //
        Subsumption(a, CheapSimplifier.simplify(definition))

      case Subsumption(ConceptConjunction(cs), ConceptDisjunction(ds)) =>
        //      Subsumption(neg(ConceptDisjunction(negPart(Set(b) + neg(a)))),
        //		  ConceptDisjunction(posPart(Set(b) + neg(a))))

        Subsumption(
          CheapSimplifier.simplify(
            ConceptConjunction(posPart(cs) ++ negPart(ds).map(neg))),
          CheapSimplifier.simplify(
            ConceptDisjunction(negPart(cs).map(neg) ++ posPart(ds)))
        )
      //nice(Subsumption(a, ConceptDisjunction(Set(b))))

      case Subsumption(ConceptConjunction(cs), b: Concept) =>
        Subsumption(
          CheapSimplifier.simplify(
            ConceptConjunction(posPart(cs + ConceptComplement(b)))),
          CheapSimplifier.simplify(
            ConceptDisjunction(negPart(cs + ConceptComplement(b)).map(neg)))
        )
      //nice(Subsumption(a, ConceptDisjunction(Set(b))))


      case Subsumption(a: Concept, ConceptDisjunction(ds)) =>
        Subsumption(CheapSimplifier.simplify(
          neg(ConceptDisjunction(negPart(ds + ConceptComplement(a))))),
          CheapSimplifier.simplify(
            ConceptDisjunction(posPart(ds +
              ConceptComplement(a)
            ))
          )
        )
      case Subsumption(a, b) =>
        //      Subsumption(neg(ConceptDisjunction(negPart(Set(b) + neg(a)))),
        //		  ConceptDisjunction(posPart(Set(b) + neg(a))))

        Subsumption(
          CheapSimplifier.simplify(
            neg(ConceptDisjunction(
              negPart(Set(b) + ConceptComplement(a))))),
          CheapSimplifier.simplify(ConceptDisjunction(
            posPart(Set(b) + ConceptComplement(a)))
          ))
      //nice(Subsumption(a, ConceptDisjunction(Set(b))))


      case _ => axiom
    }
  }


  def negPart(concepts: Set[Concept]) = concepts.filter(isNeg)
  def posPart(concepts: Set[Concept]) = concepts.filterNot(isNeg)

  def isNeg(concept: Concept): Boolean = concept match {
    case ConceptComplement(c) => !isNeg(c)
    case BottomConcept => true
    case ConceptDisjunction(ds) if !expensive => ds.forall(isNeg)
    case ConceptDisjunction(ds) if expensive => ds.filter(isNeg).size>(ds.size*.5)
    case ConceptConjunction(cs) if !expensive => cs.forall(isNeg)
    case ConceptConjunction(cs) if expensive => cs.filter(isNeg).size>(cs.size*.5)
    case UniversalRoleRestriction(_,c) => isNeg(c)
    case ExistentialRoleRestriction(_,c) => isNeg(c)
    case MinNumberRestriction(_,_,c) => isNeg(c)
    case MaxNumberRestriction(_,_,c) => isNeg(c)
    case _ => false
  }
} 
