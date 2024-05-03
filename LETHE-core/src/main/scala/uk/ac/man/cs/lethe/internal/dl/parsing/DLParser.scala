package uk.ac.man.cs.lethe.internal.dl.parsing

import java.io.File
import java.net.URL

import scala.io.Source
import scala.util.parsing.combinator.syntactical._
import scala.util.parsing.combinator.RegexParsers
import uk.ac.man.cs.lethe.internal.dl.datatypes._
import uk.ac.man.cs.lethe.internal.tools.formatting.SimpleDLFormatter

object DL2OWL {

//  import uk.ac.man.cs.lethe.internal.dl.owlapi._

  def main(args: Array[String]) = {

    val ontology = DLParser.parse(new File(args(0)))

    println(SimpleDLFormatter.format(ontology))

    //val exporter = new OWLExporter()

   // exporter.exportOntology(ontology, new File(args(0)+".owl"))
  }

}


object DLParser extends RegexParsers { 

  def parse(url: URL): Ontology = { 
    val result = new Ontology()

    assert(url!=null)

    Source.fromURL(url).getLines().foreach(line => 
      if(!line.matches("""(\s*|//.*)""") )
	result.addStatement(parseDLStatement(line)))

    result
  }

  def parse(file: File): Ontology = {
    val result = new Ontology()
    
    Source.fromFile(file, "iso-8859-1").getLines().foreach(line => 
      if(!line.matches("""(\s*|//.*|#.*|\*\*.*)""") )
	result.addStatement(parseDLStatement(line)))

    result
  }

  def parse(string: String): Ontology = { 
    val result = new Ontology()
    
    string.split("\n").foreach{ line => result.addStatement(parseDLStatement(line))}
    
    result
  }

  def parseConcept(input: String): Concept  = parseAll(concept, input) match { 
  // def parseConcept(string: String): Concept = phrase(concept)(new lexical.Scanner(string)) match { 
    case Success(c, _) => c
    case Failure(m, remaining) => throw new RuntimeException(m+"\n"+remaining.pos.longString)
    case Error(m, remaining) => throw new RuntimeException(m+"\n"+remaining.pos.longString)
  }
  
  def parseDLStatement(input: String): DLStatement = parseAll(dlStatement, input) match { 
    case Success(s, _) => s
    case Failure(m, remaining) => throw new RuntimeException(m+"\n"+remaining.pos.longString)
    case Error(m, remaining) => throw new RuntimeException(m+"\n"+remaining.pos.longString)
  }

  val CONCEPT_NAME = """\w+'*(?!\.)"""r
  val ROLE_NAME = """\w+'*"""r
  val IND_NAME = """[a-z0-9_]+'*""".r

  val INT = """\d+"""r

  def dlStatement: Parser[DLStatement] = 
    subsumption | equivalence | roleSubsumption | disjunctiveConceptAssertion| roleAssertion | functionalRoleAxiom | transitiveRoleAxiom
  
  def subsumption: Parser[DLStatement] = 
    concept ~ "<=" ~ concept ^^ { case a ~ _ ~ b => Subsumption(a, b) }
  
  def equivalence: Parser[DLStatement] = 
    concept ~ "=" ~ concept ^^ { case a ~ _ ~ b => ConceptEquivalence(a, b) } 

  def roleSubsumption: Parser[DLStatement] = 
    role ~ "<r" ~ role ^^ { case r1 ~ _ ~ r2 => RoleSubsumption(r1,r2) }


  def conceptAssertion: Parser[DLStatement] = 
    concept ~ "(" ~ IND_NAME ~ ")" ^^ { case c ~ _ ~ ind ~ _ => ConceptAssertion(c, Individual(ind)) }

  def roleAssertion: Parser[DLStatement] = 
    role ~ "(" ~ IND_NAME ~ ", " ~ IND_NAME ~ ")" ^^ { 
      case r ~ _ ~ a ~ _ ~ b ~ _ => RoleAssertion(r, Individual(a), Individual(b)) }

  // in its current form, the following causes parsing problems
  def disjunctiveConceptAssertion: Parser[DLStatement] = 
    rep1sep(conceptAssertion, "v") ^^ {
      case List(onlyOne) => onlyOne
      case (cas: List[DLStatement]) =>
        DisjunctiveConceptAssertion(cas.map(_.asInstanceOf[ConceptAssertion]).toSet)
    }

  def functionalRoleAxiom: Parser[DLStatement] =
    "func(" ~ role ~ ")" ^^ {
      case _ ~ r ~ _ => FunctionalRoleAxiom(r)
    }

  def transitiveRoleAxiom: Parser[DLStatement] =
    "trans(" ~ role ~ ")" ^^ {
      case _ ~ r ~ _ => TransitiveRoleAxiom(r)
    }


  def concept: Parser[Concept] = 
    disjunction | conjunction | conceptComplement | existentialRestriction | universalRestriction | minNumberRestriction | maxNumberRestriction | eqNumberRestriction | constant | baseConcept


  def constant: Parser[Concept] =
    ("TOP" | "BOTTOM") ^^ { case "TOP" => TopConcept
			    case "BOTTOM" => BottomConcept }


  def baseConcept: Parser[Concept] = 
    CONCEPT_NAME ^^ { s => BaseConcept(s)}

  def conceptComplement: Parser[Concept] = 
    """(-|¬)""".r ~> concept ^^ { case c => ConceptComplement(c) }

  def disjunction: Parser[Concept] = 
    "(" ~> repsep(concept, "u") <~ ")" ^^ { (ds: List[Concept]) => ConceptDisjunction(ds.toSet[Concept])}

  def conjunction: Parser[Concept] = 
    "(" ~> repsep(concept, "n") <~ ")" ^^ { (cs: List[Concept]) => ConceptConjunction(cs.toSet[Concept])}




  def existentialRestriction: Parser[Concept] = 
    "E"  ~> role ~ "." ~ concept ^^ { case r ~ _ ~ c => ExistentialRoleRestriction(r, c) }
  
  def universalRestriction: Parser[Concept] = 
    "A" ~> role ~ "." ~ concept ^^ { case r ~ _ ~ c => UniversalRoleRestriction(r, c) }


  def eqNumberRestriction: Parser[Concept] =
    "=" ~> INT ~ role ~ "." ~ concept ^^ {case n ~ r ~ _ ~ c => EqNumberRestriction(n.toInt, r, c) }

  def minNumberRestriction: Parser[Concept] = 
    ">=" ~> INT ~ role ~ "." ~ concept ^^ { case n ~ r ~ _ ~ c => MinNumberRestriction(n.toInt, r, c) }

  def maxNumberRestriction: Parser[Concept] = 
    "=<" ~> INT ~ role ~ "." ~ concept ^^ { case n ~ r ~ _ ~ c => MaxNumberRestriction(n.toInt, r, c) }


  def role: Parser[Role] = 
    roleInverse2 | baseRole | roleInverse | roleConjunction | roleDisjunction | roleComplement
  
  def baseRole: Parser[Role] = 
    ROLE_NAME ^^ { case s => BaseRole(s) }

  def roleInverse : Parser[Role] = 
    "(" ~> role <~ ")^-1" ^^ { case r => InverseRole(r) }

  def roleInverse2 : Parser[Role] =
    "inv(" ~> role <~ ")" ^^ { case r => InverseRole(r) }

  def roleConjunction: Parser[Role] = 
    "(" ~> repsep(role, "n") <~ ")" ^^ { case rs: List[Role] => RoleConjunction(rs.toSet[Role]) }

  def roleDisjunction: Parser[Role] = 
    "(" ~> repsep(role, "u") <~ ")" ^^ { case rs: List[Role] => RoleDisjunction(rs.toSet[Role]) }

  def roleComplement: Parser[Role] = 
    "¬" ~ role ^^ { case _ ~ r => RoleComplement(r) }

}
