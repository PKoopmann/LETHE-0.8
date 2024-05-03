package uk.ac.man.cs.lethe.internal.dl.forgetting.direct


import scala.util.parsing.combinator.syntactical._
import scala.util.parsing.combinator.RegexParsers

import uk.ac.man.cs.lethe.internal.dl.datatypes._
import uk.ac.man.cs.lethe.internal.dl.parsing.DLParser


object ConceptClauseParser extends RegexParsers { 

  def parseConceptClauses(input: String): Set[ConceptClause] = parse(conceptClauseSet, input) match { 
    case Success(result, _) => result
    case Failure(m, remaining) => throw new RuntimeException(m+"\n"+remaining.pos.longString)
    case Error(m, remaining) => throw new RuntimeException(m+"\n"+remaining.pos.longString)
  }


  val string = """[^,\]]+"""r

  override def skipWhitespace = false


  def conceptClauseSet: Parser[Set[ConceptClause]] =
    repsep(conceptClause, """\s"""r) ^^ { x => x.toSet } 

  def conceptClause: Parser[ConceptClause] =
    "[" ~> repsep(conceptLiteral, ", ") <~ "]" ^^ { case literals => new ConceptClause(literals) }

  def conceptLiteral: Parser[ConceptLiteral] = 
    polarity ~ string ^^ { case polarity ~ string => ConceptLiteral(polarity, DLParser.parseConcept(string)) }

  def polarity: Parser[Boolean] = 
    ("-" | "+") ^^ { case "-" => false
		    case "+" => true }
}
