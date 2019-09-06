package parser

import scala.util.parsing.combinator.RegexParsers
import binaryarray.BinaryArray
import brm.BooleanRetrievalModel

/*
 * Class to parse an entrant query based on the following grammar:
 *  Expr ::= Term ("OR" Term)*
 *  Term ::= Factor ("AND" Factor)*
 *  Factor ::= ["NOT"] (Word | "(" Expr ")")
 *  Word ::= [a-zA-Z] // Regex(\w)
 *
 * Uses the RegexParsers library to parse the entry String.
 */
class QueryParser(brm: BooleanRetrievalModel) extends RegexParsers {
  // When the parser find a word, return its binary representation
  private def word: Parser[BinaryArray] =
    """[a-zA-z]+""".r ^^ (brm.convertToBinaryArray(_))

  private def factor: Parser[BinaryArray] =
    opt("not") ~ (word | "(" ~> expr <~ ")") ^^ {
      case Some(_) ~ res =>
        ~res // If there's a not, return the NOT value of the result
      case None ~ res => res // Else return the result
    }

  private def term: Parser[BinaryArray] = factor ~ rep("and" ~> factor) ^^ {
    case factor ~ list =>
      list.foldLeft(factor) {
        case (x, y) => x & y // Make the AND operator of the factors
      }
  }

  def expr: Parser[BinaryArray] = term ~ rep("or" ~> term) ^^ {
    case expr ~ list =>
      list.foldLeft(expr) {
        case (x, y) => x | y // Make the OR operator of the terms
      }
  }

}
