package days

import scala.util.matching.Regex
import scala.util.parsing.combinator._
import scala.language.postfixOps
class PuzzleLine(lowerBound: Int, upperBound: Int, letter: String, word: String) {
  override def toString: String = {
    s"${letter} must appear ${lowerBound}-${upperBound} times in ${word}"
  }
}

object PuzzleLineParser extends RegexParsers {
  override val whiteSpace: Regex = """[ \t]+""".r

  private def boundNum: Parser[Int] = """(\d+)""".r ^^ {
    _.toInt
  }

  private def char: Parser[String] = """[a-z]""".r <~ ":" ^^ {
    letter => letter }

  private def word: Parser[String] = """[a-z]+""".r ^^ { _.toString }

  private def bounds: Parser[(Int, Int)] = boundNum ~ "-" ~ boundNum ^^ {
    case a ~ _ ~ b =>
      (a,b)
  }

  val eol: Regex = """[\r?\n]+""".r

  private def puzzleLine: Parser[PuzzleLine] = opt(eol) ~> bounds ~ char ~ word <~ opt(eol) ^^ {
    case (a,b) ~ letter ~ word => new PuzzleLine(a, b, letter, word)
  }

  def puzzleLines: Parser[List[PuzzleLine]] =  rep1(puzzleLine)


  def apply(input: String): List[PuzzleLine] = {
    parseAll(puzzleLines, input) match {
      case Success(x, _) => {
        x
      }
      case Error(x, _) =>
        println(s"ERROR: ${x}")
        List()
      case Failure(x, in) =>
        println(s"FAILURE: ${x}")
        List()
    }
  }
}


class Day2 extends Solution {
  val input: List[PuzzleLine] =  PuzzleLineParser(readInputRaw(2))


  def partA(): Any = {
    input.foreach(p => println(p))
    ""
  }
  def partB(): Any = ()
}

