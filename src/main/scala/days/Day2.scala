package days

import scala.language.postfixOps
import scala.util.matching.Regex
import scala.util.parsing.combinator._

class Day2 extends Solution {

  class PuzzleLine(lowerBound: Int, upperBound: Int, letter: Char, word: List[Char]) {
    def letterCount(chars: List[Char]): Map[Char, Int] = {
      chars.groupBy(identity).view.mapValues(_.size).toMap
    }

    def success(): Boolean = {
      (lowerBound to upperBound) contains letterCount(word).getOrElse(letter, 0)
    }

    def success2(): Boolean = {
      letterCount(List(lowerBound - 1, upperBound - 1).map(word)).getOrElse(letter, 0) == 1
    }
  }

  object PuzzleLineParser extends RegexParsers {
    override val whiteSpace: Regex = """[ \t]+""".r

    private def boundNum: Parser[Int] =
      """(\d+)""".r ^^ {
        _.toInt
      }

    private def char: Parser[String] =
      """[a-z]""".r <~ ":" ^^ {
        letter => letter
      }

    private def word: Parser[String] =
      """[a-z]+""".r ^^ {
        _.toString
      }

    private def bounds: Parser[(Int, Int)] = boundNum ~ "-" ~ boundNum ^^ {
      case a ~ _ ~ b =>
        (a, b)
    }

    val eol: Regex = """[\r?\n]+""".r

    private def puzzleLine: Parser[PuzzleLine] = opt(eol) ~> bounds ~ char ~ word <~ opt(eol) ^^ {
      case (a, b) ~ letter ~ word => new PuzzleLine(a, b, letter.charAt(0), word.toList)
    }

    def puzzleLines: Parser[List[PuzzleLine]] = rep1(puzzleLine)


    def apply(input: String): List[PuzzleLine] = {
      parseAll(puzzleLines, input) match {
        case Success(x, _) => x
        case Error(x, _) =>
          println(s"ERROR: ${x}")
          List()
        case Failure(x, in) =>
          println(s"FAILURE: ${x}")
          List()
      }
    }
  }

  val input: List[PuzzleLine] = PuzzleLineParser(readInputRaw(2))

  def partA(): Int = {
    input.count(pl => pl.success())
  }

  def partB(): Any = {
    input.count(p => p.success2())
  }
}

