package days

class Day1 extends Solution {
  val numbers: List[Int] = readInput(1).map(_.toInt)

  def partA(): Any = for {
    line <- numbers
    line2 <- numbers
    if (line + line2) == 2020
  } return line * line2

  def partB(): Any = for {
    line <- numbers
    line2 <- numbers
    line3 <- numbers
    if (line + line2 + line3) == 2020
  } return line * line2 * line3
}
