package days

class Day1 extends Solution {
  val nums: List[Int] = readInput(1).map(_.toInt)

  def partA(): Any = for {
    line <- nums
    line2 <- nums
    if (line + line2) == 2020
  } return line * line2

  def partB(): Any = for {
    line <- nums
    line2 <- nums
    line3 <- nums
    if (line + line2 + line3) == 2020
  } return line * line2 * line3
}
