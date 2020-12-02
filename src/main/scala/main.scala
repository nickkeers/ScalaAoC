import scala.io.Source

object main extends App {
  def getMultipliersA(lines: List[Int]): List[(Int, Int)] = {
    for {
      line <- lines
      line2 <- lines
      if (line+line2) == 2020
    } yield (line, line2)
  }

  def getMultipliersB(lines: List[Int]): List[(Int, Int, Int)] = {
    for {
      line <- lines
      line2 <- lines
      line3 <- lines
      if (line+line2+line3) == 2020
    } yield (line, line2, line3)
  }

  val nums = Control.readTextFile("src/day1.txt").get.map(_.toInt)

  getMultipliersA(nums).take(1) match {
    case x @ List((a, b)) => println(s"${a*b}")
  }

  getMultipliersB(nums).take(1) match {
    case x @ List((a, b, c)) => println(s"${a*b*c}")
  }

}
