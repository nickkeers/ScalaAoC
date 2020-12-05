import days.{Day1, Day2}

object main extends App {
  lazy val solutions = List(new Day1(), new Day2() )

  solutions.zip(1.to(solutions.length)).foreach{case (day, idx) =>
    println(s"Day ${idx}\n---------------")
    println(s"Part A: ${day.partA()}")
    println(s"Part B: ${day.partB()}")
    println()
  }
}
