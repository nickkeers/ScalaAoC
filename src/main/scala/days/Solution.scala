package days

import days.lib.Control

trait Solution {
  def readInput(day: Int): List[String] = Control.readTextFile(s"src/day${day}.txt").get
  def readInputRaw(day: Int): String = Control.readTextFileRaw(s"src/day${day}.txt").get
  def partA(): Any
  def partB(): Any
}
