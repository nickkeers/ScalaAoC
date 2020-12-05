package days.lib

object Control {
  def using[A <: {def close(): Unit}, B](resource: A)(f: A => B): B =
    try {
      f(resource)
    } finally {
      resource.close()
    }

  def readTextFile(filename: String): Option[List[String]] = {
    try {
      val lines = using(io.Source.fromFile(filename)) { source =>
        (for (line <- source.getLines) yield line).toList
      }
      Some(lines)
    } catch {
      case e: Exception => None
    }
  }

  def readTextFileRaw(filename: String): Option[String] = {
    try {
      val txt = using(io.Source.fromFile(filename)) { source =>
        source.getLines().mkString("\n")
      }
      Some(txt)
    } catch {
      case e: Exception => None
    }
  }
}
