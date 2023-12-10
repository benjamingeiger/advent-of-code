import scala.io.Source

object AdventOfCodeDay02 {
  def main(args: Array[String])
  {
    val inputFileName = if (args.length > 0) args(0) else "input.txt"
    val lines = Source.fromFile(inputFileName).getLines.toList

    def parseLine(line: String): List[Int] = {
      line.split(' ').map(_.toInt).toList
    }

    def differences(xs: List[Int]): List[Int] = {
      xs match {
        case Nil => Nil
        case _ :: rest => xs.zip(rest).map { case (a, b) => b - a }
      }
    }

    def allSame(xs: List[Int]): Boolean = {
      xs match {
        case Nil => true
        case h :: t => t.forall(_ == h)
      }
    }

    def extrapolate(xs: List[Int]): Int = {
      if (xs == Nil) 0
      else if (allSame(xs)) {
        xs.head
      }
      else {
        val diffs = differences(xs)
        val next = extrapolate(diffs)

        xs.last + next
      }
    }

    val result = lines.map(parseLine).map(_.reverse).map(extrapolate).sum
    println(result)

  }
}
