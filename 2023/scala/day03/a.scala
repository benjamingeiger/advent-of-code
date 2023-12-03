import scala.io.Source

object AdventOfCodeDay02 {
  def main(args: Array[String])
  {
    val inputFileName = if (args.length > 0) args(0) else "input.txt"
    val lines = Source.fromFile(inputFileName).getLines.toList

    def extractNumbers(s: String): List[(Int, Int, Int)] = {
      def step(i: Int, ss: String): List[(Int, Int, Int)] = {
        if (ss.isEmpty) List()
        else
          ("""\d+""".r findPrefixOf ss) match {
            case Some(num) => (i, i + num.length - 1, num.toInt) :: step(i + num.length, ss.substring(num.length))
            case None => step(i + 1, ss.tail)
          }
      }

      step(0, s)
    }

    val allNumbers =
      lines
      .map(extractNumbers)
      .zipWithIndex
      .flatMap({ case (l, r) =>
        l.map({ case (c1, c2, n) => (r, c1, c2, n) }) })

    val symbolLocations = lines
      .map(_.zipWithIndex).zipWithIndex
      .flatMap({ case (s, r) => s.flatMap { case (x, c) => if ("""$*#&=+-%@/""" contains x) Some((r, c)) else None }})

    val partNumbers = allNumbers
      .filter({ case (r, c1, c2, n) =>
        symbolLocations.exists({ case (sr, sc) =>
          sr >= r - 1 && sr <= r + 1 && sc >= c1 - 1 && sc <= c2 + 1 }) })
      .map(_._4)

    println(partNumbers.sum)
  }
}
