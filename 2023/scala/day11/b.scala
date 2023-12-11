import scala.io.Source

object AdventOfCodeDay10 {
  def main(args: Array[String])
  {
    val inputFileName = if (args.length > 0) args(0) else "input.txt"
    val lines = Source.fromFile(inputFileName).getLines.toList

    def indexMapCharacters[T](xs: List[String]): List[((Int, Int), Char)] = {
      xs.map(_.zipWithIndex).zipWithIndex.flatMap { case (cs, r) => cs.map { case (ch, c) => ((r, c), ch) } }
    }

    val inputChars = indexMapCharacters(lines)

    val rows = inputChars.groupBy(_._1._1)
    val cols = inputChars.groupBy(_._1._2)

    val emptyRows = rows.filter(x => x._2.forall(y => y._2 == '.')).map(_._1)
    val emptyCols = cols.filter(x => x._2.forall(y => y._2 == '.')).map(_._1)

    val galaxies = inputChars.filter(_._2 == '#').map(_._1)

    val expanded = galaxies.map({ case (r, c) => (r.toLong + 999999 * emptyRows.filter(_ < r).size, c.toLong + 999999 * emptyCols.filter(_ < c).size) })

    // borrowed from Stack Overflow: https://stackoverflow.com/questions/11803349/composing-a-list-of-all-pairs
    def allPairs[T](xs: List[T]): List[(T, T)] = {
      for {
        (x, ix) <- xs.zipWithIndex
        (y, iy) <- xs.zipWithIndex
        if ix < iy
      } yield (x, y)
    }

    def manhattanDistance(input: ((Long, Long), (Long, Long))): Long = {
      val ((r1, c1), (r2, c2)) = input
      (r2 - r1).abs + (c2 - c1).abs
    }

    val result = allPairs(expanded).map(manhattanDistance).sum
    println(result)
  }
}

