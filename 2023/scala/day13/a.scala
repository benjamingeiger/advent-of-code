import scala.io.Source

object AdventOfCodeDay13 {
  def main(args: Array[String])
  {
    val inputFileName = if (args.length > 0) args(0) else "input.txt"
    val lines = Source.fromFile(inputFileName).getLines.toList

    def indexMapCharacters[T](xs: List[String]): List[((Int, Int), Char)] = {
      xs.map(_.zipWithIndex).zipWithIndex.flatMap { case (cs, r) => cs.map { case (ch, c) => ((r, c), ch) } }
    }

    def splitLineGroups(lines: List[String]): List[List[String]] = {
      if (lines == Nil) List() else {
        val (cur, next) = lines.span(_.trim() != "")
        cur :: splitLineGroups(next.dropWhile(_.trim() == ""))
      }
    }

    val inputChars = splitLineGroups(lines).map(indexMapCharacters).map(_.toMap)

    def hasVerticalMirror(cells: Map[(Int, Int), Char]): Option[Int] = {
      def maxRow = cells.keys.map({ case (r, c) => r }).max
      def maxCol = cells.keys.map({ case (r, c) => c }).max

      def hasVerticalMirrorAt(col: Int): Boolean = {
        def equals(kv: ((Int, Int), Char)): Boolean = {
          val ((r, c), x) = kv
          val reflectedCol = 2 * col - c + 1
          x == cells.getOrElse((r, reflectedCol), x)
        }
        cells.forall(equals)
      }

      (0 until maxCol).find(col => hasVerticalMirrorAt(col))
    }

    def hasHorizontalMirror(cells: Map[(Int, Int), Char]): Option[Int] = {
      def maxRow = cells.keys.map({ case (r, c) => r }).max
      def maxCol = cells.keys.map({ case (r, c) => c }).max

      def hasHorizontalMirrorAt(row: Int): Boolean = {
        def equals(kv: ((Int, Int), Char)): Boolean = {
          val ((r, c), x) = kv
          val reflectedRow = 2 * row - r + 1
          x == cells.getOrElse((reflectedRow, c), x)
        }
        cells.forall(equals)
      }

      (0 until maxRow).find(row => hasHorizontalMirrorAt(row))
    }

    val horizontals = inputChars.flatMap(hasHorizontalMirror).map(_ + 1).sum
    val verticals = inputChars.flatMap(hasVerticalMirror).map(_ + 1).sum

    println(100 * horizontals + verticals)
  }
}
