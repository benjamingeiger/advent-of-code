import scala.io.Source

object AdventOfCodeDay13 {
  def main(args: Array[String])
  {
    val inputFileName = if (args.length > 0) args(0) else "input.txt"
    val lines = Source.fromFile(inputFileName).getLines.toList

    def indexMapCharacters[T](xs: List[String]): List[((Int, Int), Char)] = {
      xs.map(_.zipWithIndex).zipWithIndex.flatMap { case (cs, r) => cs.map { case (ch, c) => ((r, c), ch) } }
    }

    val inputChars = indexMapCharacters(lines).toMap

    def rollNorth(rocks: Map[(Int, Int), Char]): Map[(Int, Int), Char] = {
      val nextMap = rocks.toList.sortBy(_._1).flatMap({ case (coords, rock) => rock match {
        case '.' => List((coords, rock))
        case '#' => List((coords, rock))
        case 'O' => {
          val (r, c) = coords
          println(s"rolling stone at ${coords}")
          val neighbor = rocks.getOrElse((r - 1, c), '#')
          println(s"neighbor: ${neighbor}")
          neighbor match {
            case '#' => List((coords, rock))
            case 'O' => List((coords, rock))
            case '.' => List(((r - 1, c), 'O'), ((r, c), '.'))
            case _ => sys.error("invalid input char")
          }
        }
        case _ => sys.error("invalid input char")
      }}).groupBy(_._1).map({ case (k, xs) => xs.last }).toMap

      dump(nextMap)

      if (nextMap == rocks) rocks else rollNorth(nextMap)
    }

    def dump(rocks: Map[(Int, Int), Char]): Unit = {
      val minRow = rocks.keys.map(_._1).min
      val maxRow = rocks.keys.map(_._1).max
      val minCol = rocks.keys.map(_._2).min
      val maxCol = rocks.keys.map(_._2).max

      println("====")
      (minRow to maxRow).foreach(row => {
          (minCol to maxCol).foreach(col => print(rocks.getOrElse((row, col), 'X')))
          println()
      })
      println("====")
    }

    dump(inputChars)
    val rolled = rollNorth(inputChars)
    dump(rolled)

    def weight(rocks: Map[(Int, Int), Char]): Int = {
      val maxRow = rocks.keys.map(_._1).max
      rocks.map({ case (coords, rock) => rock match {
        case '.' => 0
        case '#' => 0
        case 'O' => maxRow - coords._1 + 1
        case _ => sys.error("invalid input char")
      }}).sum
    }

    println(weight(rolled))

  }
}
