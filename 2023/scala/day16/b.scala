import scala.io.Source

object AdventOfCodeDay15 {
  def main(args: Array[String])
  {
    val inputFileName = if (args.length > 0) args(0) else "input.txt"
    val lines = Source.fromFile(inputFileName).getLines.toList

    def indexMapCharacters[T](xs: List[String]): List[((Int, Int), Char)] = {
      xs.map(_.zipWithIndex).zipWithIndex.flatMap { case (cs, r) => cs.map { case (ch, c) => ((r, c), ch) } }
    }

    val inputChars = indexMapCharacters(lines).toMap

    def trackLight(input: Map[(Int, Int), Char], start: ((Int, Int), Char)): Set[(Int, Int)] = {
      def step(wavefront: Set[((Int, Int), Char)], lit: Set[((Int, Int), Char)]): Set[(Int, Int)] = {
        def advance(beam: ((Int, Int), Char)): List[((Int, Int), Char)] = {
          if (lit.contains(beam)) List()
          else {
            val ((r, c), dir) = beam
            val nextPos = dir match {
              case '>' => (r, c + 1)
              case '<' => (r, c - 1)
              case '^' => (r - 1, c)
              case 'v' => (r + 1, c)
              case _ => sys.error(s"what direction? (${dir})")
            }

            input.get(nextPos) match {
              case None => List()
              case Some(ch) => ch match {
                case '.' => List((nextPos, dir))
                case '\\' => dir match {
                  case '>' => List((nextPos, 'v'))
                  case '<' => List((nextPos, '^'))
                  case '^' => List((nextPos, '<'))
                  case 'v' => List((nextPos, '>'))
                  case _ => sys.error("lolwut")
                }
                case '/' => dir match {
                  case '>' => List((nextPos, '^'))
                  case '<' => List((nextPos, 'v'))
                  case '^' => List((nextPos, '>'))
                  case 'v' => List((nextPos, '<'))
                  case _ => sys.error("lolwut")
                }
                case '-' => dir match {
                  case '>' => List((nextPos, dir))
                  case '<' => List((nextPos, dir))
                  case '^' => List((nextPos, '>'), (nextPos, '<'))
                  case 'v' => List((nextPos, '>'), (nextPos, '<'))
                  case _ => sys.error("lolwut")
                }
                case '|' => dir match {
                  case '<' => List((nextPos, '^'), (nextPos, 'v'))
                  case '>' => List((nextPos, '^'), (nextPos, 'v'))
                  case '^' => List((nextPos, dir))
                  case 'v' => List((nextPos, dir))
                  case _ => sys.error("lolwut")
                }
              }
            }
          }
        }

        if (wavefront.size == 0) lit.map(_._1).toSet
        else {
          val frontier = wavefront.flatMap(advance)
          step(frontier, lit ++ wavefront)
        }
      }

      step(List(start).toSet, Set[((Int, Int), Char)]())
    }

    val maxRow = inputChars.toList.map(_._1._1).max
    val maxCol = inputChars.toList.map(_._1._2).max

    val topStarts = (0 to maxCol).map(c => (trackLight(inputChars, ((-1, c), 'v')).size - 1))
    val bottomStarts = (0 to maxCol).map(c => (trackLight(inputChars, ((maxRow + 1, c), '^')).size - 1))
    val leftStarts = (0 to maxRow).map(r => (trackLight(inputChars, ((r, -1), '>')).size - 1))
    val rightStarts = (0 to maxRow).map(r => (trackLight(inputChars, ((r, maxCol + 1), '<')).size - 1))

    println((topStarts ++ bottomStarts ++ leftStarts ++ rightStarts).max)
  }
}


