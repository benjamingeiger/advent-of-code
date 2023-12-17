import scala.io.Source
import scala.annotation.tailrec

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
          val neighbor = rocks.getOrElse((r - 1, c), '#')
          neighbor match {
            case '#' => List((coords, rock))
            case 'O' => List((coords, rock))
            case '.' => List(((r - 1, c), 'O'), ((r, c), '.'))
            case _ => sys.error("invalid input char")
          }
        }
        case _ => sys.error("invalid input char")
      }}).groupBy(_._1).map({ case (k, xs) => if (xs.length > 1) (k, 'O') else xs.head }).toMap

      //dump(nextMap)

      if (nextMap == rocks) rocks else rollNorth(nextMap)
    }

    def rollEast(rocks: Map[(Int, Int), Char]): Map[(Int, Int), Char] = {
      val nextMap = rocks.toList.flatMap({ case (coords, rock) => rock match {
        case '.' => List((coords, rock))
        case '#' => List((coords, rock))
        case 'O' => {
          val (r, c) = coords
          val neighbor = rocks.getOrElse((r, c + 1), '#')
          neighbor match {
            case '#' => List((coords, rock))
            case 'O' => List((coords, rock))
            case '.' => List(((r, c + 1), 'O'), ((r, c), '.'))
            case _ => sys.error("invalid input char")
          }
        }
        case _ => sys.error("invalid input char")
      }}).groupBy(_._1).map({ case (k, xs) => if (xs.length > 1) (k, 'O') else xs.head }).toMap

      if (nextMap == rocks) rocks else rollEast(nextMap)
    }

    def rollSouth(rocks: Map[(Int, Int), Char]): Map[(Int, Int), Char] = {
      val nextMap = rocks.toList.flatMap({ case (coords, rock) => rock match {
        case '.' => List((coords, rock))
        case '#' => List((coords, rock))
        case 'O' => {
          val (r, c) = coords
          val neighbor = rocks.getOrElse((r + 1, c), '#')
          neighbor match {
            case '#' => List((coords, rock))
            case 'O' => List((coords, rock))
            case '.' => List(((r + 1, c), 'O'), ((r, c), '.'))
            case _ => sys.error("invalid input char")
          }
        }
        case _ => sys.error("invalid input char")
      }}).groupBy(_._1).map({ case (k, xs) => if (xs.length > 1) (k, 'O') else xs.head }).toMap

      if (nextMap == rocks) rocks else rollSouth(nextMap)
    }

    def rollWest(rocks: Map[(Int, Int), Char]): Map[(Int, Int), Char] = {
      val nextMap = rocks.toList.sortBy(_._1).flatMap({ case (coords, rock) => rock match {
        case '.' => List((coords, rock))
        case '#' => List((coords, rock))
        case 'O' => {
          val (r, c) = coords
          val neighbor = rocks.getOrElse((r, c - 1), '#')
          neighbor match {
            case '#' => List((coords, rock))
            case 'O' => List((coords, rock))
            case '.' => List(((r, c - 1), 'O'), ((r, c), '.'))
            case _ => sys.error("invalid input char")
          }
        }
        case _ => sys.error("invalid input char")
      }}).groupBy(_._1).map({ case (k, xs) => if (xs.length > 1) (k, 'O') else xs.head }).toMap

      if (nextMap == rocks) rocks else rollWest(nextMap)
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

    def spinCycle(rocks: Map[(Int, Int), Char]): Map[(Int, Int), Char] = rollEast(rollSouth(rollWest(rollNorth(rocks))))

    def rollUntilSteady(limit: Int, start: Map[(Int, Int), Char]): Map[(Int, Int), Char] = {
      @tailrec
      def step(iteration: Int, cache: Map[Map[(Int, Int), Char], Int], current: Map[(Int, Int), Char]): Map[(Int, Int), Char] = {
        if (iteration == limit) current
        else {
          if (iteration % 1000 == 0) print ("|")
          else if (iteration % 100 == 0) print ("'")
          else if (iteration % 10 == 0) print(".")
          val nextStep = spinCycle(current)
          cache.get(nextStep) match {
            case None => step(iteration + 1, (cache + (nextStep -> (iteration + 1))), nextStep)
            case Some(i) => {
              def cycleLength = iteration - i + 1
              def remainder = limit % cycleLength

              def stepForward(n: Int, current: Map[(Int, Int), Char]): Map[(Int, Int), Char] = {
                if (n == 0) current else stepForward(n - 1, spinCycle(current))
              }

              val target = (cycleLength + (remainder - (iteration % cycleLength))) % cycleLength

              stepForward(target, current)
            }
          }
        }
      }

      step(0, Map((start -> 0)), start)
    }

    def weight(rocks: Map[(Int, Int), Char]): Int = {
      val maxRow = rocks.keys.map(_._1).max
      rocks.map({ case (coords, rock) => rock match {
        case '.' => 0
        case '#' => 0
        case 'O' => maxRow - coords._1 + 1
        case _ => sys.error("invalid input char")
      }}).sum
    }

    println(weight(rollUntilSteady(1000000000, inputChars)))
  }
}
