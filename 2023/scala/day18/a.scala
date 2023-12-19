import scala.io.Source

//import scala.collection.mutable.Map

object AdventOfCodeDay15 {
  def main(args: Array[String])
  {
    val inputFileName = if (args.length > 0) args(0) else "input.txt"
    val lines = Source.fromFile(inputFileName).getLines.toList

    case class Pos(r: Int, c: Int)
    case class Instruction(direction: Char, distance: Int, color: String)

    def parseLine(s: String): Instruction = {
      val parts = s.split(' ')
      Instruction(parts(0).head, parts(1).toInt, parts(2).substring(2, 8))
    }

    val input = lines.map(parseLine)

    println(input)

    def generateOutline(start: Pos, instructions: List[Instruction]): List[Pos] = {
      instructions match {
        case Nil => List()
        case cur :: rest =>
          val added = cur.direction match {
            case 'R' => ((start.c + 1) to (start.c + cur.distance)).map(Pos(start.r, _))
            case 'D' => ((start.r + 1) to (start.r + cur.distance)).map(Pos(_, start.c))
            case 'L' => ((start.c - 1) to (start.c - cur.distance) by -1).map(Pos(start.r, _))
            case 'U' => ((start.r - 1) to (start.r - cur.distance) by -1).map(Pos(_, start.c))
          }

          added.toList ::: generateOutline(added.last, rest)
        }
    }

    val outline = generateOutline(Pos(0, 0), input)
    println(outline)

    def scanFill(outline: List[Pos]): Set[Pos] = {
      outline.groupBy(_.r).toList.sortBy(_._1).flatMap({ case (r, ps) => {
        val edges = ps.toList.sortBy(_.c).zipWithIndex.groupBy({ case (x, i) => x.c - i }).map(xs => xs._2.map(_._1))
        edges.grouped(2).flatMap(_ match {
          case Nil => List()
          case x :: Nil => x
          case left :: right :: Nil =>
            left ::: (left.last.c to right.head.c).map(Pos(r, _)).toList ::: right
        })
      }}).toSet
    }

    def floodFill(outline: List[Pos]): Set[Pos] = {
      val minRow = outline.map(x => x.r).min
      val maxRow = outline.map(x => x.r).max
      val minCol = outline.map(x => x.c).min
      val maxCol = outline.map(x => x.c).max

      val start = Pos((maxRow - minRow) / 2 + minRow, (maxCol - minCol) / 2 + minCol)
     
      def neighbors(p: Pos): List[Pos] = List(Pos(p.r + 1, p.c), Pos(p.r - 1, p.c), Pos(p.r, p.c + 1), Pos(p.r, p.c - 1))

      def step(queue: List[Pos], filled: Set[Pos]): Set[Pos] = {
        queue match {
          case Nil => filled
          case p :: rest => {
            if ((p.r < minRow) || (p.r > maxRow) || (p.c < minCol) || (p.c > maxCol)) sys.error(s"fuck ${minRow}/${maxRow}/${minCol}/${maxCol}, ${p}")
            if (filled contains p) step(rest, filled)
            else {
              //dump(filled + p)
              step(queue ::: neighbors(p), filled + p)
            }
          }
        }
      }

      step(List(start), outline.toSet)
    }

    def dump(points: Set[Pos]): Unit = {
      val minRow = points.map(x => x.r).min
      val maxRow = points.map(x => x.r).max
      val minCol = points.map(x => x.c).min
      val maxCol = points.map(x => x.c).max

      (minRow to maxRow).foreach(r => {
        (minCol to maxCol).foreach(c => {
          if (points contains Pos(r, c)) print("#") else print(".")
        })
        println()
      })
    }

    dump(outline.toSet)
    val result = floodFill(outline)
    dump(result)
    //println(result.toList.sortBy(x => (x.r, x.c)))
    println(result.size)
  }
}


