import scala.io.Source

object AdventOfCodeDay21 {
  def main(args: Array[String])
  {
    val inputFileName = if (args.length > 0) args(0) else "input.txt"
    val lines = Source.fromFile(inputFileName).getLines.toList

    case class Block(xs: (Int, Int), ys: (Int, Int), zs: (Int, Int), idx: Int)

    def parseLine(s: String, i: Int): Block = {
      val halves = s.split("~")
      val a = halves(0).split(",").map(_.toInt)
      val b = halves(1).split(",").map(_.toInt)

      Block((a(0), b(0)), (a(1), b(1)), (a(2), b(2)), i) // i is to disambiguate, it'll make sense later
    }

    val input = lines.zipWithIndex.map({ case (l, i) => parseLine(l, i) }).sortBy({ case Block(_, _, (zmin, zmax), _) => zmin })

    def settleBlocks(blocks: List[Block]): List[Block] = {
      def settleBlock(acc: (List[Block], Map[(Int, Int), Int]), block: Block): (List[Block], Map[(Int, Int), Int]) = {
        val (blocks, terrain) = acc

        block match {
          case Block((xmin, xmax), (ymin, ymax), (zmin, zmax), i) => {
            val landing = (for { x <- (xmin to xmax); y <- (ymin to ymax) } yield terrain.getOrElse((x, y), 0)).max + 1
            val dropDistance = zmin - landing
            val newTop = (for { x <- (xmin to xmax); y <- (ymin to ymax) } yield ((x, y), zmax - dropDistance)).toList

            (block.copy(zs = (zmin - dropDistance, zmax - dropDistance)) :: blocks, (terrain ++ newTop).toMap)
          }
        }
      }

      blocks.foldLeft[(List[Block], Map[(Int, Int), Int])]((List[Block](), Map[(Int, Int), Int]()))(settleBlock)._1.reverse
    }

    val settled = settleBlocks(input)

    def findExtraneous(blocks: List[Block]): Int = {
      (for { block <- blocks } yield {
        val missingOne = blocks.filter(_ != block)
        // disambiguation is needed here, otherwise you end up with a block slotting into the disintegrated block's space,
        // so it won't count as moved (and so you'll undercount).
        (settleBlocks(missingOne).toSet -- missingOne).size
      }).sum
    }

    println(findExtraneous(settled))

  }
}


