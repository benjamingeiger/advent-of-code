import scala.io.Source

//import scala.collection.mutable.Map

object AdventOfCodeDay18 {
  def main(args: Array[String])
  {
    val inputFileName = if (args.length > 0) args(0) else "input.txt"
    val lines = Source.fromFile(inputFileName).getLines.toList

    case class Pos(r: Int, c: Int)
    case class Instruction(direction: Char, distance: Long)

    val directionMap = Map('0' -> 'R', '1' -> 'D', '2' -> 'L', '3' -> 'U')
    def parseLine(s: String): Instruction = {
      val parts = s.split(' ')
      Instruction(directionMap(parts(2)(7)), Integer.parseInt(parts(2).substring(2, 7), 16))
    }

    val input = lines.map(parseLine)

    def calculateArea(instructions: List[Instruction]): Long = {
      case class State(offset: Long, total: Long)
      def step(state: State, inst: Instruction): State = {
        inst.direction match {
          case 'R' => State(state.offset, state.total + (state.offset * inst.distance))
          case 'L' => State(state.offset, state.total - ((state.offset - 1) * inst.distance))
          case 'D' => State(state.offset - inst.distance, state.total)
          case 'U' => State(state.offset + inst.distance, state.total + inst.distance)
        }
      }

      instructions.foldLeft(State(0, 1))(step).total
    }

    val result = calculateArea(input)
    println(result)

  }
}


