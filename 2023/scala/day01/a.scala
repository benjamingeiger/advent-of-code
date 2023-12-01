import scala.io.Source

object AdventOfCodeDay01 {
    def main(args: Array[String])
    {
        val inputFileName = if (args.length > 0) args(0) else "input.txt"
        val lines = Source.fromFile(inputFileName).getLines.toList

        val foo = lines
            .map(s => s.filter(_.isDigit))
            .map(s => s.take(1) + s.takeRight(1))
            .map(_.toInt)
            .sum

        println(foo)
    }
}
