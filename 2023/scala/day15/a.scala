import scala.io.Source

object AdventOfCodeDay15 {
  def main(args: Array[String])
  {
    val inputFileName = if (args.length > 0) args(0) else "input.txt"
    val lines = Source.fromFile(inputFileName).getLines.toList

    val input = lines.head.split(',').toList

    def hash(s:String): Int = s.foldLeft(0)((acc, elem) => ((acc + elem.toInt) * 17) % 256)

    println(input.map(hash).sum)
  }
}
