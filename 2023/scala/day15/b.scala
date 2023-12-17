import scala.io.Source

object AdventOfCodeDay15 {
  def main(args: Array[String])
  {
    val inputFileName = if (args.length > 0) args(0) else "input.txt"
    val lines = Source.fromFile(inputFileName).getLines.toList

    val input = lines.head.split(',').toList

    def hash(s:String): Int = {
      s.foldLeft(0)((acc, elem) => ((acc + elem.toInt) * 17) % 256)
    }

    def manageLenses(input: List[String]): Map[Int, List[(String, Int)]] = {
      def step(acc: Map[Int, List[(String, Int)]], elem: String): Map[Int, List[(String, Int)]] = {
        val label = elem.takeWhile(_.isLetter)
        val labelIndex = hash(label)
        if (elem.last == '-') {
          acc + (labelIndex -> acc.getOrElse(labelIndex, List()).filter({ case (l, i) => l != label }))
        } else {
          val focalLength = elem.filter(_.isDigit).toInt
          val lenses = acc.getOrElse(labelIndex, List())
          if (lenses.exists({ case (k, v) => k == label })) {
            acc + (labelIndex -> lenses.map({ case (k, v) => if (k == label) (k, focalLength) else (k, v) }))
          } else {
            acc + (labelIndex -> (lenses :+ (label, focalLength)))
          }
        }
      }

      input.foldLeft(Map[Int, List[(String, Int)]]())(step)
    }

    def computePower(boxes: Map[Int, List[(String, Int)]]): Int = {
      boxes.flatMap({ case (box, lenses) => {
        (box + 1) * (lenses.zipWithIndex.map({ case ((l, n), p) => n * (p + 1) }))
      }}).sum
    }

    val foo = computePower(manageLenses(input))
    println(foo)
  }
}
