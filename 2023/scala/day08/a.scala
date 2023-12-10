import scala.io.Source

object AdventOfCodeDay02 {
  def main(args: Array[String])
  {
    val inputFileName = if (args.length > 0) args(0) else "input.txt"
    val lines = Source.fromFile(inputFileName).getLines.toList

    val path = lines.head
    val nodeText = lines.drop(2)

    val nodes = nodeText.map(s => (s.slice(0, 3), (s.slice(7, 10), s.slice(12, 15)))).toMap
    println(nodes)

    def traverse(nodeMap: Map[String, (String, String)], path: String, start: String): Int = {
      def step(current: String, pathPart: List[Char], count: Int): Int = {
        if (current == "ZZZ") count
        else pathPart match {
          case Nil => step(current, path.toList, count)
          case 'L' :: rest => step(nodeMap(current)._1, rest, count + 1)
          case 'R' :: rest => step(nodeMap(current)._2, rest, count + 1)
          case _ :: rest => step(current, rest, count)
        }
      }

      step(start, path.toList, 0)
    }

    val result = traverse(nodes, path, "AAA")
    println(result)

  }
}
