import scala.io.Source

object AdventOfCodeDay02 {
  def main(args: Array[String])
  {
    val inputFileName = if (args.length > 0) args(0) else "input.txt"
    val lines = Source.fromFile(inputFileName).getLines.toList

    val path = lines.head
    val nodeText = lines.drop(2)

    val nodes = nodeText.map(s => (s.slice(0, 3), (s.slice(7, 10), s.slice(12, 15)))).toMap

    def traverse(nodeMap: Map[String, (String, String)], path: String, start: String): Int = {
      def step(current: String, pathPart: List[Char], count: Int): Int = {
        if (current(2) == 'Z') count
        else pathPart match {
          case Nil => step(current, path.toList, count)
          case 'L' :: rest => step(nodeMap(current)._1, rest, count + 1)
          case 'R' :: rest => step(nodeMap(current)._2, rest, count + 1)
          case _ :: rest => step(current, rest, count)
        }
      }

      step(start, path.toList, 0)
    }

    //val result = traverse(nodes, path, "AAA")
    //println(result)
    
    val pathlengths = nodes.keys.filter(_(2) == 'A').toList.map(s => {
      print(s"Length for ${s}: ")
      val result = traverse(nodes, path, s)
      println(result)
      result
    })

    // borrowed from rosetta code
    def gcd(a: Long, b: Long):Long=if (b==0) a.abs else gcd(b, a%b)
    def lcm(a: Long, b: Long)=(a*b).abs/gcd(a,b)

    println(pathlengths.map(_.toLong).reduce(lcm))

  }
}
