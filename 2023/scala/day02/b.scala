import scala.io.Source

object AdventOfCodeDay01 {
    def main(args: Array[String])
    {
        val inputFileName = if (args.length > 0) args(0) else "input.txt"
        val lines = Source.fromFile(inputFileName).getLines.toList

        def parseInputLine(s: String) : (Int, List[Map[String, Int]]) = {
            val (idText, gamesText) = s.splitAt(s.indexOf(':'))

            def parseGameCount(x: String): (String, Int) = {
                val parts = x.trim.split(' ')
                (parts(1), parts(0).toInt)
            }

            def parseGameSet(x: String): Map[String, Int] = {
                x.split(',').map(_.trim).map(parseGameCount).toMap
            }

            def parseGameRecord(g: String): List[Map[String, Int]] = {
                g.split(';').map(parseGameSet).toList
            }

            val gameNumber = idText.filter(_.isDigit).toInt
            val gameResults = parseGameRecord(gamesText.tail)

            (gameNumber, gameResults)
        }

        def mergeMaps[K, V](f: (V, V) => V, d: V)(m1: Map[K, V], m2: Map[K, V]): Map[K, V] = {
            val allKeys = m1.keys.toSet.union(m2.keys.toSet)
            allKeys.map(k => (k, f(m1.getOrElse(k, d), m2.getOrElse(k, d)))).toMap
        }

        val games = lines
            .map(parseInputLine)

        val mergeMapsMax = mergeMaps[String, Int](((x: Int, y: Int) => (x max y)), 0) _

        def calculateMinimum(rec: List[Map[String, Int]]) : Map[String, Int] = {
            rec.foldLeft(Map[String, Int]()) (mergeMapsMax)
        }

        def calculatePower(s: Map[String, Int]): Int = {
            s.getOrElse("red", 0) * s.getOrElse("green", 0) * s.getOrElse("blue", 0)
        }

        val result = games.map(_._2).map(calculateMinimum).map(calculatePower).sum

        println(result)
    }
}
