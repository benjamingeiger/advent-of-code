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

        def isValidGame(game: List[Map[String, Int]]): Boolean = {
            game.forall(gameset => gameset.getOrElse("red", 0) <= 12 && gameset.getOrElse("green", 0) <= 13 && gameset.getOrElse("blue", 0) <= 14)
        }

        val games = lines
            .map(parseInputLine)

        val result = games
            .flatMap({ case (id, rec) => if (isValidGame(rec)) Some(id) else None })
            .sum

        println(result)
    }
}
