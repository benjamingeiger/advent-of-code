import scala.io.Source

object AdventOfCodeDay01 {
    def main(args: Array[String])
    {
        val inputFileName = if (args.length > 0) args(0) else "input.txt"
        val lines = Source.fromFile(inputFileName).getLines.toList

        def findDigits(s : String) : List[Int] = {
            s match {
                case "" => List()
                case s if (s.startsWith("one") || s.startsWith("1")) => 1 :: findDigits(s.substring(1))
                case s if (s.startsWith("two") || s.startsWith("2")) => 2 :: findDigits(s.substring(1))
                case s if (s.startsWith("three") || s.startsWith("3")) => 3 :: findDigits(s.substring(1))
                case s if (s.startsWith("four") || s.startsWith("4")) => 4 :: findDigits(s.substring(1))
                case s if (s.startsWith("five") || s.startsWith("5")) => 5 :: findDigits(s.substring(1))
                case s if (s.startsWith("six") || s.startsWith("6")) => 6 :: findDigits(s.substring(1))
                case s if (s.startsWith("seven") || s.startsWith("7")) => 7 :: findDigits(s.substring(1))
                case s if (s.startsWith("eight") || s.startsWith("8")) => 8 :: findDigits(s.substring(1))
                case s if (s.startsWith("nine") || s.startsWith("9")) => 9 :: findDigits(s.substring(1))
                case s if (s.startsWith("0")) => 0 :: findDigits(s.substring(1))
                case s => findDigits(s.substring(1))
            }
        }

        val foo = lines
            .map(findDigits)
            .map(_.mkString(""))
            .map(s => s.filter(_.isDigit))
            .map(s => s.take(1) + s.takeRight(1))
            .map(_.toInt)
            .sum

        println(foo)
    }
}
