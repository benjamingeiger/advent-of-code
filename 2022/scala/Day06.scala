class Day06 extends Day {

    def findHeader(headerLength: Int)(input: String) =
        def step(idx: Int, cur: List[Char]) : Int =
            val starting = cur.take(headerLength)
            if starting.length < headerLength then
                -1
            else
                if starting.distinct.length == headerLength then
                    headerLength + idx
                else
                    step(idx + 1, cur.tail)

        step(0, input.toList)

    override def partOne(lines: Seq[String]) =
        findHeader(4)(lines(0)).toString

    override def partTwo(lines: Seq[String]) =
        findHeader(14)(lines(0)).toString
}

object Day06 {
    val dayNumber = "06"

    def main(args: Array[String]) = Day06().execute(if args.length > 0 then args(0) else s"input${dayNumber}.txt")
}
