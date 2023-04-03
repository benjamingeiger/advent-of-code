class Day01 extends Day {

    def toInt(s: String) =
        try
            Some(s.toInt)
        catch 
            case ex: Exception => None

    def groupSums(lines: Seq[String]) =
        var total = 0
        var groups = List[Int]()

        for line <- lines do
            toInt(line) match
                case Some(i) => total = total + i
                case None =>
                    groups = total :: groups
                    total = 0

        if total != 0 then total :: groups else groups

    override def partOne(lines: Seq[String]) =
        groupSums(lines).max.toString

    override def partTwo(lines: Seq[String]) =
        "bar"
}

object Day01 {
    val dayNumber = "01"

    def main(args: Array[String]) = Day01().execute(if args.length > 0 then args(0) else s"input${dayNumber}.txt")
}
