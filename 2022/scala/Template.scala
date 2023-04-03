class Day01 extends Day {
    override def partOne(lines: Seq[String]) =
        "foo"

    override def partTwo(lines: Seq[String]) =
        "bar"
}

object Day01 {
    val dayNumber = "01"

    def main(args: Array[String]) = Day01().execute(if args.length > 0 then args(0) else s"input${dayNumber}.txt")
}
