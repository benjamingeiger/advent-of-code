class Day03 extends Day {
    
    def itemValue(item: Char) =
        item match
            case c if 'A' to 'Z' contains c => c.toInt - 'A'.toInt + 27
            case c if 'a' to 'z' contains c => c.toInt - 'a'.toInt + 1
            case c => throw new Exception(s"bad input character ${c}")

    def splitBag(bag: String) =
        val firstHalf = bag.substring(0, bag.length / 2)
        val secondHalf = bag.substring(bag.length / 2)

        (firstHalf, secondHalf)

    def findCommonItem(firstHalf: String, secondHalf: String) =
        val overlap = firstHalf.toSet intersect secondHalf.toSet
        if overlap.size == 1 then overlap.head else throw new Exception(s"bad overlap: ${overlap}")

    override def partOne(lines: Seq[String]) =
        lines.map(splitBag).map(findCommonItem.tupled).map(itemValue).sum.toString

    def findCommonItem2(bags: Seq[String]) =
        val overlap = bags.map(_.toSet).reduce((a, b) => a intersect b)
        if overlap.size == 1 then overlap.head else throw new Exception(s"bad overlap: ${overlap}")

    override def partTwo(lines: Seq[String]) =
        lines.grouped(3).toList.map(findCommonItem2).map(itemValue).sum.toString
}

object Day03 {
    val dayNumber = "03"

    def main(args: Array[String]) = Day03().execute(if args.length > 0 then args(0) else s"input${dayNumber}.txt")
}
