class Day02 extends Day {
    val outcomePart1 = Map(
        ('A', 'X') -> 3,
        ('A', 'Y') -> 6,
        ('A', 'Z') -> 0,
        ('B', 'X') -> 0,
        ('B', 'Y') -> 3,
        ('B', 'Z') -> 6,
        ('C', 'X') -> 6,
        ('C', 'Y') -> 0,
        ('C', 'Z') -> 3,
    )

    def calculateScorePart1(line: String) =
        val theirs = line(0)
        val ours = line(2)

        val ourScore = ours match
            case 'X' => 1
            case 'Y' => 2
            case 'Z' => 3
            case _ => 0

        val winScore = outcomePart1((theirs, ours))

        ourScore + winScore

    override def partOne(lines: Seq[String]) =
        lines.map(calculateScorePart1).sum.toString

    val moveMapPart2 = Map(
        ('A', 'X') -> 'Z',
        ('A', 'Y') -> 'X',
        ('A', 'Z') -> 'Y',
        ('B', 'X') -> 'X',
        ('B', 'Y') -> 'Y',
        ('B', 'Z') -> 'Z',
        ('C', 'X') -> 'Y',
        ('C', 'Y') -> 'Z',
        ('C', 'Z') -> 'X',
    )

    def calculateScorePart2(line: String) =
        val theirs = line(0)
        val ours = moveMapPart2((theirs, line(2)))

        val ourScore = ours match
            case 'X' => 1
            case 'Y' => 2
            case 'Z' => 3
            case _ => 0

        val winScore = outcomePart1((theirs, ours))

        ourScore + winScore

    override def partTwo(lines: Seq[String]) =
        lines.map(calculateScorePart2).sum.toString
}

object Day02 {
    val dayNumber = "02"

    def main(args: Array[String]) = Day02().execute(if args.length > 0 then args(0) else s"input${dayNumber}.txt")
}
