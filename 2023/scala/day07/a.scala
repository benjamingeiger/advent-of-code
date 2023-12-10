import scala.io.Source

object AdventOfCodeDay02 {
  def main(args: Array[String])
  {
    val inputFileName = if (args.length > 0) args(0) else "input.txt"
    val lines = Source.fromFile(inputFileName).getLines.toList

    val strengths = Map(
      'A' -> 'M',
      'K' -> 'L',
      'Q' -> 'K',
      'J' -> 'J',
      'T' -> 'I',
      '9' -> 'H',
      '8' -> 'G',
      '7' -> 'F',
      '6' -> 'E',
      '5' -> 'D',
      '4' -> 'C',
      '3' -> 'B',
      '2' -> 'A')

    def scoreHand(hand: String): Char = {
      // G: 5 of a kind
      // F: 4 of a kind
      // E: full house
      // D: 3 of a kind
      // C: 2 pair
      // B: one pair
      // A: high card

      val cards = hand.groupBy(identity).map({ case (k, vs) => (k, vs.length) }).toList.sortBy(_._2)(Ordering[Int].reverse)

      cards match {
        case (_, 5) :: _ => 'G'
        case (_, 4) :: _ => 'F'
        case (_, 3) :: (_, 2) :: _ => 'E'
        case (_, 3) :: _ => 'D'
        case (_, 2) :: (_, 2) :: _ => 'C'
        case (_, 2) :: _ => 'B'
        case (_, 1) :: _ => 'A'
      }
    }

    def sortHands(s1: String, s2: String): Boolean = {
      val handScore1 = scoreHand(s1)
      val handScore2 = scoreHand(s2)

      if (handScore1 < handScore2) true else if (handScore2 < handScore1) false else {
        val handStrengths1 = s1.map(strengths).mkString("")
        val handStrengths2 = s2.map(strengths).mkString("")

        handStrengths1 < handStrengths2
      }
    }

    def parseLine(s: String): (String, Int) = {
      val parts = s.split(" ")
      (parts(0), parts(1).toInt)
    }

    def sortFunc(hand1: (String, Int), hand2: (String, Int)): Boolean = {
      println(s"Comparing $hand1 and $hand2")
      val result = sortHands(hand1._1, hand2._1)
      if (result) println(s"${hand1._1} is less than ${hand2._1}") else println(s"${hand1._1} is not less than ${hand2._1}")
      result
    }

    val hands = lines.map(parseLine).sortWith(sortFunc)
    println(hands)

    val hands2 = hands.map(_._2).zipWithIndex
    println(hands2)

    println(hands2.map({ case (bet, i) => bet * (i + 1) }).map(_.toLong).sum)

  }
}
