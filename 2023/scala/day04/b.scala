import scala.io.Source

object AdventOfCodeDay02 {
  def main(args: Array[String])
  {
    val inputFileName = if (args.length > 0) args(0) else "input.txt"
    val lines = Source.fromFile(inputFileName).getLines.toList

    def parseCard(s: String): (Int, Set[Int], Set[Int]) = {
      val (cardNumberText, raw) = s.splitAt(s.indexOf(':'))
      val (winningText, ourText) = raw.tail.splitAt(raw.tail.indexOf('|'))

      val cardNumber = cardNumberText.filter(_.isDigit).toInt
      val winningNumbers = winningText.trim.split(' ').filter(_.length > 0).map(_.toInt).toSet
      val ourNumbers = ourText.tail.trim.split(' ').filter(_.length > 0).map(_.toInt).toSet

      (cardNumber, winningNumbers, ourNumbers)
    }

    def scoreCard(card: (Int, Set[Int], Set[Int])): Int = {
      card match {
        case (_, winning, ours) => winning.intersect(ours).size
      }
    }

    def computeWins(cards: List[(Int, Int)]) = {
      val cardMap = cards.toMap
      def step(cardNumbers: List[Int], pile: Map[Int, Int]): Map[Int, Int] = {
        cardNumbers match {
          case Nil => pile
          case h :: t =>
            val newPile = pile ++ (((h + 1) to (h + cardMap(h))).map(n => (n, pile(n) + pile(h)))).toMap
            step(t, newPile)
        }
      }

      step(cards.map(_._1), cards.map(c => (c._1, 1)).toMap).values.sum
    }

    val input = lines.map(parseCard).map(card => (card._1, scoreCard(card)))
    println(computeWins(input))
  }
}
