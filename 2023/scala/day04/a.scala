import scala.io.Source

object AdventOfCodeDay02 {
  def main(args: Array[String])
  {
    val inputFileName = if (args.length > 0) args(0) else "input.txt"
    val lines = Source.fromFile(inputFileName).getLines.toList

    def parseCard(s: String): (Int, Set[Int], Set[Int]) = {
      val (cardNumberText, raw) = s.splitAt(s.indexOf(':'))
      val (winningText, ourText) = raw.tail.splitAt(raw.tail.indexOf('|'))
      //println(s"cardNumberText: [$cardNumberText]")
      //println(s"winningText: [$winningText]")
      //println(s"ourText: [$ourText]")

      val cardNumber = cardNumberText.filter(_.isDigit).toInt
      val winningNumbers = winningText.trim.split(' ').filter(_.length > 0).map(_.toInt).toSet
      val ourNumbers = ourText.tail.trim.split(' ').filter(_.length > 0).map(_.toInt).toSet

      (cardNumber, winningNumbers, ourNumbers)
    }

    def scoreCard(card: (Int, Set[Int], Set[Int])): Int = {
      card match {
        case (_, winning, ours) =>
          val good = winning.intersect(ours).size
          if (good == 0) 0 else scala.math.pow(2, good - 1).intValue
      }
    }

    val input = lines.map(parseCard).map(scoreCard).sum
    println(input)
  }
}
