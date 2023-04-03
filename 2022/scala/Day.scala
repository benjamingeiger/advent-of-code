// Inspired by https://github.com/Yeah69/AdventOfCodeScala/blob/main/AdventOfCodeScala/src/main/scala/Day.scala

import scala.io.Source

class Day {
    def partOne(input: Seq[String]) = "Part 1 not implemented yet."
    def partTwo(input: Seq[String]) = "Part 2 not implemented yet."

    def getInput(filename: String) = Source.fromFile(filename).getLines.toList

    def execute(filename: String) : Unit =
        val input = getInput(filename)

        val partOneResults = partOne(input)
        println(partOneResults)

        val partTwoResults = partTwo(input)
        println(partTwoResults)
}

