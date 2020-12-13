package aoc.day10

import aoc.util.Util

object Day10 extends App {

  type Rating = Int

  def solvePartOne(inputPath: String): Unit = {
    val ratings = Util.loadLines(inputPath).map(_.toInt).sorted
    val differences = ratings.appended(ratings.last + 3) zip ratings.prepended(0) map { case (x, y) => x - y }
    val differencesDistribution = differences.groupBy(identity).mapValues(_.length).toMap
    println(differencesDistribution(1) * differencesDistribution(3))
  }

  def solvePartTwo(inputPath: String): Unit = {
    val ratings = Util.loadLines(inputPath).map(_.toInt).sorted
    var numberOfPaths = Map(ratings.last -> 1L).withDefaultValue(0L)
    for {
      current <- ratings.reverse
      i <- 1 to 3
      predecessor = current - i
      predecessorPaths = numberOfPaths(predecessor) + numberOfPaths(current)
    } numberOfPaths += (predecessor -> predecessorPaths)
    println(numberOfPaths(0))
  }

  println("Part One")
  solvePartOne("day10/example.txt")
  solvePartOne("day10/example2.txt")
  solvePartOne("day10/puzzle.txt")
  println()
  println("Part Two")
  solvePartTwo("day10/example.txt")
  solvePartTwo("day10/example2.txt")
  solvePartTwo("day10/puzzle.txt")

}