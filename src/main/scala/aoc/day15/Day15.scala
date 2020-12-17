package aoc.day15

import aoc.util.Util


object Day15 extends App {

  def solvePartOne(inputPath: String): Unit = solve(inputPath, 2020)
  def solvePartTwo(inputPath: String): Unit = solve(inputPath, 30000000)

  def solve(inputPath: String, lastTurn: Int): Unit = {
    val startingNumbers = Util.loadLines(inputPath).head.split(",").map(_.toInt).toSeq
    var turnPreviouslySpoken: Map[Int, Int] = Map.empty.withDefaultValue(0)
    var lastNumberSpoken = -1

    def takeTurn(turn: Int): Int = {
      val previousTurn = turn - 1
      val numberSpoken =
        if (turn <= startingNumbers.length) {
          startingNumbers(turn - 1)
        } else {
          val previousOccurrenceTurn = turnPreviouslySpoken(lastNumberSpoken)
          if (previousOccurrenceTurn == 0)
            0
          else
            previousTurn - previousOccurrenceTurn
        }
      if (lastNumberSpoken >= 0) {
        turnPreviouslySpoken += lastNumberSpoken -> previousTurn
      }
      lastNumberSpoken = numberSpoken
      numberSpoken
    }

    for (turn <- 1 to lastTurn) takeTurn(turn)
    println(lastNumberSpoken)
  }

  println("Part One")
  solvePartOne("day15/example.txt")
  solvePartOne("day15/puzzle.txt")
  println()
  println("Part Two")
  solvePartTwo("day15/example.txt")
  solvePartTwo("day15/puzzle.txt")

}