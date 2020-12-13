package aoc.day9

import aoc.util.Util

object Day9 extends App {

  private def sumExists(n: Long, candidates: Seq[Long]): Boolean =
    candidates.combinations(2).exists(_.sum == n)

  def solvePartOne(inputPath: String, windowSize: Int): Unit = {
    val n = findFirstInvalidNumber(inputPath, windowSize)
    println(n)
  }

  private def findFirstInvalidNumber(inputPath: String, windowSize: Int): Long =
    Util.loadLines(inputPath)
      .map(_.toLong)
      .sliding(windowSize + 1)
      .map(ns => (ns.last, ns.init))
      .collectFirst { case (n, candidates) if !sumExists(n, candidates) => n }
      .head

  def solvePartTwo(inputPath: String, windowSize: Int): Unit = {
    val n = findFirstInvalidNumber(inputPath, windowSize)
    val numbers = Util.loadLines(inputPath).map(_.toLong)
    2.to(numbers.length)
      .flatMap(numbers.sliding)
      .collectFirst { case xs if xs.sum == n => xs.min + xs.max }
      .foreach(println)
  }

  println("Part One")
  solvePartOne("day9/example.txt", 5)
  solvePartOne("day9/puzzle.txt", 25)
  println()
  println("Part Two")
  solvePartTwo("day9/example.txt", windowSize = 5)
  solvePartTwo("day9/puzzle.txt", windowSize = 25)

}