package aoc.day5

import aoc.day5.Seat.SeatId
import aoc.util.Util

import scala.util.chaining.scalaUtilChainingOps

case class Seat(row: Int, column: Int) {
  val id: SeatId = 8 * row + column
}

object Seat {

  type SeatId = Int

  def decode(pass: String): Seat = {
    val (rowPart, columnPart) = pass.splitAt(7)
    val row = parseBinary(rowPart, zero = 'F', one = 'B')
    val column = parseBinary(columnPart, zero = 'L', one = 'R')
    Seat(row, column)
  }

  private def parseBinary(s: String, zero: Char, one: Char): Int =
    s.replace(zero, '0')
      .replace(one, '1')
      .pipe(Integer.parseInt(_, 2))

}

object Day5 extends App {

  def solvePartOne(inputPath: String): Unit = {
    println(loadSeatIds(inputPath).max)
  }

  def solvePartTwo(inputPath: String): Unit = {
    val allIds = loadSeatIds(inputPath)
    val mySeatIds =
      for {
        id <- allIds.min to allIds.max
        if allIds.contains(id - 1)
        if !allIds.contains(id)
        if allIds.contains(id + 1)
      } yield id
    println(mySeatIds)
  }

  private def loadSeatIds(inputPath: String): Set[SeatId] =
    Util.loadLines(inputPath).map(Seat.decode).map(_.id).toSet

  println("Part One")
  solvePartOne("day5/example.txt")
  solvePartOne("day5/puzzle.txt")
  println()
  println("Part Two")
  solvePartTwo("day5/example.txt")
  solvePartTwo("day5/puzzle.txt")

}