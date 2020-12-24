package aoc.day24


import aoc.util.Util

import scala.collection.MultiSet

sealed trait Direction

case object East extends Direction

case object Southeast extends Direction

case object Southwest extends Direction

case object West extends Direction

case object Northwest extends Direction

case object Northeast extends Direction

object Direction {

  val All = Seq(East, Southeast, Southwest, West, Northwest, Northeast)

  def parseDirections(s: String): Seq[Direction] =
    if (s.isEmpty)
      Seq.empty
    else if (s startsWith "e")
      East +: parseDirections(s.tail)
    else if (s startsWith "se")
      Southeast +: parseDirections(s drop 2)
    else if (s startsWith "sw")
      Southwest +: parseDirections(s drop 2)
    else if (s startsWith "w")
      West +: parseDirections(s.tail)
    else if (s startsWith "ne")
      Northeast +: parseDirections(s drop 2)
    else if (s startsWith "nw")
      Northwest +: parseDirections(s drop 2)
    else
      throw new AssertionError(s"Unexpected $s")
}

object Hex {

  def Origin: Hex = Hex(0, 0)

}

case class Hex(x: Int, y: Int) {

  def go(direction: Direction): Hex = direction match {
    case East => copy(x = x + 1)
    case Northeast => copy(x = x + 1, y = y - 1)
    case Northwest => copy(y = y - 1)
    case Southeast => copy(y = y + 1)
    case Southwest => copy(x = x - 1, y = y + 1)
    case West => copy(x = x - 1)
  }

  def go(directions: Seq[Direction]): Hex = directions.foldLeft(this)(_ go _)

  def neighbours: Seq[Hex] = Direction.All.map(go)

}

case class Floor(blackTiles: Set[Hex]) {

  private def blackNeighbourCount(hex: Hex): Int = hex.neighbours.count(blackTiles)

  def step: Floor = {
    val toFlipToWhite = blackTiles.filter(hex => blackNeighbourCount(hex) > 2 || blackNeighbourCount(hex) == 0)
    val allWhiteTiles = blackTiles.flatMap(_.neighbours) -- blackTiles
    val toFlipToBlack = allWhiteTiles.filter(blackNeighbourCount(_) == 2)
    Floor(blackTiles -- toFlipToWhite ++ toFlipToBlack)
  }

}

object Day24 extends App {

  def solvePartOne(inputPath: String): Floor = {
    val paths = Util.loadLines(inputPath).map(Direction.parseDirections)

    val blackTiles =
      for {
        (hex, count) <- MultiSet.from(paths.map(Hex.Origin.go)).occurrences
        if count % 2 == 1
      } yield hex

    println(blackTiles.size)
    Floor(blackTiles.toSet)
  }

  def solvePartTwo(floor: Floor): Unit = {
    val finalState = Util.iterate(floor, 100)(_.step)
    println(finalState.blackTiles.size)
  }

  println("Part One")
  val floor1 = solvePartOne("day24/example.txt")
  val floor2 = solvePartOne("day24/puzzle.txt")
  println()
  println("Part Two")
  solvePartTwo(floor1)
  solvePartTwo(floor2)

}