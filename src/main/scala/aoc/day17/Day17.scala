package aoc.day17

import aoc.util.Util
import aoc.util.Util.{RichSeq, RichSeqOfSeq}

case class Position(coordinates: Seq[Int]) {

  def dimensions: Int = coordinates.length

  def neighbours: Seq[Position] =
    for {
      delta <- Seq.fill(dimensions)(-1 to 1).sequence
      if delta != Seq.fill(dimensions)(0)
      newCoordinates = coordinates.lazyZip(delta) map (_ + _)
    } yield Position(newCoordinates)

}

case class Grid(activeCells: Set[Position], dimensions: Int) {

  def allPositions: Seq[Position] = (0 until dimensions).traverse(coordinateRange).map(Position)

  private def coordinateRange(dimension: Int): Seq[Int] = {
    val coordinateValues = activeCells.map(_.coordinates(dimension))
    coordinateValues.min - 1 to coordinateValues.max + 1
  }

  def cycle: Grid = {
    var nextActiveCells = activeCells
    for {
      position <- allPositions
      isActive = activeCells(position)
      activeNeighbourCount = position.neighbours count activeCells
    } {
      if (isActive && !(activeNeighbourCount == 2 || activeNeighbourCount == 3)) {
        nextActiveCells -= position
      }
      if (!isActive && activeNeighbourCount == 3) {
        nextActiveCells += position
      }
    }
    copy(activeCells = nextActiveCells)
  }

  def activeCellCount: Int = activeCells.size

}

object Grid {

  def parse(s: String, dimensions: Int): Grid = {
    val activeCells =
      for {
        (row, y) <- s.split("\n").zipWithIndex
        (c, x) <- row.zipWithIndex
        if c == '#'
      } yield
        if (dimensions == 3)
          Position(Seq(x, y, 0))
        else
          Position(Seq(x, y, 0, 0))
    Grid(activeCells.toSet, dimensions)
  }

}

object Day17 extends App {

  def solvePartOne(inputPath: String): Unit = solve(inputPath, dimensions = 3)

  def solvePartTwo(inputPath: String): Unit = solve(inputPath, dimensions = 4)

  private def solve(inputPath: String, dimensions: Int): Unit = {
    val grid = Grid.parse(Util.loadString(inputPath), dimensions)
    val finalGrid = Util.iterate(grid, 6)(_.cycle)
    println(finalGrid.activeCellCount)
  }

  println("Part One")
  solvePartOne("day17/example.txt")
  solvePartOne("day17/puzzle.txt")
  println()
  println("Part Two")
  solvePartTwo("day17/example.txt")
  solvePartTwo("day17/puzzle.txt")

}