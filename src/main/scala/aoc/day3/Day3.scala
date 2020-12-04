package aoc.day3

import aoc.util.Util.loadLines

// Top-left is (0, 0), y grows larger downwards
case class Position(x: Int, y: Int) {

  def +(slope: Slope): Position = Position(x + slope.right, y + slope.down)

}

case class Slope(right: Int, down: Int)

sealed trait Location

case object Tree extends Location

case object OpenSquare extends Location

case class Map(locations: Seq[Seq[Location]]) {
  assert(locations.map(_.length).distinct.size == 1)

  val width: Int = locations.map(_.length).head

  def apply(position: Position): Location = {
    assert(contains(position))
    val Position(x, y) = position
    val wrappedX = (x % width + width) % width
    locations(y)(wrappedX)
  }

  def contains(position: Position): Boolean = position.y >= 0 && position.y < locations.size

  def get(position: Position): Option[Location] = if (contains(position)) Some(apply(position)) else None

  override def toString: String = locations.map(rowToString(_)).mkString("\n")

  private def rowToString(row: Seq[Location], copies: Int = 3): String =
    row.map {
      case Tree => "#"
      case OpenSquare => "."
    }.mkString * copies
}

object Map {

  private def parseLine(line: String): Seq[Location] =
    line.map {
      case '.' => OpenSquare
      case '#' => Tree
      case c => throw new RuntimeException(s"Unexpected map character $c in $line")
    }

  def parse(lines: Seq[String]): Map = Map(lines.map(parseLine))

}

object Day3 extends App {

  def solvePartOne(inputPath: String): Unit = {
    val map = Map.parse(loadLines(inputPath))
    val slope = Slope(right = 3, down = 1)
    val treeCount = countTreesEncountered(map, slope)
    println(treeCount)
  }

  private def countTreesEncountered(map: Map, slope: Slope): Int = {
    val start = Position(0, 0) + slope
    val locations = Seq.unfold(start)((position: Position) =>
      map.get(position).map(location => (location, position + slope))
    )
    locations.count(_ == Tree)
  }

  def solvePartTwo(inputPath: String): Unit = {
    val map = Map.parse(loadLines(inputPath))
    val slopes = Seq(
      Slope(right = 1, down = 1),
      Slope(right = 3, down = 1),
      Slope(right = 5, down = 1),
      Slope(right = 7, down = 1),
      Slope(right = 1, down = 2),
    )
    println(slopes.map(countTreesEncountered(map, _).toLong).product)
  }


  println("Part One")
  solvePartOne("day3/example.txt")
  solvePartOne("day3/puzzle.txt")
  println()
  println("Part Two")
  solvePartTwo("day3/example.txt")
  solvePartTwo("day3/puzzle.txt")

}