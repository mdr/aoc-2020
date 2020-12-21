package aoc.day11

import aoc.util.Util
import aoc.util.Util.iterateUntilSteadyState

import scala.annotation.tailrec

sealed trait State

case object Empty extends State

case object Floor extends State

case object Occupied extends State

case class Direction(row: Int, column: Int)

object Direction {

  val All: Seq[Direction] =
    for {
      row <- -1 to 1
      column <- -1 to 1
      if row != 0 || column != 0
    } yield Direction(row, column)
}


case class Position(row: Int, column: Int) {

  def +(direction: Direction): Position = Position(row + direction.row, column + direction.column)

  def neighbours: Seq[Position] = for {direction <- Direction.All} yield this + direction

}

case class SeatingArrangement(states: Seq[Seq[State]]) {

  def apply(position: Position): Option[State] = states.lift(position.row).flatMap(_.lift(position.column))

  private def get(position: Position): State = apply(position).getOrElse(throw new AssertionError(s"Invalid position $position"))

  override def toString: String = states.map(_.map {
    case Occupied => '#'
    case Empty => 'L'
    case Floor => '.'
  }.mkString).mkString("\n")

  val height: Int = states.length

  val width: Int = states.head.length

  private def evolveWith(f: Position => State): SeatingArrangement =
    SeatingArrangement(0.until(height).map(row => 0.until(width).map(column => f(Position(row, column)))))

  def evolve: SeatingArrangement = evolveWith(nextState)

  private def nextState(position: Position): State = {
    val adjacent = getNeighbourSeatStates(position)
    get(position) match {
      case Empty if !(adjacent contains Occupied) => Occupied
      case Occupied if adjacent.count(_ == Occupied) >= 4 => Empty
      case state => state
    }
  }

  private def getNeighbourSeatStates(position: Position): Seq[State] = position.neighbours.flatMap(apply)

  def evolve2: SeatingArrangement = evolveWith(nextState2)

  private def nextState2(position: Position): State = {
    val adjacent = getVisibleSeatStates(position)
    get(position) match {
      case Empty if !(adjacent contains Occupied) => Occupied
      case Occupied if adjacent.count(_ == Occupied) >= 5 => Empty
      case state => state
    }
  }

  private def getVisibleSeatStates(position: Position): Seq[State] =
    Direction.All.map(direction => traceLineOfSight(position, direction))

  @tailrec
  private def traceLineOfSight(position: Position, direction: Direction): State = {
    val nextPosition = position + direction
    apply(nextPosition) match {
      case Some(Occupied) => Occupied
      case Some(Empty) => Empty
      case Some(Floor) => traceLineOfSight(nextPosition, direction)
      case None => Floor
    }
  }

  def count(state: State): Int = states.flatten.count(_ == state)
}

object SeatingArrangement {

  private def parse(line: String): Seq[State] = line.map {
    case '.' => Floor
    case '#' => Occupied
    case 'L' => Empty
  }

  def parse(lines: Seq[String]): SeatingArrangement = SeatingArrangement(lines.map(parse))

}

object Day11 extends App {

  def solvePartOne(inputPath: String): Unit = {
    val arrangement = loadArrangement(inputPath)
    val stableArrangement = iterateUntilSteadyState(arrangement)(_.evolve)
    println(stableArrangement.count(Occupied))
  }

  def solvePartTwo(inputPath: String): Unit = {
    val arrangement = loadArrangement(inputPath)
    val stableArrangement = iterateUntilSteadyState(arrangement)(_.evolve2)
    println(stableArrangement.count(Occupied))
  }

  private def loadArrangement(inputPath: String): SeatingArrangement = SeatingArrangement.parse(Util.loadLines(inputPath))

  println("Part One")
  solvePartOne("day11/example.txt")
  solvePartOne("day11/puzzle.txt")
  println()
  println("Part Two")
  solvePartTwo("day11/example.txt")
  solvePartTwo("day11/puzzle.txt")

}