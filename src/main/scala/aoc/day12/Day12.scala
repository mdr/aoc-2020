package aoc.day12

import aoc.util.Util

case class Position(x: Long, y: Long) {

  def move(direction: Direction, amount: Long): Position = direction match {
    case Direction.North => copy(y = y + amount)
    case Direction.South => copy(y = y - amount)
    case Direction.East => copy(x = x + amount)
    case Direction.West => copy(x = x - amount)
  }

  def manhattanDistanceTo(that: Position): Long = (this.x - that.x).abs + (this.y - that.y).abs

  def *(n: Long): Position = copy(x = x * n, y = y * n)

  def +(that: Position): Position = copy(x = this.x + that.x, y = this.y + that.y)

  def rotate(degrees: Long): Position = {
    import math._
    val θ = toRadians(-degrees)
    val newX = x * cos(θ) - y * sin(θ)
    val newY = x * sin(θ) + y * cos(θ)
    copy(x = newX.round, y = newY.round)
  }
}

sealed trait Direction {

  def turn(degrees: Long): Direction =
    (degrees % 360 + 360) % 360 match {
      case 90 => turnRight
      case 180 => turnRight.turnRight
      case 270 => turnLeft
    }

  def turnLeft: Direction = this match {
    case Direction.North => Direction.West
    case Direction.South => Direction.East
    case Direction.East => Direction.North
    case Direction.West => Direction.South
  }

  def turnRight: Direction = this match {
    case Direction.North => Direction.East
    case Direction.South => Direction.West
    case Direction.East => Direction.South
    case Direction.West => Direction.North
  }

}

object Direction {

  case object North extends Direction

  case object South extends Direction

  case object East extends Direction

  case object West extends Direction

}


sealed trait Action

object Action {

  case object North extends Action

  case object South extends Action

  case object East extends Action

  case object West extends Action

  case object Left extends Action

  case object Right extends Action

  case object Forward extends Action

}

case class NavigationInstruction(action: Action, value: Long)

object NavigationInstruction {

  private def parseAction(s: String): Action = s match {
    case "N" => Action.North
    case "S" => Action.South
    case "E" => Action.East
    case "W" => Action.West
    case "L" => Action.Left
    case "R" => Action.Right
    case "F" => Action.Forward
  }

  def parse(s: String): NavigationInstruction = {
    val (first, rest) = s.splitAt(1)
    NavigationInstruction(parseAction(first), rest.toLong)
  }

}

case class Ship(position: Position = Position(0, 0), direction: Direction = Direction.East) {

  def move(direction: Direction, amount: Long): Ship = copy(position = position.move(direction, amount))

  def handle(instruction: NavigationInstruction): Ship = {
    val NavigationInstruction(action, value) = instruction
    action match {
      case Action.North => move(Direction.North, value)
      case Action.South => move(Direction.South, value)
      case Action.East => move(Direction.East, value)
      case Action.West => move(Direction.West, value)
      case Action.Left => copy(direction = direction.turn(-value))
      case Action.Right => copy(direction = direction.turn(value))
      case Action.Forward => move(direction, value)
    }

  }

}

case class Ship2(position: Position = Position(0, 0), waypoint: Position = Position(10, 1)) {

  private def moveWaypoint(direction: Direction, value: Long): Ship2 = copy(waypoint = waypoint.move(direction, value))

  def handle(instruction: NavigationInstruction): Ship2 = {
    val NavigationInstruction(action, value) = instruction
    action match {
      case Action.North => moveWaypoint(Direction.North, value)
      case Action.South => moveWaypoint(Direction.South, value)
      case Action.East => moveWaypoint(Direction.East, value)
      case Action.West => moveWaypoint(Direction.West, value)
      case Action.Left => copy(waypoint = waypoint.rotate(-value))
      case Action.Right => copy(waypoint = waypoint.rotate(value))
      case Action.Forward => copy(position = position + waypoint * value)
    }
  }

}

object Day12 extends App {

  def solvePartOne(inputPath: String): Unit = {
    val instructions = Util.loadLines(inputPath).map(NavigationInstruction.parse)
    val ship = instructions.foldLeft(Ship())(_ handle _)
    val distance = ship.position.manhattanDistanceTo(Position(0, 0))
    println(distance)
  }

  def solvePartTwo(inputPath: String): Unit = {
    val instructions = Util.loadLines(inputPath).map(NavigationInstruction.parse)
    val ship = instructions.foldLeft(Ship2())(_ handle _)
    val distance = ship.position.manhattanDistanceTo(Position(0, 0))
    println(distance)
  }

  println("Part One")
  solvePartOne("day12/example.txt")
  solvePartOne("day12/puzzle.txt")
  println()
  println("Part Two")
  solvePartTwo("day12/example.txt")
  solvePartTwo("day12/puzzle.txt")

}