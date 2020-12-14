package aoc.day14

import aoc.util.Util
import java.lang.Long.parseLong

sealed trait Mode

object Mode {

  case object One extends Mode

  case object Two extends Mode

}

case class State(mask: Mask = Mask.Initial, memory: Map[Long, Long] = Map(), mode: Mode) {

  def run(instruction: Instruction): State =
    instruction match {
      case AssignmentInstruction(address, value) if mode == Mode.One =>
        copy(memory = memory + (address -> mask.apply1(value)))
      case AssignmentInstruction(address, value) if mode == Mode.Two =>
        val baseAddress = mask.apply2(address)
        var newMemory = memory
        for (floaters <- mask.floaters.subsets)
          newMemory = newMemory + (baseAddress + floaters.sum -> value)
        copy(memory = newMemory)
      case SetMask(mask) =>
        copy(mask = mask)
    }

  def memorySum: Long = memory.values.sum

}

case class Mask(zeroMask: Long, oneMask: Long, floatMask: Long, floaters: Set[Long]) {

  def apply1(value: Long): Long = value & zeroMask | oneMask

  def apply2(value: Long): Long = value & floatMask | oneMask

}

object Mask {

  val Initial: Mask = Mask(0, 0, 0, Set.empty)

  def parse(s: String): Mask =
    Mask(
      zeroMask = parseLong(s.replaceAll("X", "1"), 2),
      oneMask = parseLong(s.replaceAll("X", "0"), 2),
      floatMask = parseLong(s.replaceAll("0", "1").replaceAll("X", "0"), 2),
      floaters = s.reverse.zipWithIndex.collect { case ('X', i) => 1L << i }.toSet)

}

sealed trait Instruction

case class AssignmentInstruction(address: Long, value: Long) extends Instruction

case class SetMask(mask: Mask) extends Instruction

object Instruction {

  private val AssignmentRegex = """^mem\[(\d+)] = (\d+)$""".r
  private val MaskRegex = """^mask = ([01X]+)$""".r

  def parse(s: String): Instruction = {
    if (s.startsWith("mask")) {
      val MaskRegex(maskString) = s
      SetMask(Mask.parse(maskString))
    } else {
      val AssignmentRegex(addressString, valueString) = s
      AssignmentInstruction(addressString.toLong, valueString.toLong)
    }
  }

}

object Day14 extends App {

  def solvePartOne(inputPath: String): Unit = solve(inputPath, Mode.One)

  def solvePartTwo(inputPath: String): Unit = solve(inputPath, Mode.Two)

  private def solve(inputPath: String, mode: Mode): Unit = {
    val lines = Util.loadLines(inputPath)
    val instructions = lines.map(Instruction.parse)
    val initialState = State(mode = mode)
    val finalState = instructions.foldLeft(initialState)(_ run _)
    println(finalState.memorySum)
  }


  println("Part One")
  solvePartOne("day14/example.txt")
  solvePartOne("day14/puzzle.txt")
  println()
  println("Part Two")
  solvePartTwo("day14/example2.txt")
  solvePartTwo("day14/puzzle.txt")

}