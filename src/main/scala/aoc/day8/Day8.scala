package aoc.day8

import aoc.util.Util

import scala.util.matching.Regex

sealed trait Instruction

object Instruction {

  val InstructionRegex: Regex = """([a-z]+) ([-+]\d+)""".r

  def parse(s: String): Instruction = {
    val InstructionRegex(op, arg) = s
    op match {
      case "acc" => Acc(arg.toInt)
      case "jmp" => Jmp(arg.toInt)
      case "nop" => Nop(arg.toInt)
    }
  }

}

case class Acc(n: Int) extends Instruction

case class Jmp(n: Int) extends Instruction

case class Nop(n: Int) extends Instruction

sealed trait ExecutionResult

case class Loop(accumulator: Int) extends ExecutionResult

case class Terminates(accumulator: Int) extends ExecutionResult

class Machine(instructions: Seq[Instruction]) {
  private var instructionPointer: Int = 0
  private var accumulator = 0
  private var executedInstructions: Set[Int] = Set.empty

  def run(): ExecutionResult = step() getOrElse run()

  private def step(): Option[ExecutionResult] = {
    if (executedInstructions contains instructionPointer) {
      return Some(Loop(accumulator))
    }
    if (instructionPointer >= instructions.length) {
      return Some(Terminates(accumulator))
    }
    val instruction = instructions(instructionPointer)
    executedInstructions += instructionPointer
    instruction match {
      case Acc(n) =>
        accumulator += n
        instructionPointer += 1
      case Jmp(n) =>
        instructionPointer += n
      case Nop(_) =>
        instructionPointer += 1
    }
    None
  }

}

case class Program(instructions: Seq[Instruction]) {

  def run: ExecutionResult = new Machine(instructions).run()

  def mutations: Seq[Program] = instructions.zipWithIndex.collect {
    case (Nop(n), index) => this.mutate(index, Jmp(n))
    case (Jmp(n), index) => this.mutate(index, Nop(n))
  }

  def mutate(index: Int, newInstruction: Instruction): Program = Program(instructions.updated(index, newInstruction))

}

object Day8 extends App {

  def solvePartOne(inputPath: String): Unit = {
    val program = loadProgram(inputPath)
    println(program.run)
  }

  def solvePartTwo(inputPath: String): Unit = {
    val program = loadProgram(inputPath)
    val accumulator = program.mutations.map(_.run).collect { case Terminates(n) => n }
    println(accumulator)
  }

  private def loadProgram(inputPath: String) = Program(Util.loadLines(inputPath).map(Instruction.parse))

  println("Part One")
  solvePartOne("day8/example.txt")
  solvePartOne("day8/puzzle.txt")
  println()
  println("Part Two")
  solvePartTwo("day8/example.txt")
  solvePartTwo("day8/puzzle.txt")

}