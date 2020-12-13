package aoc.day13

import aoc.util.Util

import scala.util.chaining.scalaUtilChainingOps

object NumberTheory {

  def extendedEuclid(a: BigInt, b: BigInt): (BigInt, BigInt, BigInt) =
    if (b == 0) (a, 1, 0)
    else {
      val (d1, x1, y1) = extendedEuclid(b, a % b)
      (d1, y1, x1 - a / b * y1)
    }
}

import NumberTheory._

case class Equation(rhs: BigInt, modulus: BigInt) {
  def normalise: BigInt = (rhs % modulus + modulus) % modulus

  def solve(that: Equation): Equation = {
    val Equation(a1, n1) = this
    val Equation(a2, n2) = that
    val (d, m1, m2) = extendedEuclid(n1, n2)
    if (d != 1) throw new AssertionError("Not coprime")
    Equation(a1 * m2 * n2 + a2 * m1 * n1, n1 * n2)
  }

}

object Day13 extends App {

  def solvePartOne(inputPath: String): Unit = {
    val (earliestTime, busIds) = loadProblem(inputPath)
    val busesToCatch =
      for {
        time <- LazyList.from(earliestTime)
        departingBuses = busIds.filter(time % _ == 0)
        busId <- departingBuses
      } yield time -> busId
    val (time, busId) = busesToCatch.head
    val minutesToWait = time - earliestTime
    println(minutesToWait * busId)
  }

  private def loadProblem(inputPath: String): (Int, Seq[Int]) = {
    val Seq(first, second, _*) = Util.loadLines(inputPath)
    val target = first.toInt
    val busIds = second.split(",").filterNot(_ == "x").map(_.toInt).toSeq
    (target, busIds)
  }

  case class Constraint(offset: Long, divisor: Long)

  def solvePartTwo(inputPath: String): Unit = {
    Util.loadLines(inputPath)(1)
      .split(",")
      .zipWithIndex
      .collect { case (busId, i) if busId != "x" => Equation(BigInt(-i), BigInt(busId)) }
      .reduce(_ solve _)
      .pipe(_.normalise)
      .tap(println)
  }


  println("Part One")
  solvePartOne("day13/example.txt")
  solvePartOne("day13/puzzle.txt")
  println()
  println("Part Two")
  solvePartTwo("day13/example.txt")
  solvePartTwo("day13/example2.txt")
  solvePartTwo("day13/example3.txt")
  solvePartTwo("day13/example4.txt")
  solvePartTwo("day13/example5.txt")
  solvePartTwo("day13/example6.txt")
  solvePartTwo("day13/puzzle.txt")

}