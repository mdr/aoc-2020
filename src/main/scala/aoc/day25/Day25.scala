package aoc.day25

import aoc.util.Util

import scala.math.BigInt.int2bigInt

object Day25 extends App {

  val base = 7
  val modulus = 20201227

  def discreteLog(a: BigInt, b: BigInt, n: BigInt = modulus): BigInt =
    LazyList.from(1).filter(x => a.modPow(x, n) == b).head

  def solvePartOne(inputPath: String): Unit = {
    val Array(publicKey1, publicKey2) = Util.loadString(inputPath).split("\n").map(BigInt(_))

    val secretKey1 = discreteLog(base, publicKey1)
    val secretKey2 = discreteLog(base, publicKey2)
    println(publicKey1.modPow(secretKey2, modulus))
  }

  def solvePartTwo(inputPath: String): Unit = {
  }

  println("Part One")
  solvePartOne("day25/example.txt")
  solvePartOne("day25/puzzle.txt")
  println()
  println("Part Two")
  solvePartTwo("day25/example.txt")
  solvePartTwo("day25/puzzle.txt")

}