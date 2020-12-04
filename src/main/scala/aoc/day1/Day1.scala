package aoc.day1

import aoc.util.Util.{RichSeqOfSeq, loadLines}

case class Answer(multiplicands: Seq[Int]) {
  val product: Int = multiplicands.product

  override def toString: String = multiplicands.mkString(" * ")
}

object Day1 extends App {

  def solvePartOne(inputPath: String): Unit = {
    val entries = loadLines(inputPath).map(_.toInt)
    val answer = findEntriesThatSum(entries, numberOfEntries = 2, targetSum = 2020)
    println(answer)
    println(s"$inputPath: ${answer.product}")
  }

  def findEntriesThatSum1(entries: Seq[Int], numberOfEntries: Int, targetSum: Int): Any =
    entries.combinations(3).filter(_.sum == 2020).map(_.product)

  def findEntriesThatSum2(entries: Seq[Int], numberOfEntries: Int, targetSum: Int): Answer =
    Seq.fill(numberOfEntries)(entries)
      .sequence
      .filter(_.sum == targetSum)
      .map(Answer)
      .headOption
      .getOrElse(throw new RuntimeException("No matching sum found"))

  def findEntriesThatSum(entries: Seq[Int], numberOfEntries: Int, targetSum: Int): Answer =
    entries
      .combinations(numberOfEntries)
      .filter(_.sum == targetSum)
      .map(Answer)
      .toSeq
      .headOption
      .getOrElse(throw new RuntimeException("No matching sum found"))

  private def findTwoEntriesThatSum(entries: Seq[Int], targetSum: Int): Answer = {
    val answers =
      for {
        n1 <- entries
        n2 <- entries
        if n1 + n2 == targetSum
      } yield Answer(Seq(n1, n2))
    answers.headOption.getOrElse(throw new RuntimeException("No matching sum found"))
  }

  private def findThreeEntriesThatSum(entries: Seq[Int], targetSum: Int): Answer = {
    val answers =
      for {
        n1 <- entries
        n2 <- entries
        n3 <- entries
        if n1 + n2 + n3 == targetSum
      } yield Answer(Seq(n1, n2, n3))
    answers.headOption.getOrElse(throw new RuntimeException("No matching sum found"))
  }

  def solvePartTwo(inputPath: String): Unit = {
    val entries = loadLines(inputPath).map(_.toInt)
    val answer = findEntriesThatSum(entries, numberOfEntries = 3, targetSum = 2020)
    println(answer)
    println(s"$inputPath: ${answer.product}")
  }

  println("Part One")
  solvePartOne("day1/example.txt")
  solvePartOne("day1/puzzle.txt")
  println()
  println("Part Two")
  solvePartTwo("day1/example.txt")
  solvePartTwo("day1/puzzle.txt")
}
