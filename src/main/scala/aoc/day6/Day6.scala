package aoc.day6

import aoc.util.Util
import aoc.util.Util.SumByOperation

case class PersonAnswers(answers: Set[Char])

case class GroupAnswers(personAnswers: Seq[PersonAnswers]) {
  def count: Int = personAnswers.map(_.answers).reduce(_ ++ _).size
  def count2: Int = personAnswers.map(_.answers).reduce(_ intersect _).size
}

object Day6 extends App {

  def loadAnswers(inputPath: String): Seq[GroupAnswers] = {
    val s = Util.loadString(inputPath)
    s.split("\n\n").map(parseGroup)
  }

  private def parseGroup(group: String): GroupAnswers =
    GroupAnswers(group.split("\n").map(parsePerson))

  private def parsePerson(person: String): PersonAnswers = PersonAnswers(person.toSet)

  def solvePartOne(inputPath: String): Unit = {
    val count = loadAnswers(inputPath).sumBy(_.count)
    println(count)
  }

  def solvePartTwo(inputPath: String): Unit = {
    val count = loadAnswers(inputPath).sumBy(_.count2)
    println(count)
  }

  println("Part One")
  solvePartOne("day6/example.txt")
  solvePartOne("day6/puzzle.txt")
  println()
  println("Part Two")
  solvePartTwo("day6/example.txt")
  solvePartTwo("day6/puzzle.txt")

}