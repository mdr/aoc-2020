package aoc.day2

import scala.util.parsing.combinator._

import aoc.util.Util.loadLines

case class Policy(character: Char, minOccurrences: Int, maxOccurrences: Int) {

  def validate1(password: String): Boolean = {
    val count = password.count(_ == character)
    minOccurrences <= count && count <= maxOccurrences
  }

  def validate2(password: String): Boolean = {
    val correctAtFirstPosition = password(minOccurrences - 1) == character
    val correctAtSecondPosition = password(maxOccurrences - 1) == character
    correctAtFirstPosition ^ correctAtSecondPosition
  }

}

case class PolicyAndPassword(policy: Policy, password: String) {

  def isValid1: Boolean = policy.validate1(password)

  def isValid2: Boolean = policy.validate2(password)

}

object PolicyParser extends RegexParsers {
  lazy val policy: Parser[Policy] = (number <~ "-") ~ number ~ "[a-z]".r ^^ { case min ~ max ~ c => Policy(c.head, min, max) }

  lazy val number: Parser[Int] = """(0|[1-9]\d*)""".r ^^ (_.toInt)

  lazy val policyAndPassword: Parser[PolicyAndPassword] = (policy <~ ":") ~ "[a-z]+".r ^^ { case policy ~ password =>
    PolicyAndPassword(policy, password)
  }

  def apply(s: String): PolicyAndPassword =
    parse(policyAndPassword, s) match {
      case Success(policyAndPassword, _) => policyAndPassword
      case Failure(msg, _) => throw new RuntimeException(msg)
      case Error(msg, _) => throw new RuntimeException(msg)
    }
}


object Day2 extends App {
  def solvePartOne(inputPath: String): Unit = {
    val validCount = loadPolicyAndPasswords(inputPath).count(_.isValid1)
    println(validCount)
  }

  def solvePartTwo(inputPath: String): Unit = {
    val validCount = loadPolicyAndPasswords(inputPath).count(_.isValid2)
    println(validCount)
  }

  def loadPolicyAndPasswords(inputPath: String): Seq[PolicyAndPassword] = loadLines(inputPath).map(PolicyParser(_))

  println("Part One")
  solvePartOne("day2/example.txt")
  solvePartOne("day2/puzzle.txt")
  println()
  println("Part Two")
  solvePartTwo("day2/example.txt")
  solvePartTwo("day2/puzzle.txt")

}
