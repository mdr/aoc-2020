package aoc.day19

import aoc.day19.Rule.RuleId
import aoc.util.Util

sealed trait Rule

case class CharacterRule(c: Char) extends Rule

case class SequenceRule(ruleIds: Seq[RuleId]) extends Rule

case class AlternationRule(ruleIds1: Seq[RuleId], ruleIds2: Seq[RuleId]) extends Rule

object Rule {

  type RuleId = Int

  def parse(s: String): (RuleId, Rule) = {
    val Array(ruleId, rest) = s.split(": ")
    val rule =
      if (rest.startsWith("\""))
        CharacterRule(rest(1))
      else if (rest.contains("|")) {
        val Array(first, second) = rest.split(" \\| ")
        val ruleIds1 = first.split(" ").map(_.toInt)
        val ruleIds2 = second.split(" ").map(_.toInt)
        AlternationRule(ruleIds1, ruleIds2)
      } else {
        val ruleIds = rest.split(" ").map(_.toInt)
        SequenceRule(ruleIds)
      }
    ruleId.toInt -> rule
  }

}

case class Validator(rules: Map[RuleId, Rule]) {

  private class ValidationContext(rules: Map[RuleId, Rule], message: String) {

    def validate: Boolean = validate() contains message.length

    private def validate(ruleId: RuleId = 0, pos: Int = 0): Seq[Int] =
      rules(ruleId) match {
        case CharacterRule(c) => message.lift(pos).filter(_ == c).map(_ => pos + 1).toSeq
        case SequenceRule(ruleIds) => validateSequence(ruleIds, pos)
        case AlternationRule(ruleIds1, ruleIds2) => validateSequence(ruleIds1, pos) ++ validateSequence(ruleIds2, pos)
      }

    private def validateSequence(ruleIds: Seq[RuleId], pos: Int): Seq[Int] =
      ruleIds match {
        case Seq() => Seq(pos)
        case Seq(first, rest@_*) => validate(first, pos) flatMap (validateSequence(rest, _))
      }
  }

  def isValid(message: String): Boolean = new ValidationContext(rules, message).validate

}

object Day19 extends App {

  def solvePartOne(inputPath: String): Unit = {
    val (rules, messages) = parseRulesAndMessages(inputPath)
    val validator = Validator(rules)
    println(messages.count(validator.isValid))
  }

  def solvePartTwo(inputPath: String): Unit = {
    val (originalRules, messages) = parseRulesAndMessages(inputPath)
    val overrideRules =
      parseRules(
        """8: 42 | 42 8
          |11: 42 31 | 42 11 31""".stripMargin)
    val rules = originalRules ++ overrideRules
    val validator = Validator(rules)

    println(messages.count(validator.isValid))
  }

  private def parseRulesAndMessages(inputPath: String): (Map[RuleId, Rule], Seq[String]) = {
    val Array(section1, section2) = Util.loadString(inputPath).split("\n\n")
    val rules = parseRules(section1)
    val messages = section2.split("\n").toSeq
    (rules, messages)
  }

  private def parseRules(rulesSection: String): Map[RuleId, Rule] = rulesSection.split("\n").map(Rule.parse).toMap

  println("Part One")
  solvePartOne("day19/example.txt")
  solvePartOne("day19/puzzle.txt")
  println()
  println("Part Two")
  solvePartTwo("day19/example2.txt")
  solvePartTwo("day19/puzzle.txt")

}