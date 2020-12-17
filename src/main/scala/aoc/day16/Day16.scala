package aoc.day16

import aoc.util.Util

import scala.collection.immutable.MultiDict

object FieldRule {

  def parse(s: String): FieldRule = {
    val Array(field, rest) = s.split(": ")
    val Array(range1, range2) = rest.split(" or ")
    FieldRule(field, Range.parse(range1), Range.parse(range2))
  }

}

case class FieldRule(field: String, range1: Range, range2: Range) {
  def isValid(value: Int): Boolean = range1.contains(value) || range2.contains(value)

}

object Range {

  def parse(s: String): Range = {
    val Seq(first, second) = s.split("-").toSeq
    Range(first.toInt, second.toInt)
  }

}

case class Range(from: Int, to: Int) {
  def contains(value: Int): Boolean = from <= value && value <= to

}

object Ticket {

  def parse(s: String): Ticket = Ticket(s.split(",").map(_.toInt).toSeq)

}

case class Ticket(values: Seq[Int]) {
  def get(n: Int): Int = values(n)
}

class RuleChecker(rules: Seq[FieldRule]) {

  def isValid(value: Int): Boolean = rules.exists(_.isValid(value))

  def isValid(ticket: Ticket): Boolean = ticket.values.forall(isValid)

}

object Day16 extends App {

  def solvePartOne(inputPath: String): Unit = {
    val (rules, _, nearbyTickets) = parseInput(inputPath)
    val ruleChecker = new RuleChecker(rules)
    println(nearbyTickets.flatMap(_.values).filterNot(ruleChecker.isValid).sum)
  }

  def solvePartTwo(inputPath: String): Unit = {
    val (rules, myTicket, nearbyTickets) = parseInput(inputPath)
    val ruleChecker = new RuleChecker(rules)
    val validTickets = nearbyTickets.filter(ruleChecker.isValid)

    val allPositions = myTicket.values.indices
    val pairs: Seq[(Int, FieldRule)] =
      for {
        pos <- allPositions
        candidateRules = rules.filter(couldRuleBeForPosition(_, pos, validTickets))
        candidateRule <- candidateRules
      } yield pos -> candidateRule

    var options: MultiDict[Int, FieldRule] = MultiDict.from(pairs)
    var concluded: Map[Int, FieldRule] = Map.empty

    while (concluded.size != allPositions.size) {
      val uniquelyDetermined = options.sets.collectFirst { case (pos, rules) if rules.size == 1 => pos -> rules.head }
      val (pos, rule) = uniquelyDetermined getOrElse (throw new AssertionError("No unique options found"))
      concluded += (pos -> rule)
      for (pos <- allPositions) {
        options -= (pos -> rule)
      }
    }

    val departureValues =
      for ((pos, rule) <- concluded if rule.field startsWith "departure")
        yield myTicket.get(pos)
    println(departureValues.map(_.toLong).product)

  }

  private def couldRuleBeForPosition(rule: FieldRule, n: Int, tickets: Seq[Ticket]): Boolean =
    tickets.map(_.get(n)).forall(rule.isValid)

  private def parseInput(inputPath: String): (Seq[FieldRule], Ticket, Seq[Ticket]) = {
    val Array(section1, section2, section3) = Util.loadString(inputPath).split("\n\n")
    val rules = section1.split("\n").map(FieldRule.parse).toSeq
    val myTicket = Ticket.parse(section2.split("\n")(1))
    val nearbyTickets = section3.split("\n").tail.map(Ticket.parse).toSeq
    (rules, myTicket, nearbyTickets)
  }

  println("Part One")
  solvePartOne("day16/example.txt")
  solvePartOne("day16/puzzle.txt")
  println()
  println("Part Two")
  solvePartTwo("day16/example2.txt")
  solvePartTwo("day16/puzzle.txt")

}