package aoc.day7

import aoc.day7.Day7.{Colour, ShinyGold}
import aoc.util.Util
import aoc.util.Util.SumByOperation

import scala.collection.immutable.MultiDict


case class Rule(colour: Colour, contentsSpecs: Seq[ContentsSpec])

case class ContentsSpec(amount: Int, colour: Colour)

class Walk(edges: Map[Colour, Set[Colour]]) {

  private var visited: Set[Colour] = Set.empty

  def walk(): Set[Colour] = {
    walk(ShinyGold)
    visited
  }

  private def walk(colour: Colour): Unit = {
    if (!visited(colour)) {
      visited += colour
      edges.getOrElse(colour, Set.empty).foreach(walk(_))
    }
  }

}

object Day7 extends App {

  type Colour = String

  val ShinyGold = "shiny gold"

  val ContentsSpecPattern = """^(\d+) ([a-z ]+) bags?$""".r

  def parseContentsSpec(s: String): ContentsSpec = {
    val ContentsSpecPattern(amount, colour) = s
    ContentsSpec(amount.toInt, colour)
  }

  def parseRule(line: String): Rule = {
    val parts = line.split(" bags contain ")
    val colour = parts(0)
    val rest = parts(1).dropRight(1) // full stop at end
    val contentsSpecs: Seq[ContentsSpec] =
      if (rest == "no other bags") {
        Seq.empty
      } else {
        rest.split(", ").map(parseContentsSpec)
      }
    Rule(colour, contentsSpecs)
  }

  private def invertRules(rules: Seq[Rule]): Map[Colour, Set[Colour]] = {
    val pairs =
      for {
        Rule(colour1, contentsSpecs) <- rules
        ContentsSpec(_, colour2) <- contentsSpecs
      } yield colour2 -> colour1
    MultiDict.from(pairs).sets
  }

  def solvePartOne(inputPath: String): Unit = {
    val rules = Util.loadLines(inputPath).map(parseRule)
    val backwardsEdges = invertRules(rules)
    val reachedColours = new Walk(backwardsEdges).walk()
    println((reachedColours - ShinyGold).size)
  }

  def solvePartTwo(inputPath: String): Unit = {
    val rules = Util.loadLines(inputPath).map(parseRule)

    val ruleMap: Map[Colour, Seq[ContentsSpec]] = (for (Rule(colour, contentsSpecs) <- rules) yield colour -> contentsSpecs).toMap
    println(numberOfBagsInside(ShinyGold, ruleMap))
  }

  private def numberOfBagsInside(colour: Colour, ruleMap: Map[Colour, Seq[ContentsSpec]]): Int = {
    val contentsSpecs = ruleMap(colour)
    if (contentsSpecs.isEmpty) 0 else contentsSpecs.sumBy { case ContentsSpec(amount, colour) => amount * (numberOfBagsInside(colour, ruleMap) + 1) }
  }

  println("Part One")
  solvePartOne("day7/example.txt")
  solvePartOne("day7/puzzle.txt")
  println()
  println("Part Two")
  solvePartTwo("day7/example.txt")
  solvePartTwo("day7/example2.txt")
  solvePartTwo("day7/puzzle.txt")

}