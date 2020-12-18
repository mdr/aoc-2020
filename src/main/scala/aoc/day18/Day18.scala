package aoc.day18

import aoc.util.Util

import scala.util.chaining.scalaUtilChainingOps
import scala.util.parsing.combinator.RegexParsers

trait ExpressionParsers extends RegexParsers {

  val expression: Parser[Long]

  lazy val number: Parser[Long] = """(0|[1-9]\d*)""".r ^^ (_.toLong)

  def apply(s: String): Long =
    parse(expression, s) match {
      case Success(expr, _) => expr
      case Failure(msg, _) => throw new RuntimeException(msg)
      case Error(msg, _) => throw new RuntimeException(msg)
    }

  lazy val expr0: Parser[Long] = number | ("(" ~> expression <~ ")")

}

object ExpressionParser extends ExpressionParsers {

  lazy val expression: Parser[Long] = expr0 ~ rep(("+" | "*") ~ expr0) ^^ { case lhs ~ rhss =>
    rhss.foldLeft(lhs) {
      case (x, "+" ~ y) => x + y
      case (x, "*" ~ y) => x * y
    }
  }

}

object ExpressionParser2 extends ExpressionParsers {

  lazy val expression: Parser[Long] = addExpr ~ rep("*" ~ addExpr) ^^ { case lhs ~ rhss =>
    rhss.foldLeft(lhs) { case (x, "*" ~ y) => x * y }
  }

  lazy val addExpr: Parser[Long] = expr0 ~ rep("+" ~ expr0) ^^ { case lhs ~ rhss =>
    rhss.foldLeft(lhs) { case (x, "+" ~ y) => x + y }
  }

}

object Day18 extends App {

  def solvePartOne(inputPath: String): Unit = solve(inputPath, ExpressionParser.apply)

  def solvePartTwo(inputPath: String): Unit = solve(inputPath, ExpressionParser2.apply)

  private def solve(inputPath: String, parser: String => Long): Unit =
    Util.loadLines(inputPath).map(parser).sum.tap(println)

  println("Part One")
  solvePartOne("day18/example.txt")
  solvePartOne("day18/puzzle.txt")
  println()
  println("Part Two")
  solvePartTwo("day18/example.txt")
  solvePartTwo("day18/puzzle.txt")

}