package aoc.day18

import aoc.util.Util

import scala.util.chaining.scalaUtilChainingOps
import scala.util.parsing.combinator.RegexParsers

sealed trait Expression {
  def eval: Long = this match {
    case Number(n) => n
    case AddExpr(left, right) => left.eval + right.eval
    case MultExpr(left, right) => left.eval * right.eval
  }
}

case class Number(n: Long) extends Expression

case class AddExpr(left: Expression, right: Expression) extends Expression

case class MultExpr(left: Expression, right: Expression) extends Expression

trait ExpressionParsers extends RegexParsers {

  val expression: Parser[Expression]

  lazy val number: Parser[Number] = """(0|[1-9]\d*)""".r ^^ (s => Number(s.toLong))

  def apply(s: String): Expression =
    parse(expression, s) match {
      case Success(expr, _) => expr
      case Failure(msg, _) => throw new RuntimeException(msg)
      case Error(msg, _) => throw new RuntimeException(msg)
    }

  lazy val expr0: Parser[Expression] = number | ("(" ~> expression <~ ")")

}

object ExpressionParser extends ExpressionParsers {

  lazy val expression: Parser[Expression] = expr0 ~ rep(("+" | "*") ~ expr0) ^^ { case lhs ~ rhss =>
    rhss.foldLeft(lhs) {
      case (x, "+" ~ y) => AddExpr(x, y)
      case (x, "*" ~ y) => MultExpr(x, y)
    }
  }

}

object ExpressionParser2 extends ExpressionParsers {

  lazy val expression: Parser[Expression] = addExpr ~ rep("*" ~ addExpr) ^^ { case lhs ~ rhss =>
    rhss.foldLeft(lhs) { case (x, "*" ~ y) => MultExpr(x, y) }
  }

  lazy val addExpr: Parser[Expression] = expr0 ~ rep("+" ~ expr0) ^^ { case lhs ~ rhss =>
    rhss.foldLeft(lhs) { case (x, "+" ~ y) => AddExpr(x, y) }
  }

}

object Day18 extends App {

  def solvePartOne(inputPath: String): Unit = solve(inputPath, ExpressionParser.apply)

  def solvePartTwo(inputPath: String): Unit = solve(inputPath, ExpressionParser2.apply)

  private def solve(inputPath: String, parser: String => Expression): Unit =
    Util.loadLines(inputPath).map(parser).map(_.eval).sum.tap(println)

  println("Part One")
  solvePartOne("day18/example.txt")
  solvePartOne("day18/puzzle.txt")
  println()
  println("Part Two")
  solvePartTwo("day18/example.txt")
  solvePartTwo("day18/puzzle.txt")

}