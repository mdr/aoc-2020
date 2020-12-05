package aoc.day4

import aoc.day4.Field.RichInt
import aoc.util.Util

sealed trait Field {
  def validate(value: String): Boolean
}

case object BirthYear extends Field {
  override def validate(value: String): Boolean = Field.validateYear(value, 1920, 2002)
}

case object IssueYear extends Field {
  override def validate(value: String): Boolean = Field.validateYear(value, 2010, 2020)
}

case object ExpirationYear extends Field {
  override def validate(value: String): Boolean = Field.validateYear(value, 2020, 2030)
}

case object Height extends Field {
  override def validate(value: String): Boolean = PartialFunction.cond(value) {
    case _ if value.endsWith("cm") => value.dropRight(2).toIntOption.exists(_.isInRange(150, 193))
    case _ if value.endsWith("in") => value.dropRight(2).toIntOption.exists(_.isInRange(59, 76))
  }
}

case object HairColour extends Field {
  override def validate(value: String): Boolean = value.matches("""#[0-9a-f]{6}""")
}

case object EyeColour extends Field {
  override def validate(value: String): Boolean = Set("amb", "blu", "brn", "gry", "grn", "hzl", "oth").contains(value)
}

case object PassportId extends Field {
  override def validate(value: String): Boolean = value.matches("""[0-9]{9}""")
}

case object CountryId extends Field {
  override def validate(value: String): Boolean = true
}

object Field {

  def validateYear(value: String, from: Int, to: Int): Boolean =
    value.matches("""\d{4}""") && value.toInt.isInRange(from, to)

  implicit class RichInt(n: Int) {
    def isInRange(from: Int, to: Int): Boolean = from <= n && n <= to
  }

  def fromString(s: String): Field = s match {
    case "byr" => BirthYear
    case "iyr" => IssueYear
    case "eyr" => ExpirationYear
    case "hgt" => Height
    case "hcl" => HairColour
    case "ecl" => EyeColour
    case "pid" => PassportId
    case "cid" => CountryId
    case _ => throw new AssertionError(s"Unknown field $s")
  }

  val RequiredFields: Set[Field] = Set(BirthYear, IssueYear, ExpirationYear, Height, HairColour, EyeColour, PassportId)

}

case class Passport(fields: Map[Field, String]) {

  def isValid: Boolean = Field.RequiredFields subsetOf fields.keySet

  def isValid2: Boolean = isValid && fields.forall { case (field, value) => field.validate(value) }

}

object Passport {

  def load(inputPath: String): Seq[Passport] = {
    val s = Util.loadString(inputPath)
    s.split("\n\n").map(parsePassport)
  }

  private def parsePassport(s: String): Passport =
    Passport(s.split("\n").mkString(" ").split(" ").map(parseFieldValue).toMap)

  private def parseFieldValue(s: String): (Field, String) = {
    val chunks = s.split(":")
    Field.fromString(chunks(0)) -> chunks(1)
  }

}


object Day4 extends App {

  def solvePartOne(inputPath: String): Unit = {
    val passports = Passport.load(inputPath)
    println(passports.count(_.isValid))
  }

  def solvePartTwo(inputPath: String): Unit = {
    val passports = Passport.load(inputPath)
    println(passports.count(_.isValid2))
  }

  println("Part One")
  solvePartOne("day4/example.txt")
  solvePartOne("day4/puzzle.txt")
  println()
  println("Part Two")
  solvePartTwo("day4/example.txt")
  solvePartTwo("day4/puzzle.txt")

}