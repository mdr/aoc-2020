package aoc.day23


import aoc.day23.CupState.Cup
import aoc.util.Util

import scala.annotation.tailrec

class CupCell(val label: Long, var nextCell: Option[CupCell] = None) {

  def next: CupCell = nextCell.get

}

object CupCircle {

  def build(labels: Seq[Long]): CupCircle = {
    val cells = labels.map(label => label -> new CupCell(label)).toMap
    var previousCell = cells(labels.last)
    for (label <- labels) {
      val currentCell = cells(label)
      previousCell.nextCell = Some(currentCell)
      previousCell = currentCell
    }
    new CupCircle(cells, labels.head)
  }

}

class CupCircle(cells: Map[Long, CupCell], var currentLabel: Long) {

  override def toString: String = toStringFrom(currentLabel)

  def toStringFrom(initialLabel: Long): String =
    Seq.unfold(initialLabel)(label => cells(label).nextCell.map(_.label).filter(_ != initialLabel).map(label => label -> label))
      .prepended(initialLabel)
      .mkString

  def move(): Unit = {
    val currentCell = cells(currentLabel)
    val cell1 = currentCell.next
    val cell2 = cell1.next
    val cell3 = cell2.next
    currentCell.nextCell = cell3.nextCell
    cell3.nextCell = None
    val removedLabels = Set(cell1.label, cell2.label, cell3.label)

    @tailrec
    def findDestinationLabel(label: Long): Long = {
      val lesserLabel = if (label == 1) cells.size else label - 1
      if (removedLabels contains lesserLabel) findDestinationLabel(lesserLabel) else lesserLabel
    }

    val destinationLabel = findDestinationLabel(currentLabel)

    val destinationCell = cells(destinationLabel)
    val afterDestinationCell = destinationCell.next
    destinationCell.nextCell = Some(cell1)
    cell3.nextCell = Some(afterDestinationCell)

    currentLabel = currentCell.next.label
  }

  def productThingy: BigInt = {
    val oneCell = cells(1)
    val cell1 = oneCell.next
    val cell2 = cell1.next
    BigInt(cell1.label) * BigInt(cell2.label)
  }

}


object CupState {
  type Cup = Long
}

case class CupState(cups: Seq[Cup], currentCup: Cup) {

  def move: CupState = {
    val currentIndex = cups.indexOf(currentCup)
    val (ixs1, ixs2) = ((currentIndex + 1) until (currentIndex + 4)).span(_ < cups.size)
    val removedCups = (ixs1 ++ ixs2.map(_ % cups.size)).map(cups)
    val remainingCups = cups.diff(removedCups)
    val destinationCup = findDestinationCup(currentCup, remainingCups)
    val destinationIndex = remainingCups.indexOf(destinationCup)
    val newCups = remainingCups.patch((destinationIndex + 1) % cups.size, removedCups, 0)
    val currentIndexAgain = newCups.indexOf(currentCup)
    val newCurrentCup = newCups((currentIndexAgain + 1) % cups.size)
    CupState(newCups, newCurrentCup)
  }

  @tailrec
  private def findDestinationCup(cup: Cup, remainingCups: Seq[Cup]): Cup = {
    val lesserCup = if (cup == 1) cups.size else cup - 1
    if (remainingCups contains lesserCup) lesserCup else findDestinationCup(lesserCup, remainingCups)
  }

  def order: String = {
    val ix = cups.indexOf(1)
    val (ixs1, ixs2) = ((ix + 1) until (ix + cups.size)).span(_ < cups.size)
    (ixs1 ++ ixs2.map(_ % cups.size)).map(cups).map(_.toString).mkString
  }

  def productThingy: Cup = {
    val ix = cups.indexOf(1)
    cups((ix + 1) % cups.size) * cups((ix + 2) % cups.size)
  }

}

object Day23 extends App {

  def solvePartOne(inputPath: String): Unit = {
    val cups = loadCups(inputPath)
    val initialState = CupState(cups, cups.head)
    val finalState = Util.iterate(initialState, times = 100)(_.move)
    println(finalState.order)
  }

  def solvePartTwo(inputPath: String): Unit = {
    val labels = loadCups(inputPath)
    val allLabels = labels ++ ((labels.max + 1) to 1_000_000)
    val circle = CupCircle.build(allLabels)
    for (n <- 1 to 10_000_000) {
      circle.move()
    }
    println(circle.productThingy)
  }

  private def loadCups(inputPath: String): Seq[Cup] = Util.loadString(inputPath).map(_.toString.toInt)

  println("Part One")
  solvePartOne("day23/example.txt")
  solvePartOne("day23/puzzle.txt")
  println()
  println("Part Two")
//  solvePartTwo("day23/example.txt")
    solvePartTwo("day23/puzzle.txt")

}