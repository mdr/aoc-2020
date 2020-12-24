package aoc.day22

import aoc.util.Util

import scala.annotation.tailrec

case class GameResult(player1Wins: Boolean, score: Int)

object Day22 extends App {

  type Card = Int

  def score(cards: Seq[Card]): Int = cards.reverse.lazyZip(1 to cards.size).map(_ * _).sum

  @tailrec
  def playCombat(cards1: Seq[Card], cards2: Seq[Card]): Int =
    (cards1, cards2) match {
      case (Seq(card1, otherCards1@_*), Seq(card2, otherCards2@_*)) =>
        if (card1 > card2)
          playCombat(otherCards1 ++ Seq(card1, card2), otherCards2)
        else
          playCombat(otherCards1, otherCards2 ++ Seq(card2, card1))
      case (Seq(), _) => score(cards2)
      case (_, Seq()) => score(cards1)
    }

  def playRecursiveCombat(cards1: Seq[Card], cards2: Seq[Card]): GameResult =
    playRecursiveCombat(cards1, cards2, previousRoundStates = Set.empty)

  @tailrec
  def playRecursiveCombat(cards1: Seq[Card], cards2: Seq[Card], previousRoundStates: Set[(Seq[Card], Seq[Card])]): GameResult =
    if (previousRoundStates contains (cards1 -> cards2))
      GameResult(player1Wins = true, score(cards1))
    else
      (cards1, cards2) match {
        case (Seq(card1, otherCards1@_*), Seq(card2, otherCards2@_*)) =>
          val shouldPlaySubgame = otherCards1.size >= card1 && otherCards2.size >= card2
          val player1Wins =
            if (shouldPlaySubgame)
              playRecursiveCombat(otherCards1 take card1, otherCards2 take card2).player1Wins
            else
              card1 > card2
          if (player1Wins)
            playRecursiveCombat(otherCards1 ++ Seq(card1, card2), otherCards2, previousRoundStates + (cards1 -> cards2))
          else
            playRecursiveCombat(otherCards1, otherCards2 ++ Seq(card2, card1), previousRoundStates + (cards1 -> cards2))
        case (_, Seq()) => GameResult(player1Wins = true, score(cards1))
        case (Seq(), _) => GameResult(player1Wins = false, score(cards2))
      }

  private def loadDecks(inputPath: String): (Seq[Int], Seq[Int]) = {
    val Array(section1, section2) = Util.loadString(inputPath).split("\n\n")
    val deck1 = section1.split("\n").toSeq.tail.map(_.toInt)
    val deck2 = section2.split("\n").toSeq.tail.map(_.toInt)
    (deck1, deck2)
  }

  def solvePartOne(inputPath: String): Unit = {
    val (deck1, deck2) = loadDecks(inputPath)
    println(playCombat(deck1, deck2))
  }

  def solvePartTwo(inputPath: String): Unit = {
    val (deck1, deck2) = loadDecks(inputPath)
    println(playRecursiveCombat(deck1, deck2).score)
  }

  println("Part One")
  solvePartOne("day22/example.txt")
  solvePartOne("day22/puzzle.txt")
  println()
  println("Part Two")
  solvePartTwo("day22/example.txt")
  solvePartTwo("day22/puzzle.txt")

}