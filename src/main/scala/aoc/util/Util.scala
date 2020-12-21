package aoc.util

import scala.annotation.tailrec
import scala.io.Source

object Util {

  def loadString(path: String): String = Source.fromResource(path).mkString

  def loadLines(path: String): Seq[String] = Source.fromResource(path).getLines().toSeq

  implicit class RichSeq[T](xs: Seq[T]) {
    def traverse[U](f: T => Seq[U]): Seq[Seq[U]] = xs.map(f).sequence
  }

  implicit class RichSeqOfSeq[T](xs: Seq[Seq[T]]) {
    def sequence: Seq[Seq[T]] = xs match {
      case Seq() => Seq(Seq())
      case Seq(first, rest@_*) =>
        for {
          x <- first
          xs <- rest.sequence
        } yield x +: xs
    }

  }

  import scala.collection.IterableOnce

  implicit class SumByOperation[A](coll: IterableOnce[A]) {
    def sumBy[B](f: A => B)(implicit num: Numeric[B]): B = {
      val it = coll.iterator
      if (!it.hasNext)
        return num.zero
      var result = f(it.next())
      while (it.hasNext) {
        result = num.plus(result, f(it.next()))
      }
      result
    }
  }

  @tailrec
  def iterate[T](value: T, times: Int)(f: T => T): T = if (times == 0) value else iterate(f(value), times - 1)(f)

  @tailrec
  def iterateUntilSteadyState[T](initial: T)(iterate: T => T): T = {
    val next = iterate(initial)
    if (next == initial) initial else iterateUntilSteadyState(next)(iterate)
  }

}
