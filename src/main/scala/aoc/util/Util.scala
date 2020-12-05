package aoc.util

import scala.io.Source

object Util {

  def loadString(path: String): String = Source.fromResource(path).mkString

  def loadLines(path: String): Seq[String] = Source.fromResource(path).getLines().toSeq

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

}
