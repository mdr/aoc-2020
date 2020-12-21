package aoc.day20

import aoc.day20.Tile.TileId
import aoc.util.Util

case class Fingerprint(north: Seq[Boolean], east: Seq[Boolean], south: Seq[Boolean], west: Seq[Boolean]) {

  def canFitHorizontallyWith(that: Fingerprint): Boolean = this.east == that.west

  def canFitVerticallyWith(that: Fingerprint): Boolean = this.south == that.north

  def orient(orientation: Orientation): Fingerprint = orientation match {
    case Orientation.R0 => this
    case Orientation.R90 => rotate90
    case Orientation.R180 => rotate180
    case Orientation.R270 => rotate270
    case Orientation.H => flipAboutHorizontal
    case Orientation.V => flipAboutVertical
    case Orientation.D => flipAboutMainDiagonal
    case Orientation.D2 => flipAboutOtherDiagonal
  }

  lazy val rotate90: Fingerprint = Fingerprint(west.reverse, north, east.reverse, south)

  lazy val rotate180: Fingerprint = rotate90.rotate90

  lazy val rotate270: Fingerprint = rotate90.rotate90.rotate90

  lazy val flipAboutVertical: Fingerprint = copy(north = north.reverse, east = west, south = south.reverse, west = east)

  lazy val flipAboutHorizontal: Fingerprint = copy(north = south, east = east.reverse, south = north, west = west.reverse)

  lazy val flipAboutMainDiagonal: Fingerprint = rotate90.flipAboutVertical

  lazy val flipAboutOtherDiagonal: Fingerprint = rotate90.flipAboutHorizontal

  override def toString = s"[${Fingerprint.borderValue(north)} ${Fingerprint.borderValue(east)} ${Fingerprint.borderValue(south)} ${Fingerprint.borderValue(west)}]"

}

sealed trait Orientation

object Orientation {

  case object R0 extends Orientation

  case object R90 extends Orientation

  case object R180 extends Orientation

  case object R270 extends Orientation

  case object H extends Orientation

  case object V extends Orientation

  case object D extends Orientation

  case object D2 extends Orientation

  val All: Seq[Orientation] = Seq(R0, R90, R180, R270, H, V, D, D2)
}

object Fingerprint {

  def borderValue(pixels: Seq[Boolean]): Int = {
    val bitString = pixels.map {
      case false => 0
      case true => 1
    }.mkString
    Integer.parseInt(bitString, 2)
  }

}


case class Tile(pixels: Seq[Seq[Boolean]]) {

  val fingerprint: Fingerprint = Fingerprint(
    pixels.head,
    pixels.map(_.last),
    pixels.last,
    pixels.map(_.head)
  )

  lazy val rotate90: Tile = flipAboutMainDiagonal.flipAboutVertical

  lazy val rotate180: Tile = rotate90.rotate90

  lazy val rotate270: Tile = rotate90.rotate90.rotate90

  lazy val flipAboutVertical: Tile = Tile(pixels.map(_.reverse))

  lazy val flipAboutHorizontal: Tile = Tile(pixels.reverse)

  lazy val flipAboutMainDiagonal: Tile = Tile(pixels.transpose)

  lazy val flipAboutOtherDiagonal: Tile = flipAboutVertical.rotate90

  lazy val withoutBorder: Tile = Tile(pixels.tail.init.map(_.tail.init))

  def orient(orientation: Orientation): Tile = orientation match {
    case Orientation.R0 => this
    case Orientation.R90 => rotate90
    case Orientation.R180 => rotate180
    case Orientation.R270 => rotate270
    case Orientation.H => flipAboutHorizontal
    case Orientation.V => flipAboutVertical
    case Orientation.D => flipAboutMainDiagonal
    case Orientation.D2 => flipAboutOtherDiagonal
  }

  def pasteHorizontally(that: Tile): Tile = Tile((this.pixels lazyZip that.pixels).map(_ ++ _))

  def pasteVertically(that: Tile): Tile = Tile(this.pixels ++ that.pixels)

  override def toString: String = pixels.map(_.map {
    case true => '#'
    case false => '.'
  }.mkString).mkString("\n")

  def points: Seq[Point] =
    for {
      row <- 0 until height
      column <- 0 until width
    } yield Point(row, column)

  def activePixels: Set[Point] = points.filter(get).toSet

  def get(point: Point): Boolean = pixels.lift(point.row).getOrElse(Seq.empty).lift(point.column).getOrElse(false)

  def contains(that: Tile, offset: Point): Boolean =
    that.points.filter(that.get).forall(point => this.get(point + offset))

  def height: Int = pixels.length

  def width: Int = pixels.map(_.length).max

}

object Tile {

  type TileId = BigInt

  private def parseRow(s: String): Seq[Boolean] = s.map(_ == '#')

  private val IdRegex = """Tile (\d+):""".r

  private def parseId(s: String): TileId = {
    val IdRegex(id) = s
    BigInt(id)
  }

  def parse(s: String): (TileId, Tile) = {
    val lines = s.split("\n")
    val id = parseId(lines.head)
    val pixels = lines.toSeq.tail.map(parseRow)
    val tile = Tile(pixels)
    id -> tile
  }

  def parseTile(s: String): Tile = {
    val lines = s.split("\n")
    val pixels = lines.toSeq.map(parseRow)
    Tile(pixels)
  }

}

case class Point(row: Int, column: Int) {
  def left: Point = copy(column = column - 1)

  def up: Point = copy(row = row - 1)

  def +(that: Point): Point = Point(row + that.row, column + that.column)

}

case class Solver(fingerprints: Map[TileId, Fingerprint], width: Int, height: Int) {

  case class SolutionState(allocatedTiles: Map[Point, (TileId, Orientation)] = Map.empty,
                           remainingTileIds: Set[TileId] = fingerprints.keySet,
                           nextPoint: Option[Point] = Some(Point(0, 0))) {

    def tileIdGrid: Seq[Seq[(TileId, Orientation)]] =
      (0 until height).map(row =>
        (0 until width).map(column => allocatedTiles(Point(row, column))))

    def cornerProduct: BigInt =
      allocatedTiles(Point(0, 0))._1 *
        allocatedTiles(Point(0, width - 1))._1 *
        allocatedTiles(Point(height - 1, width - 1))._1 *
        allocatedTiles(Point(height - 1, 0))._1

    private def getNextPoint(point: Point): Option[Point] = {
      val Point(row, column) = point
      if (column < width - 1)
        Some(point.copy(column = column + 1))
      else if (row < height - 1)
        Some(point.copy(row = row + 1, column = 0))
      else
        None
    }

    def allocate(point: Point, tileId: TileId, orientation: Orientation): SolutionState = copy(
      allocatedTiles = allocatedTiles + (point -> (tileId, orientation)),
      remainingTileIds = remainingTileIds - tileId,
      nextPoint = getNextPoint(point))

    def isValidTilePlacement(point: Point, tileId: TileId, orientation: Orientation): Boolean = {
      val Point(row, column) = point
      val fingerprint = fingerprints(tileId).orient(orientation)
      if (column > 0) {
        val (leftTileId, leftOrientation) = allocatedTiles(point.left)
        val leftFingerprint = fingerprints(leftTileId).orient(leftOrientation)
        if (!leftFingerprint.canFitHorizontallyWith(fingerprint))
          return false
      }
      if (row > 0) {
        val (aboveTileId, aboveOrientation) = allocatedTiles(point.up)
        val aboveFingerprint = fingerprints(aboveTileId).orient(aboveOrientation)
        if (!aboveFingerprint.canFitVerticallyWith(fingerprint))
          return false
      }
      true
    }

  }

  private def doSolve(state: SolutionState): Seq[SolutionState] =
    state.nextPoint match {
      case None => Seq(state)
      case Some(point) =>
        for {
          tileId <- state.remainingTileIds.toSeq
          orientation <- Orientation.All
          if state.isValidTilePlacement(point, tileId, orientation)
          newState = state.allocate(point, tileId, orientation)
          solvedState <- doSolve(newState)
        } yield solvedState
    }

  def solve: Seq[SolutionState] = doSolve(SolutionState())

}


object Day20 extends App {

  def solvePartOne(inputPath: String) = {
    val tiles = Util.loadString(inputPath).split("\n\n").map(Tile.parse).toMap
    val fingerprints = for ((tileId, tile) <- tiles) yield tileId -> tile.fingerprint
    val side = math.sqrt(tiles.size).toInt
    val solutions = Solver(fingerprints, width = side, height = side).solve
    val solution = solutions.head
    val combinedTile = solution.tileIdGrid.map(row => {
      row.map { case (tileId, orientation) => tiles(tileId).withoutBorder.orient(orientation) }.reduce(_ pasteHorizontally _)
    }).reduce(_ pasteVertically _)
    println(solution.cornerProduct)
    combinedTile
  }

  val monster = Tile.parseTile(
    """                  #
      |#    ##    ##    ###
      | #  #  #  #  #  #   """.stripMargin)

  def solvePartTwo(tile: Tile): Unit = {
    val orientation = Orientation.All.find { orientation =>
      val tileToCheck = tile.orient(orientation)
      tileToCheck.points.exists(tileToCheck.contains(monster, _))
    }.get
    val tileToCheck = tile.orient(orientation)
    val offsets= tileToCheck.points.filter(tileToCheck.contains(monster, _))
    val monsterPixels = offsets.flatMap(offset => monster.activePixels.map(_ + offset))
    val roughWaterPixels = tileToCheck.activePixels -- monsterPixels
    println(roughWaterPixels.size)
  }

  println("Part One")
  val solution1 = solvePartOne("day20/example.txt")
  val solution2=  solvePartOne("day20/puzzle.txt")
  println()
  println("Part Two")
  solvePartTwo(solution1)
    solvePartTwo(solution2)

}