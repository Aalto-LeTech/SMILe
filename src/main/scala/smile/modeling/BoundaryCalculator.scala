package smile.modeling

import smile.pictures.{PictureElement, Text}

import java.awt.FontMetrics
import java.awt.image.BufferedImage
import scala.annotation.tailrec

object BoundaryCalculator:
  def fromBoundaries(elements: Seq[PictureElement]): Bounds =
    if elements == null then return NullBounds

    val numberOfElements = elements.length

    if numberOfElements == 0 then NullBounds
    else if numberOfElements == 1 then elements.head.boundary
    else
      fromBoundariesRecursive(
        elements.iterator,
        false,
        Double.MaxValue,
        Double.MaxValue,
        Double.MinValue,
        Double.MinValue
      )
  end fromBoundaries

  @tailrec
  private def fromBoundariesRecursive(
      it: Iterator[PictureElement],
      foundOneBoundary: Boolean,
      x0: Double,
      y0: Double,
      x1: Double,
      y1: Double
  ): Bounds =

    if !it.hasNext then
      if foundOneBoundary then return Bounds(x0, y0, x1, y1) else return NullBounds

    val element = it.next()

    val boundary = element.boundary
    val ul       = boundary.upperLeftCorner
    val lr       = boundary.lowerRightCorner

    val x0New = if ul.x < x0 then ul.x else x0
    val y0New = if ul.y < y0 then ul.y else y0
    val x1New = if lr.x > x1 then lr.x else x1
    val y1New = if lr.y > y1 then lr.y else y1

    fromBoundariesRecursive(it, foundOneBoundary = true, x0New, y0New, x1New, y1New)
  end fromBoundariesRecursive

  final inline def fromPositions(position: Pos): Bounds =
    Bounds(position, position + (1.0, 1.0))

  /** @param a
    * @param b
    * @return
    */
  def fromPositions(a: Pos, b: Pos): Bounds =
    val (xMin, xMax) =
      if a.x <= b.x then (a.x, b.x)
      else (b.x, a.x)

    val (yMin, yMax) =
      if a.y <= b.y then (a.y, b.y)
      else (b.y, a.y)

    Bounds(xMin, yMin, xMax + 1, yMax + 1)

  def fromPositions(a: Pos, b: Pos, c: Pos): Bounds =
    val x1 = a.x
    val x2 = b.x
    val x3 = c.x

    var xMax = x1
    var xMin = x3

    if x1 >= x2 then    // x1 >= x2
      if x1 >= x3 then  // x1 >= x2 && x1 >= x3 ==> default max == x1 is OK
        if x2 < x3 then // x1 >= x3 > x2
          xMin = x2     // ==> (x1, x2)
        else {
          /* DEFAULTS OK */
        }         // x1 >= x2 >= x3 ==> (x1, x3) ==> default min == x3 is OK
      else        // x3 > x1 >= x2
        xMin = x2 // ==> (x3, x2)
        xMax = x3
    else             // x2 > x1
    if x1 >= x3 then // x2 > x1 >= x3
      xMax = x2      // ==> (x2, x3) ==> default min == x3 is OK
    else             // x2 > x1 && x3 > x1 ==> min == x1
      xMin = x1

      if x2 >= x3 then // x2 >= x3 > x1
        xMax = x2      // ==> (x2, x1)
      else             // x3 > x2 > x1
        xMax = x3      // ==> (x3, x1)

    val y1 = a.y
    val y2 = b.y
    val y3 = c.y

    var yMax = y1
    var yMin = y3

    if y1 >= y2 then    // y1 >= y2
      if y1 >= y3 then  // y1 >= y2 && y1 >= y3 ==> default max == y1 is OK
        if y2 < y3 then // y1 >= y3 > y2
          yMin = y2     // ==> (y1, y2)
        else {
          /* DEFAULTS OK */
        }         // y1 >= y2 >= y3 ==> (y1, y3) ==> default min == y3 is OK
      else        // y3 > y1 >= y2
        yMin = y2 // ==> (y3, y2)
        yMax = y3
    else             // y2 > y1
    if y1 >= y3 then // y2 > y1 >= y3
      yMax = y2      // ==> (y2, y3) ==> default min == y3 is OK
    else             // y2 > y1 && y3 > y1 ==> min == y1
      yMin = y1

      if y2 >= y3 then // y2 >= y3 > y1
        yMax = y2      // ==> (y2, y1)
      else             // y3 > y2 > y1
        yMax = y3      // ==> (y3, y1)

    Bounds(xMin, yMin, xMax + 1, yMax + 1)

  def fromPositions(positions: Seq[Pos]): Bounds =
    if positions == null then return NullBounds

    val numberOfPositions = positions.length

    if numberOfPositions >= 4 then
      fromPositionsRecursive(
        positions.iterator,
        false,
        Double.MaxValue,
        Double.MaxValue,
        Double.MinValue,
        Double.MinValue
      )
    else if numberOfPositions == 3 then fromPositions(positions.head, positions(1), positions(2))
    else if numberOfPositions == 2 then fromPositions(positions.head, positions(1))
    else if numberOfPositions == 1 then fromPositions(positions.head)
    else NullBounds
  end fromPositions

  @tailrec
  private def fromPositionsRecursive(
      it: Iterator[Pos],
      foundOneBoundary: Boolean,
      x0: Double,
      y0: Double,
      x1: Double,
      y1: Double
  ): Bounds =

    if !it.hasNext then
      return if foundOneBoundary then Bounds(x0, y0, x1 + 1, y1 + 1) else NullBounds

    val position = it.next()

    val x0New = if position.x < x0 then position.x else x0
    val y0New = if position.y < y0 then position.y else y0
    val x1New = if position.x > x1 then position.x else x1
    val y1New = if position.y > y1 then position.y else y1

    fromPositionsRecursive(it, foundOneBoundary = true, x0New, y0New, x1New, y1New)
  end fromPositionsRecursive

  def fromText(text: Text) =
    val g                    = new BufferedImage(1, 1, BufferedImage.TYPE_INT_ARGB).createGraphics()
    val metrics: FontMetrics = g.getFontMetrics(text.font)
    val width: Int           = metrics.stringWidth(text.content)
    val height: Int          = metrics.getHeight
    val upperLeft: Pos       = text.position
    val lowerRight: Pos      = text.position.moveBy(width, height)
    Bounds(upperLeft, lowerRight)
