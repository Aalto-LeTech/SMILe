package smile.modeling

import smile.pictures.{PictureElement, Text}

import java.awt.FontMetrics
import java.awt.image.BufferedImage
import scala.annotation.tailrec

object BoundaryCalculator:

  def fromBoundaries(elements: Seq[PictureElement], transformed: Boolean = false): Bounds =
    if elements.isEmpty then NullBounds
    else
      val bounds =
        if transformed then elements.map(calculateTransformedBounds) else elements.map(_.boundary)
      val xs = bounds.flatMap(b => Seq(b.upperLeftCorner.x, b.lowerRightCorner.x))
      val ys = bounds.flatMap(b => Seq(b.upperLeftCorner.y, b.lowerRightCorner.y))
      Bounds(xs.min, ys.min, xs.max, ys.max)
  end fromBoundaries

  def calculateTransformedBounds(element: PictureElement): Bounds =
    val originalBounds = element.boundary
    val corners = Seq(
      originalBounds.upperLeftCorner,
      Pos(originalBounds.lowerRightCorner.x, originalBounds.upperLeftCorner.y),
      originalBounds.lowerRightCorner,
      Pos(originalBounds.upperLeftCorner.x, originalBounds.lowerRightCorner.y)
    )
    val transformedCorners = corners.map(element.transformationMatrix.applyToPoint)
    fromPositions(transformedCorners)
  end calculateTransformedBounds

  def fromPositions(positions: Seq[Pos]): Bounds =
    if positions.isEmpty then NullBounds
    else
      val xs = positions.map(_.x)
      val ys = positions.map(_.y)
      Bounds(xs.min, ys.min, xs.max, ys.max)
  end fromPositions

  def fromText(text: Text): Bounds =
    val g                    = new BufferedImage(1, 1, BufferedImage.TYPE_INT_ARGB).createGraphics()
    val metrics: FontMetrics = g.getFontMetrics(text.font)
    val width: Int           = metrics.stringWidth(text.content)
    val height: Int          = metrics.getHeight
    val upperLeft: Pos       = text.position
    val lowerRight: Pos      = text.position.moveBy(width, height)
    Bounds(upperLeft, lowerRight)
