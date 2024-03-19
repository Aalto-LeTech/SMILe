package smile.modeling

import smile.pictures.{PictureElement, Text}

import java.awt.font.FontRenderContext
import java.awt.geom.AffineTransform

/** Provides utility methods for calculating boundaries around picture elements and positions.
  */
object BoundaryCalculator:

  /** Calculates the bounding box that encompasses all provided picture elements.
    *
    * @param elements
    *   A sequence of `PictureElement` instances.
    * @return
    *   A `Bounds` instance representing the minimum bounding box around all elements. If the
    *   sequence is empty, returns `NullBounds`.
    */
  def fromBoundaries(elements: Seq[PictureElement]): Bounds =
    if elements.isEmpty then NullBounds
    else
      val bounds = elements.map(_.boundary)
      fromPositions(bounds.flatMap(b => Seq(b.upperLeftCorner, b.lowerRightCorner)))
  end fromBoundaries

  /** Calculates the bounding box from a sequence of positions.
    *
    * @param positions
    *   A sequence of `Pos` instances representing positions.
    * @return
    *   A `Bounds` instance representing the minimum bounding box around all positions. If the
    *   sequence is empty, returns `NullBounds`.
    */
  def fromPositions(positions: Seq[Pos]): Bounds =
    if positions.isEmpty then NullBounds
    else
      val xs = positions.map(_.x)
      val ys = positions.map(_.y)
      Bounds(xs.min, ys.min, xs.max, ys.max)
  end fromPositions

  /** Calculates the bounding box for a given text object.
    *
    * @param text
    *   The `Text` object for which to calculate the bounding box.
    * @return
    *   The `Bounds` representing the bounding box of the text.
    */
  def fromText(text: Text): Bounds =
    val frc = new FontRenderContext(new AffineTransform(), false, false)

    val lineMetrics = text.font.getLineMetrics(text.content, frc)
    val height      = lineMetrics.getHeight
    val width       = text.font.getStringBounds(text.content, frc).getWidth

    val upperLeft: Pos  = text.position.moveBy(-width / 2.0, -height / 2.0)
    val lowerRight: Pos = text.position.moveBy(width / 2.0, height / 2.0)

    Bounds(upperLeft, lowerRight)
