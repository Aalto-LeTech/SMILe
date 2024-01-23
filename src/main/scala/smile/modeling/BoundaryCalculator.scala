package smile.modeling

import smile.infrastructure.BufferAdapter
import smile.pictures.{PictureElement, Text}

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
      val xs     = bounds.flatMap(b => Seq(b.upperLeftCorner.x, b.lowerRightCorner.x))
      val ys     = bounds.flatMap(b => Seq(b.upperLeftCorner.y, b.lowerRightCorner.y))
      Bounds(xs.min, ys.min, xs.max, ys.max)
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
    BufferAdapter(1, 1).withGraphics2D: g =>
      val glyphVector     = text.font.createGlyphVector(g.getFontRenderContext, text.content)
      val visualBounds    = glyphVector.getVisualBounds
      val textWidth       = visualBounds.getWidth / 2
      val textHeight      = visualBounds.getHeight / 2
      val upperLeft: Pos  = text.position.moveBy(-textWidth, -textHeight)
      val lowerRight: Pos = text.position.moveBy(textWidth, textHeight)
      Bounds(upperLeft, lowerRight)
