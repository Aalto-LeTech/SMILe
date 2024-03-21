package smile.modeling

import smile.infrastructure.PlatformSpecific
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
  def fromText(text: Text): Bounds = PlatformSpecific.textBoundaryCalculator(text)
