package smile.pictures

import smile.colors.Color
import smile.modeling.{Bounds, Pos}

/** Represents a graphical point with a specified position and color.
  *
  * @param pos
  *   The position of the point.
  * @param color
  *   The color of the point.
  */
class Point(pos: Pos, val color: Color) extends VectorGraphic:

  override lazy val boundary: Bounds = Bounds(pos, pos)

  override def copy(newPosition: Pos): Point =
    new Point(newPosition, color)

  override def scaleBy(horizontalFactor: Double, verticalFactor: Double): Point = this

  override def scaleBy(
      horizontalFactor: Double,
      verticalFactor: Double,
      relativityPoint: Pos
  ): Point =
    copy(newPosition = position.scaleBy(horizontalFactor, verticalFactor, relativityPoint))

  override def rotateBy(angle: Double, centerOfRotation: Pos): Point =
    copy(newPosition = position.rotateBy(angle, centerOfRotation))

  override def rotateByAroundOrigin(angle: Double): Point =
    copy(newPosition = position.rotateByAroundOrigin(angle))
