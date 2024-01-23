package smile.pictures

import smile.colors.Color
import smile.modeling.{Bounds, Pos}

class Point(pos: Pos, val color: Color) extends VectorGraphic:
  override lazy val boundary: Bounds = Bounds(pos, pos)
  override def copy(newPosition: Pos): Point =
    new Point(newPosition, color)

  override def scaleBy(horizontalFactor: Double, verticalFactor: Double): PictureElement = this
  override def scaleBy(
      horizontalFactor: Double,
      verticalFactor: Double,
      relativityPoint: Pos
  ): PictureElement =
    copy(newPosition = position.scaleBy(horizontalFactor, verticalFactor, relativityPoint))

  override def rotateBy(angle: Double, centerOfRotation: Pos): Point =
    copy(newPosition = position.rotateBy(angle, centerOfRotation))

  override def rotateByAroundOrigin(angle: Double): Point =
    copy(newPosition = position.rotateByAroundOrigin(angle))
