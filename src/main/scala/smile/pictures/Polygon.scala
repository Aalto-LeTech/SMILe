package smile.pictures

import smile.colors.Color
import smile.modeling.{BoundaryCalculator, Bounds, Pos, TransformationMatrix, Transformer}

class Polygon(
    pos: Pos,
    val points: Seq[Pos],
    val hasBorder: Boolean,
    val hasFilling: Boolean,
    val color: Color,
    val fillColor: Color,
    override val transformationMatrix: TransformationMatrix = TransformationMatrix.identity
) extends VectorGraphic:

  override def copy(newPosition: Pos): PictureElement =
    internalCopy(newPosition = newPosition)

  override def copy(newMatrix: TransformationMatrix): PictureElement =
    internalCopy(newMatrix = newMatrix)

  private def internalCopy(
      newPosition: Pos = position,
      newPoints: Seq[Pos] = points,
      newHasBorder: Boolean = hasBorder,
      newHasFilling: Boolean = hasFilling,
      newColor: Color = color,
      newFillColor: Color = fillColor,
      newMatrix: TransformationMatrix = transformationMatrix
  ): Polygon =
    new Polygon(
      newPosition,
      newPoints,
      newHasBorder,
      newHasFilling,
      newColor,
      newFillColor,
      newMatrix
    )

  override lazy val position: Pos = pos

  lazy val contentBoundary: Bounds = BoundaryCalculator.fromPositions(points)

  lazy val corners: Seq[Pos] =
    val ulX = contentBoundary.upperLeftCorner.x
    val ulY = contentBoundary.upperLeftCorner.y
    val lrX = contentBoundary.lowerRightCorner.x
    val lrY = contentBoundary.lowerRightCorner.y

    Seq(
      position + contentBoundary.upperLeftCorner,
      position + Pos(lrX, ulY),
      position + contentBoundary.lowerRightCorner,
      position + Pos(ulX, lrY)
    )

  lazy val upperLeftCorner: Pos  = corners.head
  lazy val upperRightCorner: Pos = corners.tail.head
  lazy val lowerRightCorner: Pos = corners.tail.tail.head
  lazy val lowerLeftCorner: Pos  = corners.tail.tail.tail.head

  override lazy val boundary: Bounds =
    Bounds(upperLeftCorner, lowerRightCorner)
