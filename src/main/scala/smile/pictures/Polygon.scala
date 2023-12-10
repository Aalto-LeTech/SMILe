package smile.pictures

import smile.colors.Color
import smile.modeling.{BoundaryCalculator, Bounds, Pos, Transformer}

class Polygon(
    pos: Pos,
    val points: Seq[Pos],
    val hasBorder: Boolean,
    val hasFilling: Boolean,
    val color: Color,
    val fillColor: Color
) extends VectorGraphic:

  override def copy(newPosition: Pos): PictureElement = new Polygon(
    newPosition,
    points,
    hasBorder,
    hasFilling,
    color,
    fillColor
  )

  private def internalCopy(
      newPosition: Pos = position,
      newPoints: Seq[Pos] = points,
      newHasBorder: Boolean = hasBorder,
      newHasFilling: Boolean = hasFilling,
      newColor: Color = color,
      newFillColor: Color = fillColor
  ): Polygon =
    new Polygon(
      newPosition,
      newPoints,
      newHasBorder,
      newHasFilling,
      newColor,
      newFillColor
    )

  override lazy val position: Pos = pos

  val contentBoundary: Bounds =
    BoundaryCalculator.fromPositions(points)

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
  lazy val lowerRightCorner: Pos = corners.tail.tail.head

  override lazy val boundary: Bounds =
    Bounds(upperLeftCorner, lowerRightCorner)

  override def scaleBy(horizontalFactor: Double, verticalFactor: Double): Polygon =
    internalCopy(
      newPoints = points.map(
        _.scaleByRelativeToOrigo(horizontalFactor, verticalFactor)
      )
    )

  override def scaleBy(
      horizontalFactor: Double,
      verticalFactor: Double,
      relativityPoint: Pos
  ): Polygon =
    internalCopy(
      newPosition = Transformer.scale(position, horizontalFactor, verticalFactor, relativityPoint),
      newPoints = points.map(
        _.scaleByRelativeToOrigo(horizontalFactor, verticalFactor)
      )
    )

  override def rotateBy(angle: Double, centerOfRotation: Pos): Polygon =
    internalCopy(
      newPosition = Transformer.rotate(position, angle, centerOfRotation),
      newPoints = points.map(_.rotateBy(angle, centerOfRotation))
    )

  override def rotateByAroundOrigo(angle: Double): Polygon =
    internalCopy(
      newPosition = Transformer.rotate(position, angle),
      newPoints = points.map(_.rotateByAroundOrigo(angle))
    )
