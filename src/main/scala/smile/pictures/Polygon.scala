package smile.pictures

import smile.colors.Color
import smile.modeling.{BoundaryCalculator, Bounds, Pos, Transformer}

class Polygon(
    pos: Pos,
    val pointsRelativeToCenterAtOrigo: Seq[Pos],
    val referencePointRelativeToCenterAtOrigo: Pos,
    val hasBorder: Boolean,
    val hasFilling: Boolean,
    val color: Color,
    val fillColor: Color
) extends VectorGraphic:

  override def copy(newPosition: Pos): PictureElement = new Polygon(
    newPosition,
    pointsRelativeToCenterAtOrigo,
    referencePointRelativeToCenterAtOrigo,
    hasBorder,
    hasFilling,
    color,
    fillColor
  )

  private def internalCopy(
      newPosition: Pos = position,
      newPointsRelativeToCenterAtOrigo: Seq[Pos] = pointsRelativeToCenterAtOrigo,
      newReferencePointRelativeToCenterAtOrigo: Pos = referencePointRelativeToCenterAtOrigo,
      newHasBorder: Boolean = hasBorder,
      newHasFilling: Boolean = hasFilling,
      newColor: Color = color,
      newFillColor: Color = fillColor
  ): Polygon =
    new Polygon(
      newPosition,
      newPointsRelativeToCenterAtOrigo,
      newReferencePointRelativeToCenterAtOrigo,
      newHasBorder,
      newHasFilling,
      newColor,
      newFillColor
    )

  override lazy val position: Pos = pos

  val contentBoundary: Bounds =
    BoundaryCalculator.fromPositions(pointsRelativeToCenterAtOrigo)

  lazy val corners: Seq[Pos] =
    val ulX = contentBoundary.upperLeftCorner.x
    val ulY = contentBoundary.upperLeftCorner.y
    val lrX = contentBoundary.lowerRightCorner.x
    val lrY = contentBoundary.lowerRightCorner.y

    val refX = referencePointRelativeToCenterAtOrigo.x
    val refY = referencePointRelativeToCenterAtOrigo.y

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

  override def scaleBy(horizontalFactor: Double, verticalFactor: Double): Polygon =
    internalCopy(
      newPointsRelativeToCenterAtOrigo = pointsRelativeToCenterAtOrigo.map(
        _.scaleByRelativeToOrigo(horizontalFactor, verticalFactor)
      ),
      newReferencePointRelativeToCenterAtOrigo = referencePointRelativeToCenterAtOrigo
        .scaleByRelativeToOrigo(horizontalFactor, verticalFactor)
    )

  override def scaleBy(
      horizontalFactor: Double,
      verticalFactor: Double,
      relativityPoint: Pos
  ): Polygon =
    internalCopy(
      newPosition = Transformer.scale(position, horizontalFactor, verticalFactor, relativityPoint),
      newPointsRelativeToCenterAtOrigo = pointsRelativeToCenterAtOrigo.map(
        _.scaleByRelativeToOrigo(horizontalFactor, verticalFactor)
      ),
      newReferencePointRelativeToCenterAtOrigo = referencePointRelativeToCenterAtOrigo
        .scaleByRelativeToOrigo(horizontalFactor, verticalFactor)
    )

  override def rotateBy(angle: Double, centerOfRotation: Pos): Polygon =
    internalCopy(
      newPosition = Transformer.rotate(position, angle, centerOfRotation),
      newPointsRelativeToCenterAtOrigo =
        pointsRelativeToCenterAtOrigo.map(_.rotateByAroundOrigo(angle)),
      newReferencePointRelativeToCenterAtOrigo =
        referencePointRelativeToCenterAtOrigo.rotateByAroundOrigo(angle)
    )

  override def rotateByAroundOrigo(angle: Double): Polygon =
    internalCopy(
      newPosition = Transformer.rotate(position, angle),
      newPointsRelativeToCenterAtOrigo =
        pointsRelativeToCenterAtOrigo.map(_.rotateByAroundOrigo(angle)),
      newReferencePointRelativeToCenterAtOrigo =
        referencePointRelativeToCenterAtOrigo.rotateByAroundOrigo(angle)
    )
