package smile.pictures

import smile.modeling.*

/** Represents a polygon defined by a sequence of points, with optional fill and stroke styles.
  *
  * @param pos
  *   The position of the polygon. This usually represents the centroid or a specific anchor point.
  * @param points
  *   The sequence of points defining the vertices of the polygon.
  * @param fillStyle
  *   Optional fill style for the interior of the polygon.
  * @param strokeStyle
  *   Optional stroke style for the outline of the polygon.
  */
class Polygon(
    pos: Pos,
    val points: Seq[Pos],
    override val fillStyle: Option[FillStyle],
    override val strokeStyle: Option[StrokeStyle]
) extends VectorGraphic:

  override def copy(newPosition: Pos): Polygon = new Polygon(
    newPosition,
    points,
    fillStyle,
    strokeStyle
  )

  private def internalCopy(
      newPosition: Pos = position,
      newPoints: Seq[Pos] = points,
      newFillStyle: Option[FillStyle] = fillStyle,
      newStrokeStyle: Option[StrokeStyle] = strokeStyle
  ): Polygon =
    new Polygon(
      newPosition,
      newPoints,
      newFillStyle,
      newStrokeStyle
    )

  override lazy val position: Pos = pos

  /** Computes the boundary of the polygon based on its points.
    */
  private val contentBoundary: Bounds =
    BoundaryCalculator.fromPositions(points)

  /** Determines the corners of the polygon's bounding box.
    */
  lazy val corners: Seq[Pos] =
    val strokeRadius = strokeStyle.map(_.width).getOrElse(0.0) / 2

    val ulX = contentBoundary.upperLeftCorner.x - strokeRadius
    val ulY = contentBoundary.upperLeftCorner.y - strokeRadius
    val lrX = contentBoundary.lowerRightCorner.x + strokeRadius
    val lrY = contentBoundary.lowerRightCorner.y + strokeRadius

    Seq(
      position + contentBoundary.upperLeftCorner - Pos(strokeRadius, strokeRadius),
      position + Pos(lrX, ulY),
      position + contentBoundary.lowerRightCorner + Pos(strokeRadius, strokeRadius),
      position + Pos(ulX, lrY)
    )

  lazy val upperLeftCorner: Pos  = corners.head
  lazy val lowerRightCorner: Pos = corners.tail.tail.head

  override lazy val boundary: Bounds =
    Bounds(upperLeftCorner, lowerRightCorner)

  override def scaleBy(horizontalFactor: Double, verticalFactor: Double): Polygon =
    internalCopy(
      newPoints = points.map(
        _.scaleByRelativeToOrigin(horizontalFactor, verticalFactor)
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
        _.scaleByRelativeToOrigin(horizontalFactor, verticalFactor)
      )
    )

  override def rotateBy(angle: Double, centerOfRotation: Pos): Polygon =
    internalCopy(
      newPosition = Transformer.rotate(position, angle, centerOfRotation),
      newPoints = points.map(_.rotateBy(angle, centerOfRotation))
    )

  override def rotateByAroundOrigin(angle: Double): Polygon =
    internalCopy(
      newPosition = Transformer.rotate(position, angle),
      newPoints = points.map(_.rotateByAroundOrigin(angle))
    )
