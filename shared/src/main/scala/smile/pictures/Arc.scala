package smile.pictures

import smile.Settings.*
import smile.modeling.{Angle, Bounds, Pos, Transformer}

/** Represents an arc, a part of a circle or ellipse, defined by its center position, width, height,
  * start angle, arc angle, and optional fill and stroke styles.
  *
  * @param pos
  *   The center point of the arc.
  * @param width
  *   The width of the arc in pixels.
  * @param height
  *   The height of the arc in pixels.
  * @param startAngle
  *   The start angle of the arc in degrees.
  * @param arcAngle
  *   The angular extent of the arc in degrees.
  * @param rotationAngle
  *   The rotation angle of the arc in degrees, affecting its orientation.
  * @param fillStyle
  *   Optional fill style for the arc.
  * @param strokeStyle
  *   Optional stroke style defining the arc's outline appearance.
  */
class Arc(
    pos: Pos = DefaultPosition,
    val width: Double,
    val height: Double,
    val startAngle: Double = Angle.Zero.inDegrees,
    val arcAngle: Double = Angle.FullAngleInDegrees,
    val rotationAngle: Double = DefaultRotationAngleInDegrees,
    override val fillStyle: Option[FillStyle],
    override val strokeStyle: Option[StrokeStyle]
) extends VectorGraphic:

  override lazy val position: Pos = pos

  private lazy val corners: Seq[Pos] =
    // TODO strokeRadius
    val halfWidth  = width / 2.0
    val halfHeight = height / 2.0

    Seq(
      Transformer.rotate(position + (-halfWidth, -halfHeight), rotationAngle),
      Transformer.rotate(position + (halfWidth, -halfHeight), rotationAngle),
      Transformer.rotate(position + (halfWidth, halfHeight), rotationAngle),
      Transformer.rotate(position + (-halfWidth, halfHeight), rotationAngle)
    )

  /** Checks if the arc represents a complete cycle (360 degrees) or more.
    */
  lazy val isFullCycle: Boolean = arcAngle.abs >= Angle.FullAngleInDegrees

  /** Determines if the arc represents a perfect circle, based on its width and height.
    */
  lazy val isCircle: Boolean = isFullCycle && (width == height)

  /** Transformed upper left corner of this [[Arc]]. */
  lazy val upperLeftCorner: Pos = corners.head

  /** Transformed upper right corner of this [[Arc]]. */
  lazy val upperRightCorner: Pos = corners.tail.head

  /** Transformed lower right corner of this [[Arc]]. */
  lazy val lowerRightCorner: Pos = corners.tail.tail.head

  /** Transformed lower left corner of this [[Arc]]. */
  lazy val lowerLeftCorner: Pos = corners.tail.tail.tail.head

  /** Transformed boundary of this [[Arc]]. */
  override lazy val boundary: Bounds = Bounds(upperLeftCorner, lowerRightCorner)

  override def copy(newPosition: Pos): Arc =
    new Arc(
      newPosition,
      width,
      height,
      startAngle,
      arcAngle,
      rotationAngle,
      fillStyle,
      strokeStyle
    )

  private def internalCopy(
      newPosition: Pos = position,
      newWidth: Double = width,
      newHeight: Double = height,
      newStartAngle: Double = startAngle,
      newArcAngle: Double = arcAngle,
      newRotationAngle: Double = rotationAngle,
      newFillStyle: Option[FillStyle] = fillStyle,
      newStrokeStyle: Option[StrokeStyle] = strokeStyle
  ): Arc =
    val limitedArcAngle =
      newArcAngle
        .min(Angle.FullAngleInDegrees)
        .max(-Angle.FullAngleInDegrees)

    new Arc(
      newPosition,
      newWidth,
      newHeight,
      newStartAngle,
      limitedArcAngle,
      newRotationAngle,
      newFillStyle,
      newStrokeStyle
    )

  override def scaleBy(horizontalFactor: Double, verticalFactor: Double): Arc =
    internalCopy(newWidth = horizontalFactor * width, newHeight = verticalFactor * height)

  override def scaleBy(
      horizontalFactor: Double,
      verticalFactor: Double,
      relativityPoint: Pos
  ): Arc =
    val scaledPosition =
      Transformer.scale(position, horizontalFactor, verticalFactor, relativityPoint)
    internalCopy(
      newPosition = scaledPosition,
      newWidth = horizontalFactor * width,
      newHeight = verticalFactor * height
    )

  private def decideNewRotationAngleFor(newRotationAngle: Double): Double =
    // If this arc represents a circle, rotating it should not have any effect on
    // its appearance, and thus the rotation angle can (and must) be zero all the
    // time. (The position can, of course, change if the rotation is not performed
    // around the arc's center point, but that is irrelevant here.)
    if isCircle then Angle.Zero.inDegrees
    else rotationAngle + newRotationAngle

  override def rotateBy(angle: Double, centerOfRotation: Pos): Arc =
    internalCopy(
      newPosition = Transformer.rotate(position, angle, centerOfRotation),
      newRotationAngle = decideNewRotationAngleFor(angle)
    )

  override def rotateByAroundOrigin(angle: Double): Arc =
    internalCopy(
      newPosition = Transformer.rotate(position, angle),
      newRotationAngle = decideNewRotationAngleFor(angle)
    )
