package smile.pictures

import smile.Settings.*
import smile.colors.Color
import smile.modeling.{Angle, Bounds, Pos, TransformationMatrix, Transformer}

/** An [[Arc]] is a [[VectorGraphic]] that is defined by a position, a width, a height, a start
  * angle, an arc angle,
  * @param position
  *   center point of the [[Arc]]
  * @param width
  *   width in pixels
  * @param height
  *   height in pixels
  * @param startAngle
  *   start angle in degrees
  * @param arcAngle
  *   arc angle in degrees
  * @param rotationAngle
  *   rotation angle in degrees
  * @param hasBorder
  *   whether this [[Arc]] has a border
  * @param hasFilling
  *   whether this [[Arc]] has a filling
  * @param color
  *   color of the border
  * @param fillColor
  *   color of the filling
  */
class Arc(
    pos: Pos,
    val width: Double,
    val height: Double,
    val startAngle: Double,
    val arcAngle: Double,
    val rotationAngle: Double,
    val hasBorder: Boolean,
    val hasFilling: Boolean,
    val color: Color,
    val fillColor: Color,
    override val transformationMatrix: TransformationMatrix
) extends VectorGraphic:
  def this(
      pos: Pos = DefaultPosition,
      width: Double,
      height: Double,
      startAngle: Double = Angle.Zero.inDegrees,
      arcAngle: Double = Angle.FullAngleInDegrees,
      hasBorder: Boolean = ShapesHaveBordersByDefault,
      hasFilling: Boolean = ShapesHaveFillingsByDefault,
      color: Color = DefaultPrimaryColor,
      fillColor: Color = DefaultSecondaryColor
  ) =
    this(
      pos,
      width,
      height,
      startAngle,
      arcAngle,
      rotationAngle = DefaultRotationAngleInDegrees,
      hasBorder,
      hasFilling,
      color,
      fillColor,
      TransformationMatrix.identity
    )

  override lazy val position: Pos = pos

  private[this] lazy val corners: Seq[Pos] =
    val halfWidth  = width / 2.0
    val halfHeight = height / 2.0

    Seq(
      Transformer.rotate(position + (-halfWidth, -halfHeight), rotationAngle),
      Transformer.rotate(position + (halfWidth, -halfHeight), rotationAngle),
      Transformer.rotate(position + (halfWidth, halfHeight), rotationAngle),
      Transformer.rotate(position + (-halfWidth, halfHeight), rotationAngle)
    )

  lazy val isFullCycle: Boolean = arcAngle.abs >= Angle.FullAngleInDegrees

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

  override def copy(newPosition: Pos): PictureElement =
    internalCopy(newPosition = newPosition)

  override def copy(newMatrix: TransformationMatrix): PictureElement =
    internalCopy(newMatrix = newMatrix)

  private def internalCopy(
      newPosition: Pos = position,
      newWidth: Double = width,
      newHeight: Double = height,
      newStartAngle: Double = startAngle,
      newArcAngle: Double = arcAngle,
      newRotationAngle: Double = rotationAngle,
      newHasBorder: Boolean = hasBorder,
      newHasFilling: Boolean = hasFilling,
      newColor: Color = color,
      newFillColor: Color = fillColor,
      newMatrix: TransformationMatrix = transformationMatrix
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
      newHasBorder,
      newHasFilling,
      newColor,
      newFillColor,
      newMatrix
    )
