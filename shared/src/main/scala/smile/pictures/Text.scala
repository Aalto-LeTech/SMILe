package smile.pictures

import smile.modeling.{BoundaryCalculator, Bounds, Pos, Transformer}

/** Represents a single line of text as a graphical element.
  *
  * @param pos
  *   The position of the text.
  * @param customBounds
  *   Optional custom bounds for the text.
  * @param content
  *   The content of the text.
  * @param typeface
  *   The typeface used for rendering the text.
  * @param fillStyle
  *   Optional fill style for the text.
  * @param strokeStyle
  *   Optional stroke style for the text.
  */
class Text(
    pos: Pos,
    val customBounds: Option[Bounds],
    val content: String,
    val typeface: String,
    val size: Double,
    val weight: Int,
    override val fillStyle: Option[FillStyle],
    override val strokeStyle: Option[StrokeStyle]
) extends VectorGraphic:

  override def copy(newPosition: Pos): Text = new Text(
    newPosition,
    customBounds,
    content,
    typeface,
    size,
    weight,
    fillStyle,
    strokeStyle
  )

  private def internalCopy(
      newPosition: Pos = position,
      newCustomBounds: Option[Bounds] = customBounds,
      newContent: String = content,
      newTypeface: String = typeface,
      newSize: Double = size,
      newWeight: Int = weight,
      newFillStyle: Option[FillStyle] = fillStyle,
      newStrokeStyle: Option[StrokeStyle] = strokeStyle
  ): Text =
    new Text(
      newPosition,
      newCustomBounds,
      newContent,
      newTypeface,
      newSize,
      newWeight,
      newFillStyle,
      newStrokeStyle
    )

  override lazy val boundary: Bounds = customBounds.getOrElse(BoundaryCalculator.fromText(this))

  override lazy val position: Pos = pos

  override def scaleBy(horizontalFactor: Double, verticalFactor: Double): Text =
    val newBounds = Transformer.scale(
      Seq(boundary.upperLeftCorner, boundary.lowerRightCorner),
      horizontalFactor,
      verticalFactor
    )
    internalCopy(newCustomBounds =
      Some(
        Bounds(
          newBounds.head,
          newBounds.last
        )
      )
    )

  override def scaleBy(
      horizontalFactor: Double,
      verticalFactor: Double,
      relativityPoint: Pos
  ): Text =
    val newBounds = Transformer.scale(
      Seq(boundary.upperLeftCorner, boundary.lowerRightCorner),
      horizontalFactor,
      verticalFactor,
      relativityPoint
    )
    internalCopy(newCustomBounds =
      Some(
        Bounds(
          newBounds.head,
          newBounds.last
        )
      )
    )

  override def rotateBy(angle: Double, centerOfRotation: Pos): Text = ??? // TODO

  override def rotateByAroundOrigin(angle: Double): Text = ??? // TODO
