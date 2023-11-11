package smile.pictures

import smile.Settings.*
import smile.colors.Color
import smile.modeling.{Angle, Len, Pos}

object Circle:
  
  def apply(center: Pos, radiusInPixels: Double): VectorGraphic =
    apply(
      center,
      radiusInPixels,
      hasBorder = ShapesHaveBordersByDefault,
      hasFilling = ShapesHaveFillingsByDefault,
      color = DefaultPrimaryColor,
      fillColor = DefaultSecondaryColor
    )

  def apply(
      center: Pos,
      radiusInPixels: Double,
      hasBorder: Boolean,
      hasFilling: Boolean,
      color: Color,
      fillColor: Color
  ): VectorGraphic =
    apply(center, Len(radiusInPixels * 2), hasBorder, hasFilling, color, fillColor)

  def apply(
      center: Pos,
      diameter: Len = Len(DefaultCircleRadiusInPixels),
      hasBorder: Boolean = ShapesHaveBordersByDefault,
      hasFilling: Boolean = ShapesHaveFillingsByDefault,
      color: Color = DefaultPrimaryColor,
      fillColor: Color = DefaultSecondaryColor
  ): VectorGraphic =
    new Arc(
      center,
      diameter.inPixels,
      diameter.inPixels,
      startAngle = Angle.Zero.inDegrees,
      arcAngle = Angle.FullAngleInDegrees,
      hasBorder,
      hasFilling,
      color,
      fillColor
    )
