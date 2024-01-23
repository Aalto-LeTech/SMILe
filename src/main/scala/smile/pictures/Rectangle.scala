package smile.pictures

import smile.Settings.{DefaultPrimaryColor, DefaultSecondaryColor, ShapesHaveBordersByDefault, ShapesHaveFillingsByDefault}
import smile.colors.Color
import smile.modeling.Pos

object Rectangle:
  def apply(sideLengthInPixels: Double): VectorGraphic =
    apply(
      sideLengthInPixels,
      hasBorder = ShapesHaveBordersByDefault,
      hasFilling = ShapesHaveFillingsByDefault,
      color = DefaultPrimaryColor,
      fillColor = DefaultSecondaryColor
    )

  def apply(
      sideLengthInPixels: Double,
      hasBorder: Boolean,
      hasFilling: Boolean,
      color: Color,
      fillColor: Color
  ): VectorGraphic =
    apply(sideLengthInPixels, Pos.Origin, hasBorder, hasFilling, color, fillColor)

  def apply(sideLengthInPixels: Double, center: Pos): VectorGraphic =
    apply(
      sideLengthInPixels,
      center,
      hasBorder = ShapesHaveBordersByDefault,
      hasFilling = ShapesHaveFillingsByDefault,
      color = DefaultPrimaryColor,
      fillColor = DefaultSecondaryColor
    )

  def apply(
      sideLengthInPixels: Double,
      center: Pos,
      hasBorder: Boolean,
      hasFilling: Boolean,
      color: Color,
      fillColor: Color
  ): VectorGraphic =
    require(
      sideLengthInPixels >= 0.0,
      s"Rectangle's side length cannot be negative (was: $sideLengthInPixels)."
    )
    apply(sideLengthInPixels, sideLengthInPixels, center, hasBorder, hasFilling, color, fillColor)

  def apply(baseLengthInPixels: Double, heightInPixels: Double): VectorGraphic =
    apply(
      baseLengthInPixels,
      heightInPixels,
      Pos.Origin,
      hasBorder = ShapesHaveBordersByDefault,
      hasFilling = ShapesHaveFillingsByDefault,
      color = DefaultPrimaryColor,
      fillColor = DefaultSecondaryColor
    )

  def apply(
      baseLengthInPixels: Double,
      heightInPixels: Double,
      hasBorder: Boolean,
      hasFilling: Boolean,
      color: Color,
      fillColor: Color
  ): VectorGraphic =
    apply(baseLengthInPixels, heightInPixels, Pos.Origin, hasBorder, hasFilling, color, fillColor)

  def apply(
      baseLengthInPixels: Double,
      heightInPixels: Double,
      center: Pos,
      hasBorder: Boolean = ShapesHaveBordersByDefault,
      hasFilling: Boolean = ShapesHaveFillingsByDefault,
      color: Color = DefaultPrimaryColor,
      fillColor: Color = DefaultSecondaryColor
  ): VectorGraphic =
    require(
      baseLengthInPixels >= 0.0,
      s"Rectangle's base length cannot be negative (was: $baseLengthInPixels)."
    )
    require(heightInPixels >= 0.0, s"Rectangle's height cannot be negative (was: $heightInPixels).")

    val cornerPoints =
      if baseLengthInPixels > 0.0 && heightInPixels > 0.0 then
        val halfWidth  = (baseLengthInPixels - 1) / 2.0
        val halfHeight = (heightInPixels - 1) / 2.0

        Seq(
          Pos(-halfWidth, -halfHeight),
          Pos(halfWidth, -halfHeight),
          Pos(halfWidth, halfHeight),
          Pos(-halfWidth, halfHeight)
        )
      else Seq()

    Polygon(
      center,
      cornerPoints,
      hasBorder,
      hasFilling,
      color,
      fillColor
    )
