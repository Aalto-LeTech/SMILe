package smile.pictures

import smile.colors.Color
import smile.modeling.{Angle, Len, Pos}

object Ellipse:
  def apply(
      center: Pos,
      semiMajorAxisInPixels: Double,
      semiMinorAxisInPixels: Double,
      hasBorder: Boolean,
      hasFilling: Boolean,
      color: Color,
      fillColor: Color
  ): VectorGraphic =
    apply(
      center,
      Len(2 * semiMajorAxisInPixels),
      Len(2 * semiMinorAxisInPixels),
      hasBorder,
      hasFilling,
      color,
      fillColor
    )

  def apply(
      center: Pos,
      width: Len,
      height: Len,
      hasBorder: Boolean,
      hasFilling: Boolean,
      color: Color,
      fillColor: Color
  ): VectorGraphic =
    Arc(
      center,
      width.inPixels,
      height.inPixels,
      startAngle = Angle.Zero.inDegrees,
      arcAngle = Angle.FullAngleInDegrees,
      hasBorder,
      hasFilling,
      color,
      fillColor
    )
