package smile.pictures

import smile.Settings.DefaultPrimaryColor
import smile.colors.{Color, PresetColor}
import smile.modeling.Pos

object Line:
  def apply(startX: Double, startY: Double, endX: Double, endY: Double): VectorGraphic =
    apply(Pos(startX, startY), Pos(endX, endY))

  def apply(
      startX: Double,
      startY: Double,
      endX: Double,
      endY: Double,
      color: Color
  ): VectorGraphic =
    apply(Pos(startX, startY), Pos(endX, endY), color)

  def apply(start: Pos, end: Pos, color: Color = DefaultPrimaryColor): VectorGraphic =

    val halfWidth  = (end.x - start.x) / 2.0
    val halfHeight = (end.y - start.y) / 2.0

    val adjustedStart = Pos(-halfWidth, -halfHeight)
    val adjustedEnd   = Pos(halfWidth, halfHeight)

    val points = Seq(adjustedStart, adjustedEnd)

    // TODO: Change to Polyline after it is implemented
    new Polygon(
      start + (halfWidth, halfHeight),
      points,
      Pos.Origo,
      hasBorder = true,
      hasFilling = false,
      color = color,
      fillColor = PresetColor.Transparent
    )
