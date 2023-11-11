package smile.modeling

import smile.infrastructure.MathUtils

object Bounds:
  def apply(
      upperLeftXInPixels: Double,
      upperLeftYInPixels: Double,
      lowerRightXInPixels: Double,
      lowerRightYInPixels: Double
  ): Bounds =
    val (x0, x1) = MathUtils.sort(upperLeftXInPixels, lowerRightXInPixels)
    val (y0, y1) = MathUtils.sort(upperLeftYInPixels, lowerRightYInPixels)
    new Bounds(Pos(x0, y0), Pos(x1, y1))

  def apply(center: Pos, width: Int, height: Int): Bounds =
    apply(center, width.toDouble, height.toDouble)

  def apply(center: Pos, widthInPixels: Double, heightInPixels: Double): Bounds =

    val halfWidth        = widthInPixels / 2.0
    val halfHeight       = heightInPixels / 2.0
    val upperLeftCorner  = Pos(center.x - halfWidth, center.y - halfHeight)
    val lowerRightCorner = Pos(center.x + halfWidth, center.y + halfHeight)

    new Bounds(upperLeftCorner, lowerRightCorner)

case class Bounds(upperLeftCorner: Pos, lowerRightCorner: Pos):
  lazy val width: Len =
    Len(lowerRightCorner.x - upperLeftCorner.x)

  lazy val height: Len =
    Len(lowerRightCorner.y - upperLeftCorner.y)

  lazy val center: Pos = upperLeftCorner.centerBetween(lowerRightCorner)

  inline def moveBy(xOffset: Double, yOffset: Double): Bounds =
    Bounds(
      upperLeftCorner.moveBy(xOffset, yOffset),
      lowerRightCorner.moveBy(xOffset, yOffset)
    )

  inline def horizontalPositionFor(
      alignment: HorizontalAlignment,
      boundaryToBeAligned: Bounds
  ): Double =

    val offset =
      alignment match
        case HorizontalAlignment.Left   => Len.Zero
        case HorizontalAlignment.Center => width.half - boundaryToBeAligned.width.half
        case HorizontalAlignment.Right  => width - boundaryToBeAligned.width

    upperLeftCorner.x + offset.inPixels
  end horizontalPositionFor

  def verticalPositionFor(alignment: VerticalAlignment, boundaryToBeAligned: Bounds): Double =
    val offset =
      alignment match
        case VerticalAlignment.Top    => Len.Zero
        case VerticalAlignment.Middle => height.half - boundaryToBeAligned.height.half
        case VerticalAlignment.Bottom => height - boundaryToBeAligned.height

    upperLeftCorner.y + offset.inPixels
  end verticalPositionFor

object NullBounds extends Bounds(Pos(0, 0), Pos(0, 0)):
  override final lazy val width: Len  = Len(0)
  override final lazy val height: Len = Len(0)
