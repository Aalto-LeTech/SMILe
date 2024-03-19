package smile.modeling

import smile.infrastructure.MathUtils

/** Provides factory methods for creating `Bounds` objects, which represent rectangular areas in a
  * coordinate space.
  */
object Bounds:

  /** Creates a `Bounds` instance ensuring that the coordinates are sorted so the upper left is
    * truly at the top-left and the lower right is at the bottom-right.
    *
    * @param upperLeftX
    *   X-coordinate of the upper left corner in pixels.
    * @param upperLeftY
    *   Y-coordinate of the upper left corner in pixels.
    * @param lowerRightX
    *   X-coordinate of the lower right corner in pixels.
    * @param lowerRightY
    *   Y-coordinate of the lower right corner in pixels.
    * @return
    *   A new `Bounds` instance.
    */
  def apply(
      upperLeftX: Double,
      upperLeftY: Double,
      lowerRightX: Double,
      lowerRightY: Double
  ): Bounds =
    val (x0, x1) = MathUtils.sort(upperLeftX, lowerRightX)
    val (y0, y1) = MathUtils.sort(upperLeftY, lowerRightY)
    new Bounds(Pos(x0, y0), Pos(x1, y1))

  def apply(center: Pos, width: Int, height: Int): Bounds =
    apply(center, width.toDouble, height.toDouble)

  /** Creates a `Bounds` instance centered around a given point with specified width and height.
    *
    * @param center
    *   The center position.
    * @param width
    *   The width in pixels.
    * @param height
    *   The height in pixels.
    * @return
    *   A new `Bounds` instance.
    */
  def apply(center: Pos, width: Double, height: Double): Bounds =

    val halfWidth        = width / 2.0
    val halfHeight       = height / 2.0
    val upperLeftCorner  = Pos(center.x - halfWidth, center.y - halfHeight)
    val lowerRightCorner = Pos(center.x + halfWidth, center.y + halfHeight)

    new Bounds(upperLeftCorner, lowerRightCorner)

/** Represents the boundaries of a rectangular area, defined by its upper left and lower right
  * corners.
  *
  * @param upperLeftCorner
  *   The upper left corner position.
  * @param lowerRightCorner
  *   The lower right corner position.
  */
case class Bounds(upperLeftCorner: Pos, lowerRightCorner: Pos):
  /** The width of the bounds. */
  lazy val width: Len =
    Len(lowerRightCorner.x - upperLeftCorner.x)

  /** The height of the bounds. */
  lazy val height: Len =
    Len(lowerRightCorner.y - upperLeftCorner.y)

  /** The center position of the bounds. */
  lazy val center: Pos = upperLeftCorner.centerBetween(lowerRightCorner)

  /** The upper right corner of the bounds. */
  lazy val upperRightCorner: Pos = Pos(lowerRightCorner.x, upperLeftCorner.y)

  /** The lower left corner of the bounds. */
  lazy val lowerLeftCorner: Pos = Pos(upperLeftCorner.x, lowerRightCorner.y)

  /** Moves the bounds by the specified offsets.
    *
    * @param xOffset
    *   The horizontal offset.
    * @param yOffset
    *   The vertical offset.
    * @return
    *   A new `Bounds` instance moved by the specified offsets.
    */
  inline def moveBy(xOffset: Double, yOffset: Double): Bounds =
    Bounds(
      upperLeftCorner.moveBy(xOffset, yOffset),
      lowerRightCorner.moveBy(xOffset, yOffset)
    )

  /** Determines the horizontal position for alignment within these bounds.
    *
    * @param alignment
    *   The horizontal alignment.
    * @param boundaryToBeAligned
    *   The bounds to be aligned.
    * @return
    *   The horizontal position for the specified alignment.
    */
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

  /** Determines the vertical position for alignment within these bounds.
    *
    * @param alignment
    *   The vertical alignment.
    * @param boundaryToBeAligned
    *   The bounds to be aligned.
    * @return
    *   The vertical position for the specified alignment.
    */
  def verticalPositionFor(alignment: VerticalAlignment, boundaryToBeAligned: Bounds): Double =
    val offset =
      alignment match
        case VerticalAlignment.Top    => Len.Zero
        case VerticalAlignment.Middle => height.half - boundaryToBeAligned.height.half
        case VerticalAlignment.Bottom => height - boundaryToBeAligned.height

    upperLeftCorner.y + offset.inPixels
  end verticalPositionFor

/** Represents a special `Bounds` instance that effectively has no size and no position.
  */
object NullBounds extends Bounds(Pos(0, 0), Pos(0, 0)):
  override final lazy val width: Len  = Len(0)
  override final lazy val height: Len = Len(0)
