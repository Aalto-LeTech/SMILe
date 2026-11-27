package smile.modeling

/** Provides factory methods for creating `Bounds` objects, which represent rectangular areas in a
  * coordinate space.
  */
object Bounds:

  /** Creates a `Bounds` instance from corner coordinates in pixels, sorting the coordinates so the
    * upper left is at the top-left and the lower right is at the bottom-right.
    */
  def apply(
      upperLeftX: Double,
      upperLeftY: Double,
      lowerRightX: Double,
      lowerRightY: Double
  ): Bounds =
    val (x0, x1) = sorted(upperLeftX, lowerRightX)
    val (y0, y1) = sorted(upperLeftY, lowerRightY)
    new Bounds(Pos(x0, y0), Pos(x1, y1))

  private def sorted(a: Double, b: Double): (Double, Double) =
    if a < b then (a, b) else (b, a)

  /** Creates a `Bounds` instance from two corner positions, sorting the coordinates so the corners
    * are the upper left and the lower right. This overload shadows the case class's own `apply`,
    * which would give corners in reverse order negative dimensions.
    */
  def apply(upperLeftCorner: Pos, lowerRightCorner: Pos): Bounds =
    apply(upperLeftCorner.x, upperLeftCorner.y, lowerRightCorner.x, lowerRightCorner.y)

  def apply(center: Pos, width: Int, height: Int): Bounds =
    apply(center, width.toDouble, height.toDouble)

  /** Creates a `Bounds` instance centered around a given point with specified width and height in
    * pixels.
    */
  def apply(center: Pos, width: Double, height: Double): Bounds =

    val halfWidth  = width / 2.0
    val halfHeight = height / 2.0

    // Goes through the apply that sorts, so a negative width or height gives the same box as a
    // positive one.
    apply(
      center.x - halfWidth,
      center.y - halfHeight,
      center.x + halfWidth,
      center.y + halfHeight
    )

/** Represents the boundaries of a rectangular area, defined by its upper left and lower right
  * corners.
  */
final case class Bounds(upperLeftCorner: Pos, lowerRightCorner: Pos) derives CanEqual:
  def width: Double = lowerRightCorner.x - upperLeftCorner.x

  def height: Double = lowerRightCorner.y - upperLeftCorner.y

  def center: Pos = upperLeftCorner.centerBetween(lowerRightCorner)

  def upperRightCorner: Pos = Pos(lowerRightCorner.x, upperLeftCorner.y)

  def lowerLeftCorner: Pos = Pos(upperLeftCorner.x, lowerRightCorner.y)

  /** Returns these bounds moved by the specified offsets. */
  def moveBy(xOffset: Double, yOffset: Double): Bounds =
    Bounds(
      upperLeftCorner.moveBy(xOffset, yOffset),
      lowerRightCorner.moveBy(xOffset, yOffset)
    )

  /** Determines the x coordinate of the left edge of `boundaryToAlign` when it is aligned within
    * these bounds.
    */
  def horizontalPositionFor(
      align: Align.Horizontal,
      boundaryToAlign: Bounds
  ): Double =

    val offset =
      align match
        case Align.Horizontal.Left   => 0.0
        case Align.Horizontal.Middle => width / 2 - boundaryToAlign.width / 2
        case Align.Horizontal.Right  => width - boundaryToAlign.width

    upperLeftCorner.x + offset
  end horizontalPositionFor

  /** Determines the y coordinate of the top edge of `boundaryToAlign` when it is aligned within
    * these bounds.
    */
  def verticalPositionFor(align: Align.Vertical, boundaryToAlign: Bounds): Double =
    val offset =
      align match
        case Align.Vertical.Top    => 0.0
        case Align.Vertical.Middle => height / 2 - boundaryToAlign.height / 2
        case Align.Vertical.Bottom => height - boundaryToAlign.height

    upperLeftCorner.y + offset
  end verticalPositionFor
