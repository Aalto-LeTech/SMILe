package smile.pictures

import smile.modeling.{Angle, Pos}

/** Factory object for creating polygons.
  */
object Pentagon:

  /** Magnitude of regular convex pentagon's rotational symmetry angle. */
  lazy val RotationalSymmetryAngle: Angle = Angle(72)

  /** The ratio of regular convex pentagon's diagonal length (i.e., width) and height. */
  private lazy val DiagonalPerHeightRatio: Double =
    (1.0 + Math.sqrt(5.0)) / Math.sqrt(5.0 + 2.0 * Math.sqrt(5.0))

  /** The ratio of regular convex pentagon's diagonal length and circumradius. */
  private lazy val DiagonalPerCircumradiusRatio: Double = Math.sqrt((5.0 + Math.sqrt(5.0)) / 2.0)

  /** Creates a pentagon based on its width, height, center position, fill style, and stroke style.
    *
    * @param width
    *   The width of the pentagon (diagonal length) in pixels.
    * @param height
    *   The height of the pentagon in pixels.
    * @param center
    *   The center position of the pentagon.
    * @param fillStyle
    *   The fill style of the pentagon.
    * @param strokeStyle
    *   The stroke style of the pentagon.
    * @return
    *   A `VectorGraphic` representing the pentagon.
    * @throws IllegalArgumentException
    *   If either width or height is negative.
    */
  def apply(
      width: Double,
      height: Double,
      center: Pos = Pos.Origin,
      fillStyle: Option[FillStyle],
      strokeStyle: Option[StrokeStyle]
  ): VectorGraphic =
    if width < 0 then
      throw new IllegalArgumentException(
        s"Pentagon's width cannot be negative (was: $width)."
      )
    if height < 0 then
      throw new IllegalArgumentException(
        s"Pentagon's height cannot be negative (was: $height)."
      )

    val circumradius = limitCircumradiusTo(width, height)

    apply(circumradius, center, fillStyle, strokeStyle)
  end apply

  /** Creates a pentagon based on its circumradius, center position, fill style, and stroke style.
    *
    * @param circumradius
    *   The circumradius of the pentagon in pixels.
    * @param center
    *   The center position of the pentagon.
    * @param fillStyle
    *   The fill style of the pentagon.
    * @param strokeStyle
    *   The stroke style of the pentagon.
    * @return
    *   A `VectorGraphic` representing the pentagon.
    * @throws IllegalArgumentException
    *   If the circumradius is negative.
    */
  def apply(
      circumradius: Double,
      center: Pos,
      fillStyle: Option[FillStyle],
      strokeStyle: Option[StrokeStyle]
  ): VectorGraphic =
    if circumradius < 0 then
      throw new IllegalArgumentException(
        s"Length of pentagon's circumradius cannot be negative (was: $circumradius)."
      )

    val points = pointsFor(circumradius, Angle.Zero)

    Polygon(center, points, fillStyle, strokeStyle)
  end apply

  /** Generates the points for a pentagon given its circumradius and a starting angle.
    *
    * @param circumradius
    *   The circumradius of the pentagon in pixels.
    * @param startAngle
    *   The starting angle for the first point of the pentagon.
    * @return
    *   A sequence of positions defining the vertices of the pentagon.
    */
  def pointsFor(circumradius: Double, startAngle: Angle): Seq[Pos] =

    val firstPointCandidate = Pos(0, -circumradius)
    val firstPoint          = firstPointCandidate.rotateByAroundOrigin(startAngle.inDegrees)

    val symmetryAngle  = RotationalSymmetryAngle.inDegrees
    val rotationAngles = Seq.tabulate(5)(n => n * symmetryAngle).tail

    firstPoint +: rotationAngles.map(firstPoint.rotateByAroundOrigin)
  end pointsFor

  /** Calculates the maximum possible circumradius for a pentagon with given width and height,
    * ensuring that the pentagon fits within these dimensions.
    *
    * @param width
    *   The width constraint in pixels.
    * @param height
    *   The height constraint in pixels.
    * @return
    *   The maximum possible circumradius that fits within the given dimensions.
    */
  def limitCircumradiusTo(width: Double, height: Double): Double =
    val heightBasedDiagonal = diagonalFromHeight(height)
    val effectiveDiagonal   = heightBasedDiagonal.min(width)
    circumradiusFromDiagonal(effectiveDiagonal)

  private def diagonalFromHeight(height: Double): Double =
    DiagonalPerHeightRatio * height

  private def circumradiusFromDiagonal(diagonal: Double): Double =
    diagonal / DiagonalPerCircumradiusRatio
