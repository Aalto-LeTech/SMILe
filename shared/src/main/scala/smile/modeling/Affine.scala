package smile.modeling

/** An affine map of the plane, with its coefficients in the order a canvas, Java2D and SVG all take
  * them. `(x, y)` goes to `(a * x + c * y + e, b * x + d * y + f)`. That makes it
  * `setTransform(a, b, c, d, e, f)` on a canvas, `new AffineTransform(a, b, c, d, e, f)` in Java2D
  * and `matrix(a b c d e f)` in SVG.
  *
  * Every affine is normalized when it is made, so that equal maps are equal values. Linear
  * coefficients within [[smile.modeling.Affine.Tolerance]] times the largest of `a` to `d` of 0, 1
  * or -1 become exactly that, `e` and `f` are rounded to the nearest multiple of 2^-20, and `-0.0`
  * becomes `0.0`. Equality is exact over the normalized values.
  *
  * A public affine is always invertible and finite.
  */
final case class Affine private (
    a: Double,
    b: Double,
    c: Double,
    d: Double,
    e: Double,
    f: Double
) derives CanEqual:

  /** Where this map takes the point `(x, y)`. */
  def apply(x: Double, y: Double): Pos = Pos(mapX(x, y), mapY(x, y))

  /** Where this map takes `point`. */
  def apply(point: Pos): Pos = apply(point.x, point.y)

  /** The x coordinate that this map takes the point `(x, y)` to. */
  def mapX(x: Double, y: Double): Double = a * x + c * y + e

  /** The y coordinate that this map takes the point `(x, y)` to. */
  def mapY(x: Double, y: Double): Double = b * x + d * y + f

  /** This map followed by `next`. */
  def andThen(next: Affine): Affine =
    val (na, nb, nc, nd, ne, nf) = (
      next.a * a + next.c * b,
      next.b * a + next.d * b,
      next.a * c + next.c * d,
      next.b * c + next.d * d,
      next.a * e + next.c * f + next.e,
      next.b * e + next.d * f + next.f
    )
    // A map that flattens the plane, as that of a bitmap scaled to zero size, composes unchecked.
    // The product of two invertible maps must be invertible too.
    if determinant == 0 || next.determinant == 0 then Affine.unchecked(na, nb, nc, nd, ne, nf)
    else Affine(na, nb, nc, nd, ne, nf)

  /** The map that undoes this one. */
  def inverse: Affine =
    val (ia, ib, ic, id, ie, jf) = inverseCoefficients
    Affine(ia, ib, ic, id, ie, jf)

  /** The map that undoes this one, or `None` when this one flattens the plane onto a line. */
  private[smile] def inverseOption: Option[Affine] =
    if determinant == 0 || !determinant.isFinite then None
    else
      val (ia, ib, ic, id, ie, jf) = inverseCoefficients
      Some(Affine.unchecked(ia, ib, ic, id, ie, jf))

  private def inverseCoefficients: (Double, Double, Double, Double, Double, Double) =
    val det      = determinant
    val (ia, ib) = (d / det, -b / det)
    val (ic, id) = (-c / det, a / det)
    (ia, ib, ic, id, -(ia * e + ic * f), -(ib * e + id * f))

  /** The signed factor by which this map scales areas. It is never zero for an affine made through
    * the public API.
    */
  def determinant: Double = a * d - b * c

  /** This map followed by a move by `(dx, dy)`. */
  private[smile] def translatedBy(dx: Double, dy: Double): Affine =
    Affine.unchecked(a, b, c, d, e + dx, f + dy)

  /** How long the map makes a unit step along the axis it stretches more. */
  private[smile] def largestAxisScale: Double = math.hypot(a, b).max(math.hypot(c, d))

  /** Whether every coefficient is a finite number. */
  private[smile] def isFinite: Boolean =
    a.isFinite && b.isFinite && c.isFinite && d.isFinite && e.isFinite && f.isFinite

  /** Whether the map has no shear and takes the axes to the axes. It only moves, scales along the
    * axes, turns by right angles and mirrors.
    */
  def keepsAxes: Boolean = (b == 0 && c == 0) || (a == 0 && d == 0)

  /** Whether the map only moves, mirrors and turns by right angles, without resizing. It lays a
    * pixel grid on another pixel grid. Its linear part has a one or a minus one in each row and
    * column, and zeros elsewhere.
    */
  def keepsPixelGrid: Boolean =
    def isUnit(value: Double) = value.abs == 1
    val axisAligned           = b == 0 && c == 0 && isUnit(a) && isUnit(d)
    val turnedByRightAngle    = a == 0 && d == 0 && isUnit(b) && isUnit(c)
    axisAligned || turnedByRightAngle

  /** Whether the map turns the axes by an angle other than a multiple of 90 degrees, or shears
    * them, so that a pixel grid is no longer parallel to the axes.
    */
  private[smile] def turnsAxes: Boolean = !keepsAxes

  /** The box around where this map takes the rectangle with the given sides. */
  def boxBounds(left: Double, top: Double, right: Double, bottom: Double): Bounds =
    // Plain arithmetic. A collection's min would box every coordinate, and bounds are measured
    // for every element of every frame.
    val x0 = mapX(left, top)
    val x1 = mapX(right, top)
    val x2 = mapX(left, bottom)
    val x3 = mapX(right, bottom)
    val y0 = mapY(left, top)
    val y1 = mapY(right, top)
    val y2 = mapY(left, bottom)
    val y3 = mapY(right, bottom)
    Bounds(
      math.min(math.min(x0, x1), math.min(x2, x3)),
      math.min(math.min(y0, y1), math.min(y2, y3)),
      math.max(math.max(x0, x1), math.max(x2, x3)),
      math.max(math.max(y0, y1), math.max(y2, y3))
    )
end Affine

object Affine:
  /** How far from an exact 0, 1 or -1 a linear coefficient may be, as a fraction of the largest of
    * `a` to `d`, to become exactly that.
    */
  val Tolerance: Double = 1e-12

  private val Grid = 1048576.0 // 2^20

  val Identity: Affine = new Affine(1, 0, 0, 1, 0, 0)

  /** The normalized affine with these coefficients.
    *
    * @throws IllegalArgumentException
    *   if a coefficient is not finite or the map flattens the plane onto a line
    */
  def apply(a: Double, b: Double, c: Double, d: Double, e: Double, f: Double): Affine =
    val made = unchecked(a, b, c, d, e, f)
    require(made.isFinite, s"An affine needs finite coefficients, not $made")
    require(
      made.determinant != 0 && made.determinant.isFinite,
      s"An affine must be invertible: $made"
    )
    made

  /** The normalized affine with these coefficients, even if it is not finite or not invertible, for
    * old code that scales a bitmap to nothing.
    */
  private[smile] def unchecked(
      a: Double,
      b: Double,
      c: Double,
      d: Double,
      e: Double,
      f: Double
  ): Affine =
    def onGrid(value: Double): Double = Math.rint(value * Grid) / Grid + 0.0
    val (na, nb, nc, nd)              =
      if !(a.isFinite && b.isFinite && c.isFinite && d.isFinite) then
        // Values that are not finite have nothing to snap against. The public apply method reports
        // the failure.
        (a + 0.0, b + 0.0, c + 0.0, d + 0.0)
      else
        val margin                      = Tolerance * a.abs.max(b.abs).max(c.abs).max(d.abs)
        def snap(value: Double): Double =
          if value.abs <= margin then 0.0
          else if (value - 1).abs <= margin then 1.0
          else if (value + 1).abs <= margin then -1.0
          else value + 0.0
        val (sa, sb, sc, sd) = (snap(a), snap(b), snap(c), snap(d))
        // A map as anisotropic as 1e12 by 1 must not lose its smaller axis to the snapping.
        if sa * sd - sb * sc == 0 && a * d - b * c != 0 then (a + 0.0, b + 0.0, c + 0.0, d + 0.0)
        else (sa, sb, sc, sd)
    new Affine(na, nb, nc, nd, onGrid(e), onGrid(f))

  /** Moving by `(dx, dy)`. */
  def translation(dx: Double, dy: Double): Affine = Affine(1, 0, 0, 1, dx, dy)

  /** Scaling by the given factors relative to the origin. */
  def scaling(sx: Double, sy: Double): Affine = Affine(sx, 0, 0, sy, 0, 0)

  /** Scaling that old code may take to zero. */
  private[smile] def uncheckedScaling(sx: Double, sy: Double): Affine =
    unchecked(sx, 0, 0, sy, 0, 0)

  /** The map that takes the rectangle `from` onto the rectangle `to`, axis by axis. */
  private[smile] def between(from: Bounds, to: Bounds): Affine =
    val sx = to.width / from.width
    val sy = to.height / from.height
    translation(-from.center.x, -from.center.y)
      .andThen(uncheckedScaling(sx, sy))
      .andThen(translation(to.center.x, to.center.y))

  /** Turning by `degrees` around `pivot`, counterclockwise on screen for a positive angle, the way
    * [[smile.modeling.Pos.rotateAround]] turns a point. Right angles are exact.
    */
  def rotation(degrees: Double, pivot: Pos): Affine =
    require(degrees.isFinite, s"An angle must be finite, not $degrees")
    val (sin, cos) = Angles.exactSinCos(degrees)
    Affine(
      cos,
      -sin,
      sin,
      cos,
      pivot.x - sin * pivot.y - cos * pivot.x,
      pivot.y + sin * pivot.x - cos * pivot.y
    )

  /** Mirroring across the vertical line through `center` (`horizontally`), the horizontal line
    * through it (`vertically`), or both.
    */
  def mirroring(horizontally: Boolean, vertically: Boolean, center: Pos): Affine =
    val sx = if horizontally then -1.0 else 1.0
    val sy = if vertically then -1.0 else 1.0
    Affine(sx, 0, 0, sy, center.x * (1 - sx), center.y * (1 - sy))

end Affine
