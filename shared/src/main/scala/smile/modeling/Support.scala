package smile.modeling

import java.util.concurrent.atomic.LongAdder

import smile.modeling.{Affine, Bounds}
import smile.pictures.StrokeStyle

/** World bounds of local shapes through support functions.
  *
  * For the world affine `M = (L, t)`, a point `p` of the local geometry lands at `L p + t`, and its
  * coordinate along the world axis `e` is `(Lᵀe) · p + e · t`. The largest such coordinate over the
  * shape is the shape's support along `Lᵀe` plus `e · t`, so four support evaluations give the
  * tight upright box around the transformed shape, under shears and mirrors as well as turns. The
  * stroke lives in the local frame and is transformed with the shape, so the same holds for the
  * support of the stroked ink (see `smile.modeling.LocalShape`).
  */
private[smile] object Support:

  /** How many times `smile.modeling.Support.worldBounds` has measured a leaf, each time with four
    * support evaluations. Tests read it before and after the work they measure, to count what
    * reading bounds costs. Measuring from several threads at once keeps the count exact.
    */
  private[smile] def leafEvaluations: Long = evaluations.sum

  private val evaluations = new LongAdder

  /** The tight upright box around what `shape` paints, with `stroke` if it has one, once `m` has
    * placed it.
    *
    * An affine that keeps the axes (moves, axis scalings, right-angle turns and mirrors) maps the
    * local box of the ink onto the world box exactly, corner for corner. So scaling a shape along
    * the world axes scales its bounds by exactly the same factors, which is what makes `scaleTo`
    * exact in one step.
    */
  def worldBounds(shape: LocalShape, stroke: Option[StrokeStyle], m: Affine): Bounds =
    evaluations.increment()
    def h(vx: Double, vy: Double): Double = inkSupport(shape, stroke, vx, vy)

    if m.keepsAxes then m.boxBounds(-h(-1, 0), -h(0, -1), h(1, 0), h(0, 1))
    else
      // x' = a * x + c * y + e and y' = b * x + d * y + f, so Lᵀ takes the world's x axis to
      // (a, c) and its y axis to (b, d).
      Bounds(m.e - h(-m.a, -m.c), m.f - h(-m.b, -m.d), m.e + h(m.a, m.c), m.f + h(m.b, m.d))

  /** The length of `(vx, vy)`, which a round stroke and a circle reach along it.
    *
    * A turn's rows have a sine and a cosine whose squares sum to 1 only up to a rounding error, so
    * their length can come out a unit in the last place short of 1, and a turned circle a little
    * narrower than its diameter. A length within a few units in the last place of 1 is therefore
    * taken to be exactly 1, which keeps a circle's bounds exact under every turn. The bound is then
    * short by at most 9e-16 of the radius.
    *
    * This guarantees an exact diameter only for turns that do not scale (and their mirror images),
    * whose rows have length 1 up to rounding. A turn combined with a scaling has rows of another
    * length, which is left as `hypot` computes it, so such a circle's diameter is exact only up to
    * that rounding.
    */
  private[modeling] def length(vx: Double, vy: Double): Double =
    val exact = math.hypot(vx, vy)
    if (exact - 1).abs <= UnitSnap then 1.0 else exact

  private val UnitSnap = 4 * Math.ulp(1.0)

  /** The support of the ink of `shape` along `(vx, vy)` in its local frame. The ink is its fill
    * geometry and, if it is stroked, the stroke's outline. Every support evaluation of
    * `smile.modeling.Support.worldBounds` goes through here.
    */
  private def inkSupport(
      shape: LocalShape,
      stroke: Option[StrokeStyle],
      vx: Double,
      vy: Double
  ): Double =
    stroke match
      case Some(style) => shape.strokedSupport(vx, vy, style)
      case None        => shape.support(vx, vy)
end Support
