package smile.testing

import smile.colors.Color
import smile.modeling.{Bounds, Pos}

/* Questions about where colors are in a raster. Positions are in pixel coordinates. A pixel's
 * position is its upper left corner, and its center is half a pixel further. Two colors are the
 * same within a tolerance when no channel differs by more than it, after premultiplying by alpha,
 * so fully transparent pixels are the same whatever their nominal color. */

/** How much of `color` over `over` a pixel is, from 0 to 1, if it lies within `tolerance` per
  * channel of the way between the two colors.
  */
private def blendCoverage(argb: Int, color: Color, over: Color, tolerance: Int): Option[Double] =
  val pixel    = Seq((argb >> 16) & 0xff, (argb >> 8) & 0xff, argb & 0xff).map(_.toDouble)
  val target   = Seq(color.red, color.green, color.blue).map(_.toDouble)
  val base     = Seq(over.red, over.green, over.blue).map(_.toDouble)
  val way      = target.zip(base).map(_ - _)
  val length   = way.map(step => step * step).sum
  val coverage =
    if length == 0 then 1.0
    else (pixel.zip(base).map(_ - _).zip(way).map(_ * _).sum / length).max(0).min(1)
  val residual = pixel.indices.map(i => (pixel(i) - (base(i) + coverage * way(i))).abs).max
  Option.when(residual <= tolerance)(coverage)

private def distance(argb: Int, color: Color): Int =
  ParityMetric.delta(ParityMetric.premultiplied(argb), ParityMetric.premultiplied(color.toARGBInt))

extension (raster: Raster)

  /** The indices of the pixels whose centers are within `radius` of `at`. */
  private def indicesAround(at: Pos, radius: Double): Seq[Int] =
    val (left, right) = ((at.x - radius - 0.5).floor.toInt, (at.x + radius - 0.5).ceil.toInt)
    val (top, bottom) = ((at.y - radius - 0.5).floor.toInt, (at.y + radius - 0.5).ceil.toInt)
    for
      y <- top.max(0) to bottom.min(raster.height - 1)
      x <- left.max(0) to right.min(raster.width - 1)
      if math.hypot(x + 0.5 - at.x, y + 0.5 - at.y) <= radius
    yield y * raster.width + x

  private def matching(color: Color, tolerance: Int): Seq[Int] =
    raster.argb.indices.filter(index => distance(raster.argb(index), color) <= tolerance)

  private def centerOf(index: Int): Pos =
    Pos(index % raster.width + 0.5, index / raster.width + 0.5)

  /** The colors of the pixels whose centers are within `radius` of `at`. */
  def colorsAround(at: Pos, radius: Double): Seq[Color] =
    indicesAround(at, radius).map(index => new Color(raster.argb(index)))

  /** Whether a pixel of `color` has its center within `radius` of `at`. */
  def hasColorAround(color: Color, at: Pos, radius: Double, tolerance: Int = 16): Boolean =
    indicesAround(at, radius).exists(index => distance(raster.argb(index), color) <= tolerance)

  /** Whether a pixel whose center is within `radius` of `at` is `color` blended over `over`,
    * covering at least `minCoverage` of it. This is how a thin antialiased line or edge looks. Its
    * pixels lie on the way from the background's color to the line's. A pixel counts when it is
    * within `tolerance` per channel of that way, so a different color or a gray does not count
    * however close it is. Opaque colors are assumed.
    */
  def hasBlendAround(
      color: Color,
      over: Color,
      at: Pos,
      radius: Double,
      minCoverage: Double = 0.4,
      tolerance: Int = 16
  ): Boolean =
    indicesAround(at, radius).exists(index =>
      blendCoverage(raster.argb(index), color, over, tolerance).exists(_ >= minCoverage)
    )

  /** The center of the pixel of `color` nearest to `at`, if there is one. */
  def nearestOf(color: Color, at: Pos, tolerance: Int = 16): Option[Pos] =
    matching(color, tolerance).map(centerOf).minByOption(p => math.hypot(p.x - at.x, p.y - at.y))

  /** The most common color among the pixels whose centers lie in `area`. Ties go to the color met
    * first in row order. Transparent when no pixel lies in it.
    */
  def dominantColor(area: Bounds): Color =
    val inside = raster.argb.indices.filter: index =>
      val center = centerOf(index)
      center.x >= area.upperLeftCorner.x && center.x < area.lowerRightCorner.x &&
      center.y >= area.upperLeftCorner.y && center.y < area.lowerRightCorner.y
    if inside.isEmpty then new Color(0)
    else
      val counts = inside.groupMapReduce(raster.argb(_))(_ => 1)(_ + _)
      val most   = counts.values.max
      new Color(raster.argb(inside.find(index => counts(raster.argb(index)) == most).get))

  /** The box around the pixels of `color`, if there are any. */
  def boundsOf(color: Color, tolerance: Int = 16): Option[Bounds] =
    val found = matching(color, tolerance)
    Option.when(found.nonEmpty):
      val xs = found.map(_ % raster.width)
      val ys = found.map(_ / raster.width)
      Bounds(Pos(xs.min, ys.min), Pos(xs.max + 1, ys.max + 1))

  /** The mean of the centers of the pixels of `color`, if there are any. */
  def centroidOf(color: Color, tolerance: Int = 16): Option[Pos] =
    val found = matching(color, tolerance).map(centerOf)
    Option.when(found.nonEmpty)(
      Pos(found.map(_.x).sum / found.size, found.map(_.y).sum / found.size)
    )
