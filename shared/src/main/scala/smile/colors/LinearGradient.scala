package smile.colors

import smile.modeling.Pos

object LinearGradient:
  /** Defines the cycle methods available for a `LinearGradient`.
    *
    * These cycle methods determine how the gradient pattern continues beyond its defined start and
    * end points. Every renderer honors them. Java2D and SVG have the three methods built in. The
    * canvas only continues the end colors, so it is handed a gradient with the cycles written out
    * as stops across everything it can show (see `smile.colors.LinearGradient.writtenOutOver`).
    */
  enum CycleMethod derives CanEqual:
    /** The gradient does not repeat. It displays the terminal colors beyond the start and end
      * points.
      */
    case NoCycle

    /** The gradient pattern repeats by mirroring the gradient along the axis perpendicular to the
      * gradient's start and end points.
      */
    case Reflect

    /** The gradient pattern repeats by cycling the colors from start to end. */
    case Repeat

  /** A gradient from `from` at `start` to `to` at `end` that continues its end colors beyond them.
    */
  def apply(start: Pos, end: Pos, from: Color, to: Color): LinearGradient =
    LinearGradient(start, end, Seq(0.0 -> from, 1.0 -> to))

  /** A gradient from `from` at `start` to `to` at `end` that continues beyond them by
    * `cycleMethod`.
    */
  def apply(
      start: Pos,
      end: Pos,
      from: Color,
      to: Color,
      cycleMethod: CycleMethod
  ): LinearGradient =
    LinearGradient(start, end, Seq(0.0 -> from, 1.0 -> to), cycleMethod)

  /** Whether two resolved gradient points are too close for a renderer to span a gradient between
    * them. The backends hold coordinates as single-precision floats, so points that differ only
    * beyond a float's precision are one point to them. Java2D throws on a gradient of no length, a
    * browser's canvas paints nothing, and another browser's canvas paints the last stop. Every
    * backend asks this and paints the gradient's [[smile.colors.LinearGradient.averageColor]]
    * instead, which is what a gradient that short amounts to.
    */
  private[smile] def isTooShortToPaint(start: Pos, end: Pos): Boolean =
    start.x.toFloat == end.x.toFloat && start.y.toFloat == end.y.toFloat

  /** The most cycles `smile.colors.LinearGradient.writtenOutOver` writes out. A gradient that
    * repeats more often than this across one canvas is finer than its pixels almost anywhere a
    * canvas can be shown.
    */
  private[smile] val MaximumWrittenOutCycles = 4096

  private def isFinite(point: Pos): Boolean =
    !point.x.isNaN && !point.x.isInfinite && !point.y.isNaN && !point.y.isInfinite

/** Represents a linear gradient paint with two or more colors.
  *
  * ```scala
  * import smile.colors.PresetColor.*
  * import smile.modeling.*
  * import smile.pictures.*
  *
  * val stops  = Seq(0.0 -> MidnightBlue, 0.55 -> Tomato, 1.0 -> Gold)
  * val sunset = Rectangle(160, 100, fillWith(LinearGradient(Pos(0, 0), Pos(0, 100), stops)), None)
  * val sea = Rectangle(160, 40, fillWith(LinearGradient(Pos(0, 0), Pos(160, 0), Teal, Navy)), None)
  * ```
  *
  * ![A sky shading from midnight blue through red to gold at the horizon](pictures/sunset.png) ![A
  * stripe shading from teal on the left to navy on the right](pictures/sea.png)
  *
  * ## Coordinate contract
  *
  * `start` and `end` are **element-local** coordinates whose origin is the top-left corner of the
  * shape's own, untransformed box (a text's layout box), widened by a stroke's radius. For a shape
  * that is not turned or scaled, `Pos(0, 0)` is the top-left corner of its bounds and
  * `Pos(bounds.width, bounds.height)` is the opposite one, so
  *
  * ```
  * val gradient = LinearGradient(Pos(0, 0), Pos(width, 0), red, blue)
  * ```
  *
  * always paints a horizontal red-to-blue gradient across the whole shape, no matter where the
  * shape sits in the picture. They are not world coordinates. The same gradient on two shapes
  * paints each of them from its own corner. Every renderer (Java2D on the JVM, canvas and SVG on
  * Scala.js) resolves the points this way.
  *
  * The frame travels with the element. Coordinates outside the box are legal and place the
  * gradient's ends outside the shape, where `cycleMethod` takes over. A gradient lives in its
  * shape's own frame, and every renderer paints it under the shape's affine. So a moved, rotated,
  * scaled or mirrored element moves, turns, stretches or mirrors its gradient rigidly with it, and
  * the gradient's points never change.
  *
  * ## What a gradient has to be
  *
  * A gradient that can be constructed can be painted, on every backend. Its two points are finite
  * and differ, since a gradient without a length has no direction to run in. There are at least two
  * stops, and their fractions lie within zero to one and do not run backwards. Two stops that share
  * a fraction make a hard edge from one color to the next.
  *
  * A gradient so short that a renderer's floats cannot tell its two points apart is painted with
  * its [[smile.colors.LinearGradient.averageColor]].
  *
  * @param start
  *   The starting position of the gradient, relative to the top-left corner of the shape's own,
  *   untransformed box.
  * @param end
  *   The ending position of the gradient, relative to the top-left corner of the shape's own,
  *   untransformed box.
  * @param stops
  *   The colors of the gradient, each at its fraction of the way from `start` to `end`.
  * @param cycleMethod
  *   The method used to repeat the gradient pattern if the shape to be filled is larger than the
  *   gradient defined by the start and end points.
  */
final case class LinearGradient(
    start: Pos,
    end: Pos,
    stops: Seq[(Double, Color)],
    cycleMethod: LinearGradient.CycleMethod = LinearGradient.CycleMethod.NoCycle
) extends Paint derives CanEqual:
  /** The fractions of this gradient's stops, in order. */
  private[smile] def fractions: Seq[Double] = stops.map((fraction, _) => fraction)

  /** The colors of this gradient's stops, in order. */
  private[smile] def colors: Seq[Color] = stops.map((_, color) => color)

  require(stops.size >= 2, "A linear gradient requires at least two color stops")
  require(
    fractions.forall(fraction => fraction >= 0.0 && fraction <= 1.0),
    s"Gradient fractions must be within [0.0, 1.0] (were: ${fractions.mkString(", ")})"
  )
  require(
    fractions.zip(fractions.drop(1)).forall((fraction, next) => fraction <= next),
    s"Gradient fractions must be in increasing order (were: ${fractions.mkString(", ")})"
  )
  require(
    LinearGradient.isFinite(start) && LinearGradient.isFinite(end),
    s"A linear gradient's points must be finite numbers (were: $start and $end)"
  )
  require(
    start != end,
    s"A linear gradient needs two different points to run between (both were: $start)"
  )

  /** Resolves this gradient's element-local [[smile.colors.LinearGradient.start]] and
    * [[smile.colors.LinearGradient.end]] into the coordinate space a renderer is currently drawing
    * in.
    *
    * @param elementOrigin
    *   The top-left corner of the element's boundary, expressed in the space the element's own
    *   geometry is drawn in. That space may already be translated, rotated or scaled.
    */
  private[smile] def anchoredAt(elementOrigin: Pos): (Pos, Pos) =
    (elementOrigin + start, elementOrigin + end)

  /** The colors of this gradient's stops, as packed ARGB colors. */
  override private[smile] def paints: Set[Int] = colors.map(_.toARGBInt).toSet

  /** This gradient with the color of each stop replaced through `table`, which maps packed ARGB
    * colors. Its points and fractions stay as they are.
    */
  override private[smile] def mapPaints(table: Int => Int): LinearGradient =
    copy(stops = stops.map((fraction, color) => (fraction, color.mapPaints(table))))

  /** This gradient for a backend that knows no cycle methods and continues the end colors beyond
    * the gradient's two points, as an HTML canvas does. The result is an equivalent gradient whose
    * points lie beyond the area to be painted, with every cycle that falls within the area written
    * out as stops of its own. Every other cycle is mirrored for
    * [[smile.colors.LinearGradient.CycleMethod.Reflect]].
    *
    * @param start
    *   The gradient's resolved start point, in the space the backend is painting in.
    * @param end
    *   Its resolved end point there.
    * @param area
    *   Points, in the same space, whose hull contains everything that may be painted, such as the
    *   corners of the render target.
    * @return
    *   The points and the stops, as offsets between them, to build the gradient from. `None` when
    *   the area spans more than `smile.colors.LinearGradient.MaximumWrittenOutCycles` cycles, in
    *   which case the gradient is best painted as its [[smile.colors.LinearGradient.averageColor]].
    */
  private[smile] def writtenOutOver(
      start: Pos,
      end: Pos,
      area: Seq[Pos]
  ): Option[(Pos, Pos, Seq[(Double, Color)])] =
    if cycleMethod == LinearGradient.CycleMethod.NoCycle || area.isEmpty then
      Some((start, end, stops))
    else
      // How many gradient lengths along the gradient's direction each point lies from the start
      // point.
      val direction     = end - start
      val lengthSquared = direction.x * direction.x + direction.y * direction.y
      val distances     = area.map: point =>
        ((point.x - start.x) * direction.x + (point.y - start.y) * direction.y) / lengthSquared

      val firstCycle = distances.min.floor.min(0.0)
      val lastCycle  = distances.max.ceil.max(1.0)
      val cycles     = lastCycle - firstCycle

      Option.when(cycles <= LinearGradient.MaximumWrittenOutCycles):
        val writtenOut =
          for
            index <- 0 until cycles.toInt
            isMirrored = cycleMethod == LinearGradient.CycleMethod.Reflect &&
              math.floorMod(firstCycle.toLong + index, 2L) == 1L
            (fraction, color) <-
              if isMirrored then stops.reverse.map((fraction, color) => (1.0 - fraction, color))
              else stops
          yield ((index + fraction) / cycles, color)

        (start + direction * firstCycle, start + direction * lastCycle, writtenOut)
  end writtenOutOver

  /** The color this gradient amounts to over its length. Each color is weighted by how much of the
    * way from `start` to `end` it covers, and by its opacity. It matches what the renderers draw,
    * which interpolate the stops' colors and opacity separately, unpremultiplied, so a transparent
    * stop's color still shows on the way to it. Red to a transparent white averages to a
    * translucent pink. Before the first stop and after the last, their colors hold. A gradient with
    * no opacity anywhere averages to a transparent black.
    */
  lazy val averageColor: Color =
    val padded   = ((0.0, colors.head) +: stops) :+ (1.0, colors.last)
    val segments = padded.zip(padded.drop(1))

    val opacity = segments.map { case ((from, fromColor), (to, toColor)) =>
      (to - from) * (fromColor.opacity + toColor.opacity) / 2.0
    }.sum

    // The integral over [0, 1] of the given channel times the opacity, both of which run linearly
    // between two stops.
    def premultiplied(channel: Color => Int): Double =
      segments.map { case ((from, fromColor), (to, toColor)) =>
        val (a0, c0, a1, c1) =
          (fromColor.opacity, channel(fromColor), toColor.opacity, channel(toColor))
        (to - from) * (2.0 * a0 * c0 + a0 * c1 + a1 * c0 + 2.0 * a1 * c1) / 6
      }.sum

    if opacity == 0 then Color(0, 0, 0, 0)
    else
      def unpremultiplied(channel: Color => Int): Int =
        math.round(premultiplied(channel) / opacity).toInt
      Color(
        unpremultiplied(_.red),
        unpremultiplied(_.green),
        unpremultiplied(_.blue),
        math.round(opacity).toInt
      )
  end averageColor
