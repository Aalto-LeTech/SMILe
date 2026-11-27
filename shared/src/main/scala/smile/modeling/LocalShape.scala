package smile.modeling

import smile.modeling.{Angles, Bounds, Pos}
import smile.pictures.{StrokeOutline, StrokeStyle}

import scala.util.hashing.MurmurHash3

/** The geometry of a leaf element in its own local frame, before the element's affine places it.
  *
  * A shape is described by its support function. `smile.modeling.LocalShape.support` tells how far
  * the filled geometry reaches along a direction. That is all `smile.modeling.Support.worldBounds`
  * needs to find the tight box around the shape under any affine, shears and mirrors included.
  *
  * A shape is shared by every transformed copy of its element, so its local box and its stroke
  * outline are computed once and kept on the shape.
  */
private[smile] sealed trait LocalShape derives CanEqual:

  /** The support function of the filled geometry, the largest `vx * x + vy * y` over its points.
    * The direction need not be a unit vector, and the result scales with it.
    */
  def support(vx: Double, vy: Double): Double

  /** The box around the filled geometry in the local frame, strokes not included. */
  def localBox: Bounds

  /** The center of `smile.modeling.LocalShape.localBox`, where an element's anchor starts.
    */
  def center: Pos = localBox.center

  /** The support function of what `stroke` paints together with the fill, the largest
    * `vx * x + vy * y` over the ink of both. The stroke follows the local geometry and is scaled
    * with it, as every backend draws it.
    */
  private[smile] def strokedSupport(vx: Double, vy: Double, stroke: StrokeStyle): Double

  /** The box around the support function `h`, from its values along the four axis directions. */
  protected final def boxOf(h: (Double, Double) => Double): Bounds =
    Bounds(-h(-1, 0), -h(0, -1), h(1, 0), h(0, 1))
end LocalShape

/** A polygon (`closed`) or a polyline, as its vertices interleaved in `xy`: x0, y0, x1, y1 and so
  * on. The fill of an open path closes it with a straight edge, as every backend fills it.
  *
  * A stroke follows the path with the joins of its style at the corners and, on an open path, its
  * caps at the two ends. Consecutive duplicate vertices span no edge and are skipped. A path whose
  * vertices all coincide has no direction for a stroke to follow, and its stroke is taken to reach
  * a square of the stroke's radius around the point.
  */
private[smile] final class PathShape(val xy: IArray[Double], val closed: Boolean)
    extends LocalShape:
  require(
    xy.length >= 2 && xy.length % 2 == 0,
    s"A path needs whole vertices and at least one of them, not ${xy.length} coordinates"
  )
  require(xy.forall(_.isFinite), "A path's coordinates must be finite")

  def vertexCount: Int = xy.length / 2

  def support(vx: Double, vy: Double): Double = PathShape.maxDot(xy, vx, vy)

  lazy val localBox: Bounds = boxOf(support)

  private[smile] def strokedSupport(vx: Double, vy: Double, stroke: StrokeStyle): Double =
    val radius = stroke.width / 2
    if !(radius > 0) then support(vx, vy)
    else
      val outline = outlineFor(radius, stroke)
      val discs   =
        if outline.discs.isEmpty then Double.NegativeInfinity
        else PathShape.maxDot(outline.discs, vx, vy) + radius * Support.length(vx, vy)
      support(vx, vy).max(PathShape.maxDot(outline.points, vx, vy)).max(discs)

  // The last stroke outline built. An element is nearly always drawn with one stroke, so one entry
  // is enough. The outline is immutable, so a thread that reads a stale entry only rebuilds it.
  @volatile private var lastOutline: PathShape.Outline | Null = null

  private def outlineFor(radius: Double, stroke: StrokeStyle): PathShape.Outline =
    val cached = lastOutline
    if cached != null && cached.matches(radius, stroke) then cached
    else
      val built = PathShape.Outline.of(this, radius, stroke)
      lastOutline = built
      built

  override def equals(other: Any): Boolean = other match
    case that: PathShape => closed == that.closed && xy.sameElements(that.xy)
    case _               => false

  // Equal coordinates must hash equally, and 0.0 == -0.0, so the zeros are made one.
  override def hashCode: Int =
    MurmurHash3.mix(MurmurHash3.orderedHash(xy.map(_ + 0.0)), closed.hashCode)

  override def toString: String =
    s"PathShape(${xy.mkString("[", ", ", "]")}, closed = $closed)"
end PathShape

private[smile] object PathShape:

  /** The largest `vx * x + vy * y` over the interleaved points `xy`. */
  private def maxDot(xy: IArray[Double], vx: Double, vy: Double): Double =
    var best = Double.NegativeInfinity
    var i    = 0
    while i < xy.length do
      val dot = vx * xy(i) + vy * xy(i + 1)
      if dot > best then best = dot
      i += 2
    best

  /** What a stroke of `radius` adds to a path, in the path's local frame. The hull of `points` is
    * the stroke's polygonal ink, made of the sides at each vertex, miter tips and square cap
    * corners that `StrokeOutline` gives. `discs` holds the centers of the discs of the stroke's
    * radius that round joins and caps add. The support of the stroke is the larger of the two
    * supports, and is exact for every join and cap.
    */
  private final class Outline(
      val radius: Double,
      val cap: StrokeStyle.Cap,
      val join: StrokeStyle.Join,
      val points: IArray[Double],
      val discs: IArray[Double]
  ):
    def matches(otherRadius: Double, stroke: StrokeStyle): Boolean =
      radius == otherRadius && cap == stroke.cap && join == stroke.join

  private object Outline:
    def of(path: PathShape, radius: Double, stroke: StrokeStyle): Outline =
      val vertices = Vector.tabulate(path.vertexCount)(i => Pos(path.xy(2 * i), path.xy(2 * i + 1)))
      val deduplicated = vertices.foldLeft(Vector.empty[Pos]): (kept, vertex) =>
        if kept.lastOption.contains(vertex) then kept else kept :+ vertex
      val corners =
        if path.closed && deduplicated.sizeIs > 1 && deduplicated.head == deduplicated.last then
          deduplicated.init
        else deduplicated

      val points = Vector.newBuilder[Pos]
      val discs  = Vector.newBuilder[Pos]

      def directionBetween(from: Pos, to: Pos): Pos =
        StrokeOutline
          .direction(to - from)
          .getOrElse(throw IllegalStateException(s"No direction from $from to $to"))

      def addJoin(corner: Pos, incoming: Pos, outgoing: Pos): Unit =
        points ++= StrokeOutline.sides(corner, incoming, radius)
        points ++= StrokeOutline.sides(corner, outgoing, radius)
        stroke.join match
          case StrokeStyle.Join.Round => discs += corner
          case other => points ++= StrokeOutline.join(corner, incoming, outgoing, radius, other)

      def addCap(end: Pos, outward: Pos): Unit =
        points ++= StrokeOutline.sides(end, outward, radius)
        stroke.cap match
          case StrokeStyle.Cap.Round => discs += end
          case other                 => points ++= StrokeOutline.cap(end, outward, radius, other)

      if corners.sizeIs == 1 then
        val lone = corners.head
        for dx <- Seq(-radius, radius); dy <- Seq(-radius, radius) do points += lone + Pos(dx, dy)
      else if path.closed then
        val count = corners.size
        for index <- corners.indices do
          val corner = corners(index)
          addJoin(
            corner,
            directionBetween(corners((index + count - 1) % count), corner),
            directionBetween(corner, corners((index + 1) % count))
          )
      else
        val last = corners.size - 1
        addCap(corners.head, directionBetween(corners(1), corners.head))
        addCap(corners(last), directionBetween(corners(last - 1), corners(last)))
        for index <- 1 until last do
          val corner = corners(index)
          addJoin(
            corner,
            directionBetween(corners(index - 1), corner),
            directionBetween(corner, corners(index + 1))
          )

      def interleaved(positions: Vector[Pos]): IArray[Double] =
        IArray.from(positions.iterator.flatMap(p => Iterator(p.x, p.y)))

      Outline(
        radius,
        stroke.cap,
        stroke.join,
        interleaved(points.result()),
        interleaved(discs.result())
      )
    end of
  end Outline
end PathShape

/** A full or partial ellipse centered at the local origin, with the half-axes `rx` along x and `ry`
  * along y.
  *
  * Angles are in degrees and run counterclockwise on a y-down screen from three o'clock. The angle
  * `t` is the local point `(rx * cos(t), -ry * sin(t))`. The sweep starts at `startAngle` and
  * extends by `arcAngle`, counterclockwise for a positive extent and clockwise for a negative one.
  * An extent of 360 or more is the whole ellipse.
  *
  * A partial sweep is filled as a chord segment, the curve closed by the straight edge between its
  * ends, and stroked as an open curve with the stroke's caps at its ends, as every backend draws
  * it.
  */
private[smile] final class EllipseArcShape(
    val rx: Double,
    val ry: Double,
    val startAngle: Double,
    val arcAngle: Double
) extends LocalShape:
  require(rx >= 0 && rx.isFinite, s"An ellipse's half-width must be finite and not negative: $rx")
  require(ry >= 0 && ry.isFinite, s"An ellipse's half-height must be finite and not negative: $ry")
  require(startAngle.isFinite && arcAngle.isFinite, "An arc's angles must be finite")

  /** Whether the sweep goes all the way around. */
  def isFullCycle: Boolean = arcAngle.abs >= Angles.FullTurn

  // The sweep from its smaller angle to its larger one.
  private val sweepStart = startAngle.min(startAngle + arcAngle)
  private val sweepEnd   = startAngle.max(startAngle + arcAngle)

  // The sines and cosines of the sweep's ends, exact at right angles.
  private val (startSin, startCos) = Angles.exactSinCos(sweepStart)
  private val (endSin, endCos)     = Angles.exactSinCos(sweepEnd)

  /** The support of the sweep, the largest `A * cos(t) + B * sin(t)` over the sweep's angles `t`,
    * where `A = vx * rx` and `B = -vy * ry`. Over a whole turn that is `hypot(A, B)`, reached at
    * the stationary angle `atan2(B, A)`. A partial sweep reaches it only if that angle lies within
    * the sweep, and otherwise reaches its largest value at one of its ends.
    */
  def support(vx: Double, vy: Double): Double =
    val a = vx * rx
    val b = -vy * ry
    // Factored for a circle, so that a turned circle measures its radius without a rounding error.
    val crest = if rx == ry then rx * Support.length(vx, vy) else math.hypot(a, b)
    if isFullCycle || containsAngle(math.toDegrees(math.atan2(b, a))) then crest
    else (a * startCos + b * startSin).max(a * endCos + b * endSin)

  // A partial sweep is shorter than a full turn, so at most one of the angles equivalent to the
  // given one lies within it. That is the first one at or after the sweep's start.
  private def containsAngle(degrees: Double): Boolean =
    val turns = math.ceil((sweepStart - degrees) / Angles.FullTurn)
    degrees + turns * Angles.FullTurn <= sweepEnd

  lazy val localBox: Bounds = boxOf(support)

  /** A stroke of radius `r` reaches `r * |v|` past the curve along `v`, exactly for a whole ellipse
    * and for round caps. Butt caps end the stroke at the curve's normal, so where the farthest
    * point is an end the bound is conservative by less than `r * |v|`. Square caps carry the stroke
    * a radius past the ends, and their corners are added.
    */
  private[smile] def strokedSupport(vx: Double, vy: Double, stroke: StrokeStyle): Double =
    val radius = stroke.width / 2
    val curve  = support(vx, vy)
    if !(radius > 0) then curve
    else
      val band = curve + radius * Support.length(vx, vy)
      if isFullCycle || stroke.cap != StrokeStyle.Cap.Square then band
      else
        val corners = squareCapCorners(radius)
        var best    = band
        var i       = 0
        while i < corners.length do
          best = best.max(vx * corners(i) + vy * corners(i + 1))
          i += 2
        best

  // The corners of the two square caps of the last radius asked for, interleaved.
  @volatile private var lastCaps: (Double, IArray[Double]) | Null = null

  private def squareCapCorners(radius: Double): IArray[Double] =
    val cached = lastCaps
    if cached != null && cached._1 == radius then cached._2
    else
      val corners = IArray.from(
        capCorners(startSin, startCos, -1, radius) ++ capCorners(endSin, endCos, 1, radius)
      )
      lastCaps = (radius, corners)
      corners

  // The corners of a square cap at the sweep's end at the angle with the given sine and cosine.
  // The sweep's angle grows along (-rx * sin, -ry * cos), and outwardSign points that way past
  // the end. Where the curve has no direction, as at the tip of a flattened ellipse, the cap's
  // square may face any way, and the box around its farthest reach stands in.
  private def capCorners(sin: Double, cos: Double, outwardSign: Double, radius: Double) =
    val end = Pos(rx * cos, -ry * sin)
    StrokeOutline.direction(Pos(-rx * sin * outwardSign, -ry * cos * outwardSign)) match
      case Some(outward) =>
        StrokeOutline
          .cap(end, outward, radius, StrokeStyle.Cap.Square)
          .flatMap(p => Seq(p.x, p.y))
      case None =>
        val reach = radius * math.sqrt(2)
        Seq(end.x - reach, end.y - reach, end.x + reach, end.y + reach)

  override def equals(other: Any): Boolean = other match
    case that: EllipseArcShape =>
      rx == that.rx && ry == that.ry && startAngle == that.startAngle &&
      arcAngle == that.arcAngle
    case _ => false

  override def hashCode: Int = (rx + 0.0, ry + 0.0, startAngle + 0.0, arcAngle + 0.0).hashCode

  override def toString: String = s"EllipseArcShape($rx, $ry, $startAngle, $arcAngle)"
end EllipseArcShape

/** A point, as the 1x1 box centered at the local origin, which an element's affine scales and turns
  * like any other geometry. A point is filled and never stroked, so a stroke adds nothing to it.
  */
private[smile] final class PointShape extends LocalShape:
  def support(vx: Double, vy: Double): Double = (vx.abs + vy.abs) / 2

  val localBox: Bounds = Bounds(-0.5, -0.5, 0.5, 0.5)

  private[smile] def strokedSupport(vx: Double, vy: Double, stroke: StrokeStyle): Double =
    support(vx, vy)

  override def equals(other: Any): Boolean = other.isInstanceOf[PointShape]

  override def hashCode: Int = PointShape.Hash

  override def toString: String = "PointShape"

private[smile] object PointShape:
  private val Hash = "PointShape".hashCode

/** A line of text laid out in its local frame, as `smile.modeling.TextLayout` places it, with the
  * middle of its layout box at the origin.
  *
  * Its geometry is the union of the layout box and the glyphs' visual box, so that italics,
  * overhangs and accents that reach out of the layout box stay inside the bounds. The glyphs'
  * outlines are stroked with round joins (see [[smile.pictures.StrokeStyle]]), which reach exactly
  * one stroke radius from them, so a stroke adds its radius around that union.
  */
private[smile] final class TextShape(val layout: TextLayout) extends LocalShape:
  def support(vx: Double, vy: Double): Double =
    val box = localBox
    (vx * box.upperLeftCorner.x).max(vx * box.lowerRightCorner.x) +
      (vy * box.upperLeftCorner.y).max(vy * box.lowerRightCorner.y)

  lazy val localBox: Bounds =
    val (layoutBox, visualBox) = (layout.layoutBox, layout.visualBox)
    Bounds(
      layoutBox.upperLeftCorner.x.min(visualBox.upperLeftCorner.x),
      layoutBox.upperLeftCorner.y.min(visualBox.upperLeftCorner.y),
      layoutBox.lowerRightCorner.x.max(visualBox.lowerRightCorner.x),
      layoutBox.lowerRightCorner.y.max(visualBox.lowerRightCorner.y)
    )

  private[smile] def strokedSupport(vx: Double, vy: Double, stroke: StrokeStyle): Double =
    val radius = stroke.width / 2
    if !(radius > 0) then support(vx, vy) else support(vx, vy) + radius * Support.length(vx, vy)

  override def equals(other: Any): Boolean = other match
    case that: TextShape => layout == that.layout
    case _               => false

  override def hashCode: Int = layout.hashCode

  override def toString: String = s"TextShape($layout)"
end TextShape
