package smile.pictures

import smile.colors.Color
import smile.modeling.{Affine, Bounds, Pos}
import smile.modeling.{EllipseArcShape, LocalShape, PathShape, PointShape, TextLayout, TextShape}
import smile.pictures.{FillStyle, StrokeStyle}

/** Polygons and polylines through any points.
  *
  * ```scala
  * import smile.colors.PresetColor.*
  * import smile.modeling.*
  *
  * val points =
  *   Seq(Pos(0, 20), Pos(60, 20), Pos(60, 0), Pos(100, 30), Pos(60, 60), Pos(60, 40), Pos(0, 40))
  * val arrow = Polygon(Pos.Origin, points, fillWith(Navy), None)
  * ```
  *
  * ![A navy arrow pointing right](pictures/arrow.png)
  */
object Polygon:

  /** The polygon through `points`, each relative to `pos`, closed back to the first point when
    * there are more than two of them, and otherwise the line from one to the other. Its anchor is
    * the middle of its points' box, wherever `pos` is. No points at all give the empty picture.
    *
    * @throws IllegalArgumentException
    *   if a coordinate is not finite
    */
  def apply(
      pos: Pos,
      points: Seq[Pos],
      fill: Option[FillStyle],
      line: Option[StrokeStyle]
  ): Element =
    points.foreach(Element.requireFinite(_, "A point"))
    Element.requireFinite(pos, "A position")
    if points.isEmpty then Empty
    else
      val (xs, ys) = (points.map(_.x), points.map(_.y))
      val middle   = Pos((xs.min + xs.max) / 2, (ys.min + ys.max) / 2)
      Shapes.polygon(points, pos + middle, fill, line)
end Polygon

/** Rectangles and squares.
  *
  * ```scala
  * import smile.colors.PresetColor.*
  *
  * val rectangle = Rectangle(120, 60, fillWith(CornflowerBlue), None)
  * ```
  *
  * ![A blue rectangle](pictures/rectangle.png)
  */
object Rectangle:

  /** A square with sides of `sideLength`, centered at the origin.
    *
    * @throws IllegalArgumentException
    *   if the side length is negative or not a number
    */
  def apply(sideLength: Double, fill: Option[FillStyle], line: Option[StrokeStyle]): Shape =
    apply(sideLength, Pos.Origin, fill, line)

  /** A square with sides of `sideLength`, centered at `center`.
    *
    * @throws IllegalArgumentException
    *   if the side length is negative or not a number
    */
  def apply(
      sideLength: Double,
      center: Pos,
      fill: Option[FillStyle],
      line: Option[StrokeStyle]
  ): Shape =
    Shapes.requireNonNegative(sideLength, "square's side length")
    apply(sideLength, sideLength, center, fill, line)

  /** A rectangle `baseLength` wide and `height` high, centered at the origin.
    *
    * @throws IllegalArgumentException
    *   if a length is negative or not a number
    */
  def apply(
      baseLength: Double,
      height: Double,
      fill: Option[FillStyle],
      line: Option[StrokeStyle]
  ): Shape =
    apply(baseLength, height, Pos.Origin, fill, line)

  /** A rectangle `baseLength` wide and `height` high, centered at `center`.
    *
    * @throws IllegalArgumentException
    *   if a length is negative or not a number
    */
  def apply(
      baseLength: Double,
      height: Double,
      center: Pos,
      fill: Option[FillStyle],
      line: Option[StrokeStyle]
  ): Shape =
    Shapes.requireNonNegative(baseLength, "rectangle's base length")
    Shapes.requireNonNegative(height, "rectangle's height")
    val (halfWidth, halfHeight) = (baseLength / 2.0, height / 2.0)
    Shapes.polygon(
      Seq(
        Pos(-halfWidth, -halfHeight),
        Pos(halfWidth, -halfHeight),
        Pos(halfWidth, halfHeight),
        Pos(-halfWidth, halfHeight)
      ),
      center,
      fill,
      line
    )
end Rectangle

/** Equilateral, isosceles and scalene triangles, with the apex up.
  *
  * Every side has to have a length, and the sides have to satisfy the triangle inequality. A
  * factory given sides that cannot make a triangle refuses with an `IllegalArgumentException`.
  *
  * ```scala
  * import smile.colors.PresetColor.*
  *
  * val triangle = Triangle(80, fillWith(SeaGreen), None)
  * ```
  *
  * ![A green triangle with its apex up](pictures/triangle.png)
  */
object Triangle:

  /** The height of an equilateral triangle as a multiple of its side. */
  val HeightOfEquilateralTriangleAsFactorOfSide: Double = math.sqrt(3) / 2.0

  /** The side of an equilateral triangle as a multiple of its height. */
  val SideOfEquilateralTriangleAsFactorOfHeight: Double =
    1.0 / HeightOfEquilateralTriangleAsFactorOfSide

  /** An equilateral triangle with sides of `sideLength`, centered at the origin. */
  def apply(sideLength: Double, fill: Option[FillStyle], line: Option[StrokeStyle]): Shape =
    apply(sideLength, Pos.Origin, fill, line)

  /** An equilateral triangle with sides of `sideLength`, its box centered at `center`. */
  def apply(
      sideLength: Double,
      center: Pos,
      fill: Option[FillStyle],
      line: Option[StrokeStyle]
  ): Shape =
    validateSide(sideLength, "side")
    isosceles(
      HeightOfEquilateralTriangleAsFactorOfSide * sideLength / 2.0,
      sideLength / 2.0,
      center,
      fill,
      line
    )

  /** An equilateral triangle `height` high, centered at the origin. */
  def basedOnHeight(height: Double, fill: Option[FillStyle], line: Option[StrokeStyle]): Shape =
    basedOnHeight(height, Pos.Origin, fill, line)

  /** An equilateral triangle `height` high, its box centered at `center`. */
  def basedOnHeight(
      height: Double,
      center: Pos,
      fill: Option[FillStyle],
      line: Option[StrokeStyle]
  ): Shape =
    // An equilateral triangle's sides follow from its height, so a zero height leaves no sides.
    Shapes.requirePositive(height, "height of triangle")
    isosceles(
      height / 2.0,
      SideOfEquilateralTriangleAsFactorOfHeight * height / 2.0,
      center,
      fill,
      line
    )

  /** An isosceles triangle with two sides of `sideLength` over a base of `baseLength`, centered at
    * the origin.
    */
  def apply(
      sideLength: Double,
      baseLength: Double,
      fill: Option[FillStyle],
      line: Option[StrokeStyle]
  ): Shape =
    apply(sideLength, baseLength, Pos.Origin, fill, line)

  /** An isosceles triangle with two sides of `sideLength` over a base of `baseLength`, its box
    * centered at `center`.
    */
  def apply(
      sideLength: Double,
      baseLength: Double,
      center: Pos,
      fill: Option[FillStyle],
      line: Option[StrokeStyle]
  ): Shape =
    validateSide(baseLength, "base")
    validateSide(sideLength, "side")
    // The two equal sides have to reach each other over the base. Without this check the height
    // below is the square root of a negative number, and the triangle's vertices NaN.
    if baseLength > 2 * sideLength then
      throw new IllegalArgumentException(
        s"Illegal side lengths: a base of $baseLength is too long for two sides of $sideLength " +
          "to meet over it (the triangle inequality does not hold)."
      )
    val halfHeight =
      math.sqrt((sideLength * sideLength - baseLength * baseLength / 4.0).max(0.0)) / 2.0
    isosceles(halfHeight, baseLength / 2.0, center, fill, line)

  /** An isosceles triangle `height` high over a base of `baseLength`, centered at the origin. */
  def basedOnHeightAndBase(
      height: Double,
      baseLength: Double,
      fill: Option[FillStyle],
      line: Option[StrokeStyle]
  ): Shape =
    basedOnHeightAndBase(height, baseLength, Pos.Origin, fill, line)

  /** An isosceles triangle `height` high over a base of `baseLength`, its box centered at `center`.
    */
  def basedOnHeightAndBase(
      height: Double,
      baseLength: Double,
      center: Pos,
      fill: Option[FillStyle],
      line: Option[StrokeStyle]
  ): Shape =
    validateSide(baseLength, "base")
    Shapes.requireNonNegative(height, "height of triangle")
    isosceles(height / 2.0, baseLength / 2.0, center, fill, line)

  /** A triangle with the given base and sides, the left side rising from the base's left end,
    * centered at the origin.
    */
  def apply(
      baseLength: Double,
      leftSideLength: Double,
      rightSideLength: Double,
      fill: Option[FillStyle],
      line: Option[StrokeStyle]
  ): Shape =
    apply(baseLength, leftSideLength, rightSideLength, Pos.Origin, fill, line)

  /** A triangle with the given base and sides, the left side rising from the base's left end, its
    * box centered at `center`.
    */
  def apply(
      baseLength: Double,
      leftSideLength: Double,
      rightSideLength: Double,
      center: Pos,
      fill: Option[FillStyle],
      line: Option[StrokeStyle]
  ): Shape =
    validateSide(baseLength, "base")
    validateSide(leftSideLength, "left side")
    validateSide(rightSideLength, "right side")
    if !(baseLength + leftSideLength >= rightSideLength &&
        baseLength + rightSideLength >= leftSideLength &&
        leftSideLength + rightSideLength >= baseLength)
    then
      throw new IllegalArgumentException(
        s"Illegal side lengths ($baseLength, $leftSideLength, $rightSideLength): " +
          "The triangle inequality does not hold."
      )
    // The angle between the base and the left side, by the law of cosines.
    val cosine =
      (leftSideLength * leftSideLength + baseLength * baseLength -
        rightSideLength * rightSideLength) / (2 * leftSideLength * baseLength)
    // Sides that only just close the triangle can round the cosine a hair past one.
    val leftAngle = math.toDegrees(math.acos(cosine.max(-1.0).min(1.0)))
    // The left side laid along the base and swung clockwise on screen by that angle puts the apex
    // below the base. The corners below flip it to the top.
    val apex = Pos(leftSideLength, 0).rotateAroundOrigin(-leftAngle)
    Shapes.polygon(
      Seq(Pos(apex.x, -apex.y), Pos(0, 0), Pos(baseLength, 0)),
      center,
      fill,
      line
    )
  end apply

  private def isosceles(
      halfHeight: Double,
      halfBase: Double,
      center: Pos,
      fill: Option[FillStyle],
      line: Option[StrokeStyle]
  ): Shape =
    Shapes.polygon(
      Seq(Pos(0, -halfHeight), Pos(halfBase, halfHeight), Pos(-halfBase, halfHeight)),
      center,
      fill,
      line
    )

  private def validateSide(length: Double, name: String): Unit =
    Shapes.requirePositive(length, s"length of triangle's $name")
end Triangle

/** Regular pentagons with a vertex straight up.
  *
  * ```scala
  * import smile.colors.PresetColor.*
  * import smile.modeling.*
  *
  * val pentagon = Pentagon(40, Pos.Origin, fillWith(Teal), None)
  * ```
  *
  * ![A teal pentagon](pictures/pentagon.png)
  */
object Pentagon:

  private val DiagonalPerHeightRatio: Double =
    (1.0 + Math.sqrt(5.0)) / Math.sqrt(5.0 + 2.0 * Math.sqrt(5.0))

  private val DiagonalPerCircumradiusRatio: Double = Math.sqrt((5.0 + Math.sqrt(5.0)) / 2.0)

  /** The largest regular pentagon that fits in a box `width` wide and `height` high, centered at
    * the origin.
    *
    * @throws IllegalArgumentException
    *   if a size is negative or not a number
    */
  def apply(
      width: Double,
      height: Double,
      fill: Option[FillStyle],
      line: Option[StrokeStyle]
  ): Shape =
    apply(width, height, Pos.Origin, fill, line)

  /** The largest regular pentagon that fits in a box `width` wide and `height` high, its box
    * centered at `center`.
    *
    * @throws IllegalArgumentException
    *   if a size is negative or not a number
    */
  def apply(
      width: Double,
      height: Double,
      center: Pos,
      fill: Option[FillStyle],
      line: Option[StrokeStyle]
  ): Shape =
    Shapes.requireNonNegative(width, "pentagon's width")
    Shapes.requireNonNegative(height, "pentagon's height")
    apply(limitCircumradiusTo(width, height), center, fill, line)

  /** A regular pentagon with its vertices `circumradius` from its center of symmetry, its box
    * centered at `center`.
    *
    * @throws IllegalArgumentException
    *   if the circumradius is negative or not a number
    */
  def apply(
      circumradius: Double,
      center: Pos,
      fill: Option[FillStyle],
      line: Option[StrokeStyle]
  ): Shape =
    Shapes.requireNonNegative(circumradius, "length of pentagon's circumradius")
    Shapes.polygon(Shapes.regularPolygonPoints(5, circumradius, 0), center, fill, line)

  /** The circumradius of the largest regular pentagon that fits in a box of the given size. */
  private[pictures] def limitCircumradiusTo(width: Double, height: Double): Double =
    (DiagonalPerHeightRatio * height).min(width) / DiagonalPerCircumradiusRatio
end Pentagon

/** Five-pointed stars with a point straight up.
  *
  * ```scala
  * import smile.colors.PresetColor.*
  * import smile.modeling.*
  *
  * val star = Star5(40, 16, Pos.Origin, fillWith(Gold), lineWith(Black, 2, round = true))
  * ```
  *
  * ![A golden star with a black outline](pictures/star.png)
  */
object Star5:

  /** The largest star that fits in a box `width` wide and `height` high, with its inner corners
    * `cuspRadius` from its center of symmetry, centered at the origin.
    */
  def apply(
      width: Double,
      height: Double,
      cuspRadius: Double,
      fill: Option[FillStyle],
      line: Option[StrokeStyle]
  ): Shape =
    apply(width, height, cuspRadius, Pos.Origin, fill, line)

  /** The largest star that fits in a box `width` wide and `height` high, with its inner corners
    * `cuspRadius` from its center of symmetry, its box centered at `center`.
    *
    * @throws IllegalArgumentException
    *   if a size is negative or not a number
    */
  def apply(
      width: Double,
      height: Double,
      cuspRadius: Double,
      center: Pos,
      fill: Option[FillStyle],
      line: Option[StrokeStyle]
  ): Shape =
    Shapes.requireNonNegative(width, "star pentagon's width")
    Shapes.requireNonNegative(height, "star pentagon's height")
    apply(Pentagon.limitCircumradiusTo(width, height), cuspRadius, center, fill, line)

  /** A star with its points `circumradius` and its inner corners `cuspRadius` from its center of
    * symmetry, its box centered at `center`.
    *
    * @throws IllegalArgumentException
    *   if a radius is negative or not a number
    */
  def apply(
      circumradius: Double,
      cuspRadius: Double,
      center: Pos,
      fill: Option[FillStyle],
      line: Option[StrokeStyle]
  ): Shape =
    Shapes.requireNonNegative(circumradius, "length of star pentagon's circumradius")
    Shapes.requireNonNegative(cuspRadius, "length of star pentagon's cusp radius")
    val outer = Shapes.regularPolygonPoints(5, circumradius, 0)
    // Half a symmetry angle to the left of the outer points, the way the points are listed, so
    // each inner corner follows the outer one it comes after.
    val inner = Shapes.regularPolygonPoints(5, cuspRadius, 36)
    Shapes.polygon(outer.zip(inner).flatMap((o, i) => Seq(o, i)), center, fill, line)
end Star5

/** Straight lines.
  *
  * ```scala
  * import smile.colors.PresetColor.*
  * import smile.modeling.*
  *
  * val line = Line(Pos(0, 40), Pos(120, 0), lineWith(Crimson, 8, round = true).get)
  * ```
  *
  * ![A thick diagonal line with round ends](pictures/line.png)
  */
object Line:

  /** A line from `(startX, startY)` to `(endX, endY)`. */
  def apply(
      startX: Double,
      startY: Double,
      endX: Double,
      endY: Double,
      stroke: StrokeStyle
  ): Shape =
    apply(Pos(startX, startY), Pos(endX, endY), stroke)

  /** A line from `start` to `end`, its anchor halfway between them. */
  def apply(start: Pos, end: Pos, stroke: StrokeStyle): Shape =
    val center = Pos((start.x + end.x) / 2.0, (start.y + end.y) / 2.0)
    Shapes.polygon(Seq(start - center, end - center), center, None, Some(stroke))
end Line

/** Circles.
  *
  * ```scala
  * import smile.colors.PresetColor.*
  * import smile.modeling.*
  * import smile.modeling.Pos
  * import smile.pictures.*
  *
  * val circle = Circle(Pos.Origin, 40, fillWith(Orange), lineWith(Black, 4, round = true))
  * ```
  *
  * ![An orange circle with a black outline](pictures/circle.png)
  */
object Circle:

  /** A circle of `radius` centered at `center`.
    *
    * @throws IllegalArgumentException
    *   if the radius is negative or not a number
    */
  def apply(
      center: Pos,
      radius: Double,
      fill: Option[FillStyle],
      line: Option[StrokeStyle]
  ): Shape =
    Shapes.requireNonNegative(radius, "circle's radius")
    Shapes.placed(EllipseArcShape(radius, radius, 0, 360), center, fill, line)
end Circle

/** Ellipses with their axes along the x and y axes.
  *
  * ```scala
  * import smile.colors.PresetColor.*
  * import smile.modeling.*
  *
  * val ellipse = Ellipse(Pos.Origin, 60, 30, fillWith(Orange), None)
  * ```
  *
  * ![An orange ellipse](pictures/ellipse.png)
  */
object Ellipse:

  /** An ellipse centered at `center` with the half-axis `semiMajorAxis` along x and `semiMinorAxis`
    * along y.
    *
    * @throws IllegalArgumentException
    *   if an axis is negative or not a number
    */
  def apply(
      center: Pos,
      semiMajorAxis: Double,
      semiMinorAxis: Double,
      fill: Option[FillStyle],
      line: Option[StrokeStyle]
  ): Shape =
    Shapes.requireNonNegative(semiMajorAxis, "ellipse's semi-major axis")
    Shapes.requireNonNegative(semiMinorAxis, "ellipse's semi-minor axis")
    Shapes.placed(EllipseArcShape(semiMajorAxis, semiMinorAxis, 0, 360), center, fill, line)
end Ellipse

/** Arcs of ellipses, and whole ellipses.
  *
  * ```scala
  * import smile.colors.PresetColor.*
  * import smile.modeling.*
  *
  * val arch = Arc(Pos.Origin, 120, 120, 0, 180, None, lineWith(Crimson, 8, round = true))
  * val bowl = Arc(Pos.Origin, 120, 80, 180, 180, fillWith(Gold), lineWith(Black, 2, round = true))
  * ```
  *
  * ![A red arch, the upper half of a circle drawn as a thick line](pictures/arch.png) ![A golden
  * bowl, the lower half of an ellipse filled up to its chord](pictures/bowl.png)
  */
object Arc:

  /** The part of the ellipse `width` wide and `height` high centered at `pos` that sweeps from
    * `startAngle` by `arcAngle` degrees, counterclockwise on screen from three o'clock for a
    * positive extent. An extent of 360 or more is the whole ellipse. A partial sweep is filled as
    * the segment its chord cuts off.
    *
    * The arc's anchor is the middle of the arc's own box. For a partial arc that differs from
    * `pos`, the middle of the whole ellipse's box. To turn an arc about the ellipse's center, use
    * `rotateAround(angle, pos)`.
    *
    * @throws IllegalArgumentException
    *   if a size is negative or not a number, or an angle is not finite
    */
  def apply(
      pos: Pos = Pos.Origin,
      width: Double,
      height: Double,
      startAngle: Double = 0.0,
      arcAngle: Double = 360.0,
      fill: Option[FillStyle],
      line: Option[StrokeStyle]
  ): Shape =
    Shapes.requireNonNegative(width, "arc's width")
    Shapes.requireNonNegative(height, "arc's height")
    Shapes.placed(EllipseArcShape(width / 2, height / 2, startAngle, arcAngle), pos, fill, line)
end Arc

/** Lines of text.
  *
  * ```scala
  * import smile.colors.PresetColor.*
  * import smile.modeling.*
  *
  * val title = Text(Pos.Origin, None, "SMILe", "sans-serif", 48, 700, fillWith(Navy), None)
  * ```
  *
  * ![The word SMILe in bold navy letters](pictures/title.png)
  *
  * A text is an element like any other, and every transform of [[smile.pictures.Element]] applies
  * to its letters.
  */
object Text:

  /** `content` in `font` at `size` pixels and the CSS-style `weight`, its layout box centered at
    * `pos`.
    *
    * With `customBounds`, the text is scaled to the size of that box and moved so that the middle
    * of its bounds is the middle of the box. Last, it is turned by `rotationAngle` around its
    * anchor.
    *
    * @throws IllegalArgumentException
    *   if the size is negative or not finite
    */
  def apply(
      pos: Pos,
      customBounds: Option[Bounds],
      content: String,
      font: String,
      size: Double,
      weight: Int,
      fill: Option[FillStyle],
      line: Option[StrokeStyle],
      rotationAngle: Double = 0.0
  ): Element =
    val natural = Shapes.placed(TextShape(TextLayout(content, font, size, weight)), pos, fill, line)
    val sized   = customBounds match
      case None      => natural
      case Some(box) =>
        val scaled = natural.scaleTo(box.width, box.height)
        scaled.bounds match
          case Some(now) => scaled.moveBy(box.center.x - now.center.x, box.center.y - now.center.y)
          case None      => scaled
    sized.rotateBy(rotationAngle)
  end apply
end Text

/** Single points. */
object Point:

  /** A point of `color` at `pos`. It is a 1x1 square centered there, which scales and turns like
    * any other shape.
    */
  def apply(pos: Pos, color: Color): Shape =
    Shapes.placed(PointShape(), pos, Some(FillStyle(color)), None)
end Point

/** Validation and placing of local geometry, shared by the shape factories. */
private object Shapes:

  /** `shape` moved by `at`, with its anchor at its local box's center. */
  def placed(
      shape: LocalShape,
      at: Pos,
      fill: Option[FillStyle],
      line: Option[StrokeStyle]
  ): Shape =
    Element.requireFinite(at, "A position")
    new Shape(
      shape,
      Element.normalizedFill(fill),
      Element.normalizedStroke(line),
      Affine.translation(at.x, at.y),
      shape.center
    )

  /** A polygon through `points`, or a polyline through two, recentered on its local box's center
    * and placed with that center at `at`.
    */
  def polygon(
      points: Seq[Pos],
      at: Pos,
      fill: Option[FillStyle],
      line: Option[StrokeStyle]
  ): Shape =
    val xy     = IArray.from(points.flatMap(p => Seq(p.x, p.y)))
    val raw    = PathShape(xy, closed = points.sizeIs > 2)
    val middle = raw.localBox.center
    val shape  =
      if middle.x == 0 && middle.y == 0 then raw
      else
        PathShape(
          IArray.tabulate(xy.length)(i => xy(i) - (if i % 2 == 0 then middle.x else middle.y)),
          raw.closed
        )
    placed(shape, at, fill, line)

  /** The vertices of a regular polygon around the origin, the first one straight up and turned
    * counterclockwise on screen by `startAngle` degrees, the rest following counterclockwise.
    */
  def regularPolygonPoints(sides: Int, circumradius: Double, startAngle: Double): Seq[Pos] =
    val first         = Pos(0, -circumradius).rotateAroundOrigin(startAngle)
    val symmetryAngle = 360.0 / sides
    first +: (1 until sides).map(n => first.rotateAroundOrigin(n * symmetryAngle))

  /** Refuses a size that is negative, infinite or not a number, naming it by `description`. */
  def requireNonNegative(value: Double, description: String): Unit =
    if value < 0 then
      throw new IllegalArgumentException(
        s"${description.capitalize} cannot be negative (was: $value)."
      )
    if value.isNaN then
      throw new IllegalArgumentException(s"${description.capitalize} must be a number (was: NaN).")
    if value.isInfinite then
      throw new IllegalArgumentException(
        s"${description.capitalize} must be finite (was: $value)."
      )

  /** Refuses a length that is zero, negative or not a number, naming it by `description`. */
  def requirePositive(value: Double, description: String): Unit =
    requireNonNegative(value, description)
    if value == 0 then
      throw new IllegalArgumentException(s"${description.capitalize} cannot be zero (was: $value).")
end Shapes
