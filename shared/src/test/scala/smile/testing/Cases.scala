package smile.testing

import smile.colors.{Color, LinearGradient}
import smile.modeling.{Bounds, Pos}
import smile.pictures.*

/** A value with the name it goes by in an assertion's clue. */
final case class Labeled[+A](label: String, value: A)

/** The deterministic case tables behind the law and invariant suites. Both platforms run exactly
  * these cases, in this order, so a failure names its case and can be repeated anywhere.
  *
  * The tables are curated. The library's branches sit at special values (right angles, whole turns,
  * negative and unit factors, an angle a hair off a right one), which a table visits on every run
  * and a random double practically never does. A seeded generator adds a few arbitrary combinations
  * on top (see [[smile.testing.Cases.seededAngles]]).
  *
  * Every fixture is lopsided (scalene, off-center, oblong, with a diagonal gradient) because a
  * symmetric shape looks the same under a mirrored or reversed transformation and so cannot tell a
  * sign error from the right answer.
  */
object Cases:

  val red: Color   = Color(255, 0, 0)
  val green: Color = Color(0, 255, 0)
  val blue: Color  = Color(0, 0, 255)

  /** The seed of every generated case. Assertion clues carry the generated values themselves. */
  val Seed: Long = 20261008L

  /** The angles with a branch of their own somewhere in the library, in both directions and beyond
    * a full turn.
    */
  val rightAngles: Seq[Double] = Seq(0.0, 90.0, 180.0, 270.0, 360.0, -90.0, -180.0, -270.0)

  val obliqueAngles: Seq[Double] = Seq(30.0, 45.0, -37.0, 137.5, 211.5, 450.0, -720.0, 89.999999)

  val angles: Seq[Double] = rightAngles ++ obliqueAngles

  /** A handful of arbitrary angles from the seeded generator, the same on every run. */
  val seededAngles: Seq[Double] =
    val random = new java.util.Random(Seed)
    Seq.fill(6)(math.round((random.nextDouble() * 1440.0 - 720.0) * 1000.0) / 1000.0)

  val pivots: Seq[Pos] = Seq(Pos.Origin, Pos(7, -3), Pos(-20, 15))

  val uniformFactors: Seq[Double] = Seq(2.0, 0.5, 1.0, 3.25)

  val positiveFactorPairs: Seq[(Double, Double)] =
    Seq((2.0, 2.0), (0.5, 0.5), (2.0, 0.5), (1.0, 3.0), (0.25, 1.5))

  /** Factor pairs that mirror: one axis, the other, and both (a half turn). */
  val mirroringFactorPairs: Seq[(Double, Double)] =
    Seq((-1.0, 1.0), (1.0, -1.0), (-1.0, -1.0), (-2.0, 2.0), (2.0, -2.0))

  def stroke(
      width: Double,
      cap: StrokeStyle.Cap = StrokeStyle.Cap.Square,
      join: StrokeStyle.Join = StrokeStyle.Join.Miter
  ): Option[StrokeStyle] = Some(StrokeStyle(blue, width, cap, join))

  /** A short gradient that lies diagonally and off the middle of every fixture below, so that a
    * shifted, mirrored, or unscaled gradient differs from the right one.
    */
  val gradient: LinearGradient = LinearGradient(Pos(4, 2), Pos(19, 9), red, blue)

  private val styles: Seq[Labeled[(Option[FillStyle], Option[StrokeStyle])]] = Seq(
    Labeled("filled", (fillWith(red), None)),
    Labeled("stroked", (fillWith(red), stroke(4))),
    Labeled("gradient-filled", (fillWith(gradient), None))
  )

  private def styled(name: String)(
      shape: (Option[FillStyle], Option[StrokeStyle]) => Element
  ): Seq[Labeled[Element]] =
    styles.map(style => Labeled(s"${style.label} $name", shape(style.value._1, style.value._2)))

  /** A text stretched onto a box, which keeps it measurable where no layout engine exists (jsdom).
    * The box is not centered on the position.
    */
  def boxedText(
      fill: Option[FillStyle] = fillWith(red),
      line: Option[StrokeStyle] = None,
      rotationAngle: Double = 0
  ): Element = Text(
    pos = Pos(6, -2),
    customBounds = Some(Bounds(Pos(-18, -9), Pos(26, 9))),
    content = "Lo",
    font = "SansSerif",
    size = 12,
    weight = 400,
    fill = fill,
    line = line,
    rotationAngle = rotationAngle
  )

  val polygons: Seq[Labeled[Element]] =
    styled("scalene triangle")(Triangle(30, 18, 26, Pos(5, -3), _, _)) ++
      styled("oblong rectangle")(Rectangle(30, 12, Pos(-4, 6), _, _)) ++
      styled("star")(Star5(22, 9, Pos(3, -4), _, _)) ++
      Seq(
        Labeled("slanted line", Line(Pos(-20, -6), Pos(17, 11), StrokeStyle(blue, 5))),
        Labeled(
          "butt-capped line",
          Line(Pos(-14, 9), Pos(12, -3), StrokeStyle(blue, 5, StrokeStyle.Cap.Butt))
        )
      )

  val arcs: Seq[Labeled[Element]] =
    styled("partial arc")(Arc(Pos(5, -3), 40, 20, 20, 110, _, _)) ++
      styled("turned partial arc")(
        Arc(Pos(-2, 4), 40, 20, -50, -200, _, _).rotateAround(25, Pos(-2, 4))
      ) ++
      styled("turned ellipse")(Arc(Pos(3, 2), 36, 14, 0, 360, _, _).rotateAround(-35, Pos(3, 2))) ++
      styled("circle")(Circle(Pos(-5, 3), 11, _, _))

  val texts: Seq[Labeled[Element]] = Seq(
    Labeled("boxed text", boxedText()),
    Labeled("turned boxed text", boxedText(rotationAngle = 25)),
    Labeled("gradient-filled boxed text", boxedText(fill = fillWith(gradient)))
  )

  val vectorGraphics: Seq[Labeled[Element]] = polygons ++ arcs ++ texts

  /** Red on the left half and blue on the right, oblong, away from the origin. */
  def twoColorBitmap: Bitmap =
    Bitmap(8, 4, Seq.fill(4)(Seq.fill(4)(red) ++ Seq.fill(4)(blue)).flatten).moveBy(9, -5)

  def maskGroup: Element = MaskGroup(
    Rectangle(20, 12, Pos(2, 3), fillWith(red), None),
    Triangle(30, 18, 26, Pos(14, -3), fillWith(blue), None)
  )

  /** A crop of a lopsided picture that cuts through its parts, which keeps them as vectors. */
  def clip: Element =
    Picture(polygons.head.value, arcs(1).value).crop(Bounds(-10, -12, 14, 9))

  /** A wrap shift of a lopsided picture by a fraction of its size. */
  def wrap: Element = Picture(polygons.head.value, arcs.head.value).wrapShift(7, -4)

  /** The elements that are no single vector shapes. A bitmap's own rotation resamples it onto whole
    * pixels, so laws that compare pixels allow it a pixel where they hold the others to rounding
    * errors.
    */
  def otherElements: Seq[Labeled[Element]] = Seq(
    Labeled("point", Point(Pos(7, -4), red)),
    Labeled("mask group", maskGroup),
    Labeled("clip", clip),
    Labeled("wrap", wrap),
    Labeled("bitmap", twoColorBitmap)
  )

  def elements: Seq[Labeled[Element]] = vectorGraphics ++ otherElements

  def picture: Element =
    Picture(polygons.head.value, arcs(1).value, texts.head.value, Point(Pos(-25, -12), green))

  def isRaster(element: Element): Boolean = element match
    case _: Bitmap => true
    case _         => false
