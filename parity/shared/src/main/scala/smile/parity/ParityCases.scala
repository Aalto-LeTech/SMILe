package smile.parity

import smile.testing.*

import smile.colors.{Color, LinearGradient, PresetColor}
import smile.colors.LinearGradient.CycleMethod
import smile.modeling.{Affine, Bounds, Pos}
import smile.testing.Expectation.*
import smile.pictures.*
import smile.pictures.StrokeStyle.{Cap, Join}

/** The catalog of pictures every backend renders. One definition serves all of them. The shared
  * suite checks each picture's probes on the platform it runs on, the Scala.js suite compares
  * canvas with SVG, and the three-way tier on the JVM renders the same catalog with AWT and,
  * through a Scala.js bundle of it, in a browser.
  *
  * Each picture is small and shows one feature, chosen for a kind of drift between backends it
  * would expose. `catches` names that drift. Shapes are lopsided on purpose, because a symmetric
  * one looks the same mirrored or turned the other way. Angles follow the library's convention.
  * Positive is counterclockwise on screen, measured from three o'clock, so 90 degrees points up.
  * The cases were drawn when positive meant clockwise, and their angles were negated to keep the
  * same pictures and probes.
  *
  * Every case is rendered at each of [[smile.parity.ParityCase.Scales]], so a probe has to hold at
  * 1x, 2x and 1.5x unless it names its scales, and the cases in `densities` at the end are there
  * for what the device grid decides.
  */
object ParityCases:

  /** Fonts a text case may be set in, in order of preference. The first one both the JVM and the
    * browser can find is used. Liberation Sans is what a Linux CI machine has once
    * `playwright install --with-deps` has installed Playwright's system dependencies. Arial is its
    * metric twin on macOS and Windows, and DejaVu Sans is a common Linux desktop default.
    *
    * None is bundled, because an SVG drawn as an image, which is how the harness rasterizes SVG,
    * sees only system fonts and never one a page or a test has loaded.
    */
  val FontCandidates: Seq[String] = Seq("Liberation Sans", "Arial", "DejaVu Sans")

  val red: Color   = Color(255, 0, 0)
  val green: Color = Color(0, 255, 0)
  val blue: Color  = Color(0, 0, 255)
  val black: Color = Color(0, 0, 0)

  private def stroke(width: Double, cap: Cap = Cap.Square, join: Join = Join.Miter) =
    Some(StrokeStyle(blue, width, cap, join))

  private def horizontal(length: Double, cycleMethod: CycleMethod = CycleMethod.NoCycle) =
    LinearGradient(Pos(0, 0), Pos(length, 0), red, blue, cycleMethod)

  private def one(element: Element): Element = Picture(element)

  /** `elements` cropped to `bounds`, an axis-aligned clip, which the plan puts on whole device
    * pixels. A case with one rounds to device pixels.
    */
  private def within(bounds: Bounds, elements: Element*): Element =
    Picture(elements*).crop(bounds)

  private def solid(x: Double, y: Double, color: Color, at: Seq[Double] = ParityCase.Scales) =
    InkProbe(Pos(x, y), Ink.Solid(color), at)
  private def empty(x: Double, y: Double, at: Seq[Double] = ParityCase.Scales) =
    InkProbe(Pos(x, y), Ink.Empty, at)
  private def near(
      x: Double,
      y: Double,
      color: Color,
      by: Int = 12,
      at: Seq[Double] = ParityCase.Scales
  ) =
    InkProbe(Pos(x, y), Ink.Near(color, by), at)

  /** Flat colors. The two browser backends agree to the pixel, and AWT up to antialiasing. */
  private def flat(id: String, catches: String, probes: InkProbe*)(
      picture: => Element
  ) =
    ParityCase(id, catches, () => picture, probes, Exact, AA, AA)

  /** Interpolated colors, where AWT's gradient is allowed its own rounding. */
  private def graded(id: String, catches: String, probes: InkProbe*)(
      picture: => Element
  ) =
    ParityCase(id, catches, () => picture, probes, Exact, Gradient, Gradient)

  /** How far apart, in pixels, two platforms may put the ink of a 20-pixel text. Every backend
    * places a text by its layout box, from the font's own metrics, so what is left is how each font
    * engine hints and rasterizes the glyphs, measured at half a pixel of centroid at most.
    */
  private val TextSlack = 1.0

  private val weightSnapping = KnownDiffers(
    "AWT has only a regular and a bold face and takes every weight below 700 as regular; a " +
      "browser picks the nearest face and draws 600 bold: measured 46 % more ink in bold and its " +
      "ink box 1.5 pixels wider on each side; bounded by 60 % of the larger mass and 4 pixels",
    Layout(TextSlack),
    Layout(4),
    ParityMetric.Limits(massTolerance = Some(0.6))
  )

  private def text(id: String, catches: String)(picture: => Element) =
    ParityCase(
      id,
      catches,
      () => picture,
      Seq.empty,
      canvasVsSvg = AA,
      awtVsCanvas = Layout(TextSlack),
      awtVsSvg = Layout(TextSlack),
      needs = Set(Needs.SvgTextLayout, Needs.CommonFont)
    )

  private val partialArc = Arc(Pos(0, 0), 60, 40, 0, -90, fillWith(red), None)
  private val scalene    = Triangle(30, 18, 26, Pos(0, 0), fillWith(red), stroke(3))

  private def wedge(tipAngleInDegrees: Double, line: Option[StrokeStyle]): Element =
    val halfHeight = 40 * math.tan((tipAngleInDegrees / 2).toRadians)
    Polygon(Pos(0, 0), Seq(Pos(-20, -halfHeight), Pos(20, 0), Pos(-20, halfHeight)), None, line)

  /** A partial arc is `ellipse` on the canvas and an `A` path command in SVG, and a browser
    * flattens the two with different curves, so the pictures agree only up to antialiasing.
    */
  private def swept(id: String, catches: String, probes: InkProbe*)(
      picture: => Element
  ) =
    flat(id, catches, probes*)(picture).copy(canvasVsSvg = AA)

  private val arcs: Seq[ParityCase] = Seq(
    flat("arc-full", "swapped radii", solid(25, 0, red), solid(0, 12, red), empty(27, 12)):
      one(Ellipse(Pos(0, 0), 30, 15, fillWith(red), None))
    ,
    // Both browser backends draw the same path here, and agree to the pixel at 1x and 2x. At 1.5x
    // the SVG is rasterized through a viewBox that scales it by 1.5 inside the document, and
    // Chromium puts the turned edge up to 52 of 255 apart from the canvas's, which sets the same
    // scaling as its transform. Those are two rasterizations, which agree up to antialiasing.
    flat(
      "arc-full-rot",
      "rotation sign of an ellipse",
      solid(21.6, 12.5, red),
      empty(21.6, -12.5)
    )(
      one(Arc(Pos(0, 0), 60, 30, 0, 360, fillWith(red), None).rotateAround(-30, Pos(0, 0)))
    ).copy(atScales = Seq(AtScale(1.5, Backend.Canvas, Backend.Svg, AA))),
    swept("arc-part-pos", "sweep direction and start angle", solid(17, 12.8, red), empty(5, 5)):
      one(Arc(Pos(0, 0), 60, 40, 0, -90, fillWith(red), stroke(3)))
    ,
    swept(
      "arc-part-neg",
      "anticlockwise flag, SVG sweep flag",
      solid(17, -12.8, red),
      empty(5, -5)
    ):
      one(Arc(Pos(0, 0), 60, 40, 0, 90, fillWith(red), stroke(3)))
    ,
    swept("arc-zero-cross", "start angle taken modulo 360", solid(25, 0, red)):
      one(Arc(Pos(0, 0), 60, 40, -300, -120, fillWith(red), None))
    ,
    swept(
      "arc-large",
      "large-arc flag",
      empty(-20, -13),
      solid(-5, -5, red),
      solid(20, 13, red),
      solid(20, -13, red),
      solid(-20, 13, red)
    ):
      one(Arc(Pos(0, 0), 60, 40, 90, -270, fillWith(red), None))
    ,
    swept(
      "arc-neg-start-rot",
      "angle conversion together with rotation",
      solid(-7.2, -15.4, red),
      empty(3.6, 15.2)
    ):
      one(Arc(Pos(0, 0), 60, 40, 45, 200, fillWith(red), stroke(3)).rotateAround(-37, Pos(0, 0)))
    ,
    swept("arc-butt-caps", "caps on an open arc", solid(7.76, 19.32, blue), empty(5, 5)):
      one(Arc(Pos(0, 0), 60, 40, -20, -110, None, stroke(6, Cap.Butt)))
    ,
    flat(
      "arc-full-stroke",
      "a cap at the seam of a closed ellipse",
      solid(20, 0, blue),
      solid(14.14, 14.14, blue),
      empty(0, 0)
    ):
      one(Circle(Pos(0, 0), 20, None, stroke(6)))
  )

  private val polygons: Seq[ParityCase] = Seq(
    flat(
      "poly-concave",
      "vertex order and the fill rule",
      empty(0, -10),
      solid(0, 8, red),
      solid(-15, 0, red)
    ):
      one(
        Polygon(
          Pos(0, 0),
          Seq(Pos(-20, -15), Pos(0, 0), Pos(20, -15), Pos(20, 15), Pos(-20, 15)),
          fillWith(red),
          None
        )
      )
    ,
    flat("line-cap-butt", "a butt cap adds nothing", solid(16, 5.5, blue), empty(31.94, 9.49)):
      one(Line(Pos(2, 2), Pos(30, 9), StrokeStyle(blue, 6, Cap.Butt)))
    ,
    flat("line-cap-round", "a round cap", solid(16, 5.5, blue), solid(31.94, 9.49, blue)):
      one(Line(Pos(2, 2), Pos(30, 9), StrokeStyle(blue, 6, Cap.Round)))
    ,
    flat("line-cap-square", "a square cap on an open path", solid(16, 5.5, blue)):
      one(Line(Pos(2, 2), Pos(30, 9), StrokeStyle(blue, 6, Cap.Square)))
    ,
    // A point is a 1x1 square whose device corner snaps to a whole device pixel. At 1x it is the
    // pixel from (3, 2), at 2x the four from (3, 1.5), and at 1.5x the square from (10/3, 2),
    // which covers its last column and row of device pixels by half.
    flat(
      "point",
      "the pixel a point snaps to",
      solid(3.5, 2.5, red, at = Seq(1.0)),
      solid(4.5, 2.5, green, at = Seq(1.0)),
      solid(3.5, 3.5, green, at = Seq(1.0)),
      solid(3.5, 2.0, red, at = Seq(2.0)),
      solid(4.5, 2.0, green, at = Seq(2.0)),
      solid(3.5, 3.0, green, at = Seq(2.0)),
      solid(3.5, 2.2, red, at = Seq(1.5)),
      solid(4.9, 2.2, green, at = Seq(1.5)),
      solid(3.5, 3.4, green, at = Seq(1.5))
    )(
      Picture(Point(Pos(3.7, 2.2), red), Rectangle(10, 10, Pos(5, 5), fillWith(green), None))
    ),
    flat("rect-fill", "the baseline: a flat rectangle", solid(0, 0, red), solid(-19.5, -9.5, red)):
      one(Rectangle(40, 20, Pos(0, 0), fillWith(red), None))
    ,
    flat(
      "rect-stroke-over",
      "a stroke on top of the fill",
      solid(-19, 0, blue),
      solid(-21.5, 0, blue),
      solid(0, 0, red)
    ):
      one(Rectangle(40, 20, Pos(0, 0), fillWith(red), Some(StrokeStyle(blue, 6))))
    ,
    flat(
      "rect-stroke-under",
      "paint order of a stroke beneath the fill",
      solid(-19, 0, red),
      solid(-21.5, 0, blue),
      solid(0, 0, red)
    ):
      one(Rectangle(40, 20, Pos(0, 0), fillWith(red), Some(StrokeStyle(blue, 6, onTop = false))))
    ,
    flat("tri-equilateral", "apex on top", solid(0, 0, red), solid(-8, 6, red), empty(-11, -9)):
      one(Triangle(30, Pos(0, 0), fillWith(red), stroke(3)))
    ,
    flat("tri-isosceles", "apex on top, base below", solid(0, 3, red), empty(-6, -8)):
      one(Triangle(26, 16, Pos(0, 0), fillWith(red), stroke(3)))
    ,
    flat("tri-scalene", "the apex leans to the shorter side", solid(-6, -2, red), empty(4, -5)):
      one(scalene)
    ,
    flat("pentagon", "the first vertex points up", solid(0, 0, red), solid(0, -16, red)):
      one(Pentagon(20, Pos(0, 0), fillWith(red), stroke(2)))
    ,
    flat("star5", "sharp cusps and their miters", solid(0, 0, red), solid(0, -14, red)):
      one(Star5(24, 10, Pos(0, 0), fillWith(red), stroke(2)))
  )

  private val strokes: Seq[ParityCase] = Seq(
    flat("join-miter", "a miter join's point", solid(30, 0, blue), solid(19, 0, blue)):
      one(wedge(20, stroke(6, join = Join.Miter)))
    ,
    flat("join-round", "a round join", empty(30, 0), solid(21, 0, blue), solid(19, 0, blue)):
      one(wedge(20, stroke(6, join = Join.Round)))
    ,
    flat("join-bevel", "a bevel join", empty(30, 0), empty(22, 0), solid(19, 0, blue)):
      one(wedge(20, stroke(6, join = Join.Bevel)))
    ,
    flat("miter-within-limit", "a miter SVG's own limit of 4 would bevel", solid(26, 0, blue)):
      one(wedge(16, stroke(4)))
    ,
    flat("miter-past-limit", "a miter past the limit is beveled", solid(19, 0, blue)):
      one(wedge(10, stroke(4)))
    ,
    flat(
      "thick-stroke",
      "a stroke wider than its shape",
      solid(0, 0, blue),
      solid(-16, -11, blue)
    ):
      one(Rectangle(20, 10, Pos(0, 0), None, stroke(14)))
  )

  private val transformations: Seq[ParityCase] = Seq(
    flat(
      "rot-own-center",
      "rotation sign of baked vertices",
      solid(14.3, 9.3, red),
      empty(14.3, -9.3)
    ):
      one(Rectangle(40, 20, Pos(0, 0), fillWith(red), stroke(2)).rotateAround(-33, Pos(0, 0)))
    ,
    swept("rot-pivot", "an arc carried around a pivot", solid(36, 29.8, red), empty(23.3, 41.2)):
      one(partialArc.rotateAround(70, Pos(40, 10)))
    ,
    flat(
      "scale-nonuniform",
      "stroke width under uneven scaling",
      solid(0, 0, red),
      // In the top spike, where it is wide enough for the fill to show between the stroke on its
      // two sides. The stroke scales with the star, so it is 1.7 times as thick across as the 2
      // asked for.
      solid(0, -9, red),
      empty(0, 10)
    ):
      one(Star5(24, 10, Pos(0, 0), fillWith(red), stroke(2)).scaleBy(1.7, 0.6))
    ,
    flat("scale-mirror-polygon", "a mirrored polygon", solid(6, -2, red), empty(-4, -5)):
      one(scalene.scaleBy(-1, 1))
    ,
    // Mirrored through its anchor, the middle of its own box, the sweep stays in that box.
    swept("scale-mirror-arc", "a mirrored sweep", solid(20, 7, red), empty(5, 15)):
      one(partialArc.scaleBy(1, -1))
    ,
    flat(
      "order",
      "which element is on top",
      solid(0, 0, green),
      solid(8, 8, blue),
      solid(18, 18, blue),
      solid(-15, -15, red)
    ):
      Picture(
        Circle(Pos(0, 0), 5, fillWith(green), None),
        Rectangle(30, 30, Pos(5, 5), fillWith(blue), None),
        Rectangle(30, 30, Pos(-5, -5), fillWith(red), None)
      )
    ,
    flat(
      "opacity-overlap",
      "blending of translucent fills",
      near(0, 0, Color(85, 0, 170, 192), 4),
      near(-18, -12, Color(255, 0, 0, 128), 2),
      near(18, 12, Color(0, 0, 255, 128), 2)
    )(
      Picture(
        Rectangle(30, 20, Pos(8, 5), fillWith(Color(0, 0, 255, 128)), None),
        Rectangle(30, 20, Pos(-8, -5), fillWith(Color(255, 0, 0, 128)), None)
      )
    ).copy(translucent = true)
  )

  /** A gradient that cycles. SVG has the cycle methods built in while the canvas gets the cycles
    * written out as stops, so the two round their colors apart, and where a repeating gradient
    * jumps, a pixel may fall on either side.
    */
  private def cycled(id: String, catches: String, probes: InkProbe*)(
      picture: => Element
  ) =
    graded(id, catches, probes*)(picture).copy(canvasVsSvg = Gradient)

  /** Where a repeating gradient jumps back to its first color inside a pixel at the edge of a
    * shape, Java2D and Chromium may take opposite sides of the jump. The metric lets a pixel at a
    * jump take either side only away from edges, where the color decides alone, as
    * [[smile.testing.ParityMetric]] describes. At an edge, coverage is compared too, and such a
    * pixel has no counterpart.
    */
  private val jumpAtEdge = KnownDiffers(
    "where the repeating gradient jumps inside an antialiased edge pixel, Java2D colors it with " +
      "the start of the gradient and Chromium with its end: their colors along the gradient are " +
      "within one step of 255 of each other everywhere else, and at 1x three edge pixels of 651 " +
      "fall on a jump, one more than the edge budget allows; bounded by the edge pixels the " +
      "jumps cross, 4 % of them",
    Gradient,
    Gradient,
    ParityMetric.Limits(edgeBudget = 0.04)
  )

  private val nearRed  = Color(221, 0, 34)
  private val nearBlue = Color(34, 0, 221)

  /** Red until 10, red to blue until 30, then by the cycle method. Local x runs from 0 to 60. */
  private def cycling(cycleMethod: CycleMethod): Element =
    one(
      Rectangle(
        60,
        20,
        Pos(0, 0),
        fillWith(LinearGradient(Pos(10, 0), Pos(30, 0), red, blue, cycleMethod)),
        None
      )
    )

  private val gradients: Seq[ParityCase] = Seq(
    graded(
      "grad-rect",
      "where a gradient is anchored",
      near(-25, 0, Color(234, 0, 21)),
      near(0, 0, Color(128, 0, 128)),
      near(25, 0, Color(21, 0, 234))
    ):
      one(Rectangle(60, 20, Pos(0, 0), fillWith(horizontal(60)), None))
    ,
    graded(
      "grad-arc-rot",
      "a gradient turning with its arc",
      near(-16.85, -14.14, nearRed),
      near(16.85, 14.14, nearBlue)
    ):
      one(
        Arc(Pos(0, 0), 60, 30, 0, 360, fillWith(horizontal(60)), None).rotateAround(-40, Pos(0, 0))
      )
    ,
    graded("grad-poly-rot-scale", "gradient points carried through transformations"):
      one(
        Triangle(30, 18, 26, Pos(0, 0), fillWith(horizontal(30)), None)
          .rotateAround(-25, Pos(0, 0))
          .scaleBy(2, 0.5)
      )
    ,
    graded(
      "grad-stroke",
      "a stroke's gradient origin",
      near(5, 0, nearRed),
      near(55, 0, nearBlue)
    ):
      one(Line(Pos(0, 0), Pos(60, 0), StrokeStyle(horizontal(68), 8)))
    ,
    graded(
      "grad-stops",
      "the precision of stop offsets: a long gradient whose middle stops frame the shape",
      near(-25, 0, Color(234, 0, 21)),
      near(0, 0, Color(128, 0, 128)),
      near(25, 0, Color(21, 0, 234))
    ):
      one(
        Rectangle(
          60,
          20,
          Pos(0, 0),
          fillWith(
            LinearGradient(
              Pos(-970, 0),
              Pos(1030, 0),
              Seq(0.0 -> red, 0.485 -> red, 0.515 -> blue, 1.0 -> blue)
            )
          ),
          None
        )
      )
    ,
    graded("grad-alpha", "interpolation toward a transparent stop", near(-28, 0, red))(
      one(
        Rectangle(
          60,
          20,
          Pos(0, 0),
          fillWith(LinearGradient(Pos(0, 0), Pos(60, 0), red, Color(0, 0, 255, 0))),
          None
        )
      )
    ).copy(translucent = true),
    graded(
      "gradient-fade-to-transparent",
      "a fade to a transparent stop whose color channels are white",
      near(-19, 0, red),
      // Every backend interpolates the stops' colors unpremultiplied, so the fade passes pink.
      near(0, 0, Color(255, 128, 128, 128))
    )(
      one(
        Rectangle(
          40,
          20,
          Pos(0, 0),
          fillWith(LinearGradient(Pos(0, 0), Pos(40, 0), red, PresetColor.Transparent)),
          None
        )
      )
    ).copy(translucent = true),
    graded(
      "grad-cycle-none",
      "the end colors beyond a short gradient",
      near(-27, 0, red, 3),
      near(20, 0, blue, 3)
    ):
      cycling(CycleMethod.NoCycle)
    ,
    cycled(
      "grad-cycle-repeat",
      "a repeating gradient",
      near(5, 0, Color(191, 0, 64)),
      near(-25, 0, Color(64, 0, 191))
    ):
      cycling(CycleMethod.Repeat)
    ,
    cycled(
      "grad-cycle-reflect",
      "a reflecting gradient",
      near(5, 0, Color(64, 0, 191)),
      near(-25, 0, Color(191, 0, 64))
    ):
      cycling(CycleMethod.Reflect)
    ,
    cycled("grad-cycle-arc-rot", "a slanted reflecting gradient on a turned ellipse"):
      one(
        Arc(
          Pos(0, 0),
          60,
          30,
          0,
          360,
          fillWith(LinearGradient(Pos(20, 5), Pos(32, 12), red, blue, CycleMethod.Reflect)),
          None
        ).rotateAround(-40, Pos(0, 0))
      )
    ,
    cycled("grad-cycle-stroke", "a repeating gradient along a stroke")(
      one(
        Line(
          Pos(0, 0),
          Pos(60, 21),
          StrokeStyle(LinearGradient(Pos(4, 4), Pos(16, 8), red, blue, CycleMethod.Repeat), 8)
        )
      )
    ).copy(atScales =
      Seq(Backend.Canvas, Backend.Svg).map(AtScale(1.0, Backend.Awt, _, jumpAtEdge))
    ),
    cycled("grad-cycle-poly", "a repeating gradient carried through transformations"):
      one(
        Triangle(
          30,
          18,
          26,
          Pos(0, 0),
          fillWith(LinearGradient(Pos(3, 2), Pos(15, 9), red, blue, CycleMethod.Repeat)),
          stroke(2)
        ).rotateAround(-25, Pos(0, 0)).scaleBy(2, 0.5)
      )
  )

  private def texts(font: String): Seq[ParityCase] =
    def label(
        customBounds: Option[Bounds] = None,
        weight: Int = 400,
        fill: Option[FillStyle] = fillWith(black),
        line: Option[StrokeStyle] = None,
        rotationAngle: Double = 0
    )       = one(Text(Pos(0, 0), customBounds, "Hxg", font, 20, weight, fill, line, rotationAngle))
    val box = Some(Bounds(Pos(0, 0), 80.0, 30.0))

    Seq(
      text("text-plain", "where each platform centers glyphs")(label()),
      text("text-rot", "the pivot a text turns around")(label(rotationAngle = -30)),
      text("text-bounds", "glyphs stretched onto a layout box")(label(customBounds = box)),
      text("text-bounds-rot", "a stretched text turned")(
        label(customBounds = box, rotationAngle = 25)
      ),
      text("text-weight-600", "a weight between regular and bold")(label(weight = 600))
        .copy(awtVsCanvas = weightSnapping, awtVsSvg = weightSnapping),
      text("text-weight-700", "bold")(label(weight = 700)),
      text("text-stroke", "joins of stroked glyphs")(
        label(fill = fillWith(red), line = Some(StrokeStyle(blue, 2)))
      ),
      text("grad-text", "a gradient on stretched glyphs")(
        label(customBounds = box, fill = fillWith(horizontal(80)))
      )
    )

  /** Red, green, blue and black in turn, so that every neighbor differs. */
  private def checker: Bitmap =
    val colors = Seq(red, green, blue, black)
    Bitmap(4, 4, Seq.tabulate(16)(index => colors((index % 4 + index / 4) % 4)))

  private val pixelFrame = Bounds(Pos(-6, -6), Pos(6, 6))

  /** A bitmap, which every pair of backends agrees on as `agreement` says. */
  private def raster(id: String, catches: String, agreement: Expectation)(
      picture: => Element
  ) =
    ParityCase(
      id,
      catches,
      () => picture,
      Seq.empty,
      agreement,
      agreement,
      agreement
    )

  /** Where Java2D and Chromium resample a bitmap differently. Chromium's canvas truncates where a
    * device pixel's center falls on the source to a sixteenth of a source pixel before weighing its
    * bilinear filter, and Java2D takes the exact position. At 1x and 2x a bitmap's pixel centers
    * fall on halves and quarters of a source pixel, which sixteenths hold exactly, and at 1.5x on
    * thirds, which they do not. A weight of 1/6 becomes 2/16, by 10 of 255 on an edge between black
    * and a full color. `measured` is how far apart the pictures were.
    */
  private def chromiumBilinear(measured: String, otherwise: Expectation) =
    KnownDiffers(
      "Chromium's canvas truncates the position its bilinear filter weighs to a sixteenth of a " +
        s"source pixel, and at 1.5x pixel centers fall on thirds of one: measured $measured; " +
        "bounded by two sixteenths of the largest step between neighboring pixels, 32 of 255",
      otherwise,
      otherwise,
      ParityMetric.Limits(flatStep = Some(32))
    )

  /** Where the canvas and SVG backends clip a turned bitmap to its extent. Both clip the same
    * polygon, which the plan gives them, around pixels padded with their own edge, and Chromium
    * covers its sharp corners differently. A canvas clip or fill, and an SVG fill, give a pixel
    * that a corner's tip barely enters about twice the area the tip covers, and an SVG `clip-path`
    * gives that area, as Java2D does. A turned sprite's corners fall on such pixels at 1.5x.
    */
  private val turnedCornerTips = KnownDiffers(
    "Chromium covers the tip of a polygon's sharp corner with about twice its area on canvas and " +
      "with its area through an SVG clip-path: at 1.5x the four corner pixels of the turned " +
      "extent measured alpha 18 against 9, 7 against 4 and 2 against 1, 4 pixels more than 2 " +
      "apart and the largest 9 of 255, and none at 1x or 2x; bounded by 12 of 255 a pixel",
    Exact,
    Exact,
    ParityMetric.Limits(flatStep = Some(12))
  )

  private val bitmaps: Seq[ParityCase] = Seq(
    // Copied one to one at 1x, and resampled at 2x and 1.5x, where the filters round apart.
    raster("bitmap-int", "pixels copied onto whole pixels", Exact)(
      within(pixelFrame, checker.moveBy(1, -1))
    ).copy(atScales =
      Seq(AtScale(1.5, Backend.Awt, Backend.Canvas, chromiumBilinear("largest 17 of 255", Exact)))
    ),
    raster("bitmap-subpixel", "a bitmap between pixels", AA):
      within(pixelFrame, checker.moveBy(0.5, 0.25))
    ,
    // Smooth scaling interpolates on both platforms, and two bilinear filters round apart.
    raster("bitmap-scaled", "resampling when a bitmap is scaled", Exact)(
      one(checker.scaleBy(3, 3))
    ).copy(
      awtVsCanvas = Gradient,
      awtVsSvg = Gradient,
      atScales = Seq(
        AtScale(
          1.5,
          Backend.Awt,
          Backend.Canvas,
          chromiumBilinear("2 flat pixels 14 of 255 apart, 12 allowed", Gradient)
        ),
        // The same difference seen through the SVG image, which Chromium resamples with the same
        // filter, and by the same numbers.
        AtScale(
          1.5,
          Backend.Awt,
          Backend.Svg,
          chromiumBilinear("2 flat pixels 14 of 255 apart, 12 allowed", Gradient)
        )
      )
    ),
    // Drawn directly, the source is resampled once with the composed map and clipped by the
    // crop, inside its own turned extent. The backends differ by two bilinear filters and two
    // antialiased edges.
    raster("bitmap-direct", "a bitmap scaled, turned, flipped and cropped at once", AA)(
      within(
        pixelFrame, {
          val turned = checker.scaleBy(2.2, 2.2).rotateAround(23, Pos(0.5, 0)).flipHorizontally
          turned match
            case bitmap: Bitmap =>
              // The bitmap's own crop, from the corner of its turned buffer.
              val corner = bitmap.source.boundary.upperLeftCorner
              bitmap.crop(Bounds(corner.x + 1, corner.y + 1, corner.x + 9, corner.y + 9))
            case other => other
        }
      )
    ).copy(awtVsCanvas = Gradient, awtVsSvg = Gradient),
    raster("bitmap-dense", "a display-scaled bitmap drawn at its logical size", AA):
      one(
        smile.infrastructure
          .Renderer()
          .createBitmapFrom(one(Rectangle(8, 6, Pos(0, 0), fillWith(red), stroke(2))), 1.5)
      )
    ,
    // Under nearest-neighbor scaling every backend copies pixels, and they have to be the same
    // ones. An enlarged checker is made of blocks, and a display-scaled one drawn back at its
    // logical size is the checker again.
    raster("bitmap-scaled-nearest", "nearest-neighbor scaling of a buffer", Exact)(
      one(checker.scaleBy(3, 3))
    ).copy(scaling = Some(smile.ScalingMethod.NearestNeighbor)),
    // At 1.5x each device pixel spans two source pixels. Its center hits their shared edge. Java2D
    // samples the later pixel and browsers sample the earlier one. JVMPlanRenderer offsets the
    // image by 1/1024 pixel to match browser sampling.
    raster("bitmap-dense-nearest", "nearest-neighbor resampling as a bitmap is drawn", Exact)(
      within(
        pixelFrame,
        smile.infrastructure
          .Renderer()
          .createBitmapFrom(
            one(checker),
            3.0,
            smile.RenderSettings(bufferScalingMethod = smile.ScalingMethod.NearestNeighbor)
          )
          .moveBy(1, -1)
      )
    ).copy(scaling = Some(smile.ScalingMethod.NearestNeighbor)),
    raster("bitmap-scaled-fast", "the fast method copies pixels too", Exact)(
      one(checker.scaleBy(2, 4))
    ).copy(scaling = Some(smile.ScalingMethod.Fast)),
    raster("bitmap-alpha", "what premultiplied storage loses", Exact)(
      within(
        pixelFrame,
        Bitmap(
          4,
          4,
          Seq.tabulate(16)(index => Color(255, 40, 200, Seq(255, 128, 16, 0)(index % 4)))
        )
      )
    ).copy(
      translucent = true,
      atScales =
        Seq(AtScale(1.5, Backend.Awt, Backend.Canvas, chromiumBilinear("largest 4 of 255", Exact)))
    )
  )

  private def cross: Element = Picture(
    Rectangle(40, 10, Pos(0, 0), fillWith(blue), None),
    Rectangle(10, 40, Pos(0, 0), fillWith(green), None)
  )

  /** A masked picture. A mask is drawn in a layer of the device pixels the group covers, which the
    * SVG backend's mask region, planned at 1x, matches at 1x alone. Shown larger, it cuts the
    * antialiased edge of a mask that ends between device pixels. So a mask rounds to device pixels.
    */
  private def masked(id: String, catches: String, probes: InkProbe*)(
      picture: => Element
  ) =
    ParityCase(id, catches, () => picture, probes, AA, AA, AA)

  private val masks: Seq[ParityCase] = Seq(
    masked(
      "mask",
      "where a mask sits and what it keeps",
      solid(0, 0, blue),
      solid(0, 8, green),
      empty(8, 8)
    ):
      one(MaskGroup(Circle(Pos(0, 0), 12, fillWith(red), None), cross))
    ,
    masked(
      "mask-inverted",
      "an inverted mask, which cuts its shape out as a hole",
      empty(0, 0),
      empty(0, 8),
      solid(16, 0, blue),
      solid(0, 16, green)
    ):
      one(MaskGroup(Circle(Pos(0, 0), 12, fillWith(red), None), cross, inverted = true))
    ,
    masked("mask-black", "alpha, not luminance, masking", solid(0, 0, blue), solid(0, 8, green)):
      one(MaskGroup(Circle(Pos(0, 0), 12, fillWith(black), None), cross))
    ,
    masked("mask-gradient", "a mask that fades", near(-10, 0, blue, 40))(
      one(
        MaskGroup(
          Rectangle(
            24,
            24,
            Pos(0, 0),
            fillWith(LinearGradient(Pos(0, 0), Pos(24, 0), black, Color(0, 0, 0, 0))),
            None
          ),
          cross
        )
      )
    ).copy(translucent = true),
    masked(
      "mask-nested",
      "a mask group inside a masked picture",
      solid(5, 5, green),
      solid(11, 11, green),
      empty(-10, -10)
    ):
      one(
        MaskGroup(
          Rectangle(30, 30, Pos(0, 0), fillWith(red), None),
          one(
            MaskGroup(
              Circle(Pos(5, 5), 12, fillWith(red), None),
              one(Rectangle(60, 60, Pos(0, 0), fillWith(green), None))
            )
          )
        )
      )
    ,
    masked("mask-subpixel", "a mask whose edges fall between pixels", solid(0.5, 0.5, blue)):
      within(
        Bounds(Pos(-16, -16), Pos(16, 16)),
        MaskGroup(Rectangle(21, 21, Pos(0.25, 0.25), fillWith(red), None), cross)
      )
    ,
    // The content covers -15..15 x -10..10 and the mask 5..25 x -10..10, so the group, and the
    // layer each backend draws it in, covers 5..15 x -10..10, within a crop that reaches past it.
    masked(
      "mask-past-content",
      "a mask half past its content, whose layer is the part of the mask over the content",
      solid(10, 0, blue),
      solid(13, 5, blue),
      empty(16, 0),
      empty(3, 0)
    ):
      within(
        Bounds(Pos(-20, -15), Pos(30, 15)),
        MaskGroup(
          Circle(Pos(15, 0), 10, fillWith(red), None),
          Rectangle(30, 20, Pos(0, 0), fillWith(blue), None)
        )
      )
  )

  /** How many levels a deep case nests. It is three times
    * `smile.infrastructure.DrawPlan.MaxNesting`, the depth a plan nests before it draws the deeper
    * layers ahead as slots.
    */
  private val Depth = 48

  /** `start` with `step` applied to it `Depth` times, each time to what the step before made. */
  private def nest(start: Element)(step: (Element, Int) => Element): Element =
    (0 until Depth).foldLeft(start)(step)

  private def deepSquare = Rectangle(40, 40, Pos(0, 0), fillWith(blue), None)
  private def deepCircle = Circle(Pos(0, 0), 15, fillWith(green), None)
  private val deepWindow = Bounds(Pos(-15, -15), Pos(15, 15))

  /** A small dot, in one of 50 columns of a row, depending on `i`. */
  private def deepDot(i: Int) = Circle(Pos(i % 50 * 4.0, 0), 2, fillWith(green), None)

  /** A dot in front of each level of a turned wrap, whose ink stays as the levels below fade. */
  private def deepCenter = Circle(Pos(0, 0), 4, fillWith(green), None)

  /** Java2D and Chromium round the alpha of an antialiased edge that a layer masks apart, and each
    * level masks the edges of the level below again. A level is a mask, or a turned crop, which is
    * drawn as a mask of its rectangle. The slots do not cause the drift. A plan draws 16 levels
    * without slots, and those measured 15 of 572 edge pixels unmatched at 2x.
    */
  private val compoundedMasks = KnownDiffers(
    "Java2D and Chromium round the alpha of an antialiased edge apart, and each of 48 levels of " +
      "masks or turned crops masks the edges of the level below again: measured 18 of 542 edge " +
      "pixels unmatched at 2x and 9 of 394 at 1.5x; bounded by 5 % of the edge pixels",
    AA,
    AA,
    ParityMetric.Limits(edgeBudget = 0.05)
  )

  /** Java2D and Chromium resample a turned tile with filters of their own, and each level of a
    * turned wrap resamples what the level below drew, so what is left of the deeper levels fades
    * apart, while the dot in front of the last one agrees. The slots do not cause the drift. The 16
    * levels a plan draws without slots measured 624.1 against 502.3 pixels of ink at 1x, the ink
    * 0.46 pixels apart.
    */
  private val compoundedResampling = KnownDiffers(
    "Java2D and Chromium resample a turned tile with filters of their own, and each of 48 levels " +
      "resamples what the level below drew, so the deeper levels fade apart: measured 571.8 " +
      "against 451.5 pixels of ink at 1x (21 % of the larger mass), 1317.9 against 1117.9 at 1.5x " +
      "and 2255.5 against 2027.2 at 2x, the ink up to 0.49 pixels apart and 23 of 855 edge " +
      "pixels unmatched at 1x; bounded by where and how much ink there is, to 0.75 pixels and " +
      "30 % of the larger mass",
    AA,
    Layout(0.75),
    ParityMetric.Limits(massTolerance = Some(0.3))
  )

  /** Trees deeper than a plan nests, whose deeper layers each backend draws ahead. Their masks and
    * turned clips round to device pixels as a mask does, which `masked` explains. The canvas and
    * SVG agree on them as on shallow ones, and Java2D drifts from both as the levels compound.
    *
    * In the SVG output each level of masks or turned crops is a mask or a clipped group inside the
    * one before. The wraps come out shallow, because their tile is an image that already holds the
    * levels below.
    */
  private val deep: Seq[ParityCase] = Seq(
    masked("deep-masks", "masks of masks, drawn ahead", solid(0, 0, green), empty(18, 18))(
      nest(deepSquare)((a, _) => MaskGroup(a, deepCircle))
    ).copy(awtVsCanvas = compoundedMasks, awtVsSvg = compoundedMasks),
    // A mask of a turned masked group is a group of its own, so the levels nest. Quarter turns
    // keep the axes and the pixel grid, so nothing is resampled.
    masked(
      "deep-masked",
      "masks of turned masked content, drawn ahead",
      solid(0, 0, blue),
      empty(18, 18)
    )(
      nest(deepSquare)((a, _) => MaskGroup(deepCircle, a.rotateBy(90)))
    ).copy(awtVsCanvas = compoundedMasks, awtVsSvg = compoundedMasks),
    // The masks of the moved groups are one group's, whose content goes through them in order.
    masked(
      "deep-masked-flat",
      "masks of moved masked content, one layer of masks in order",
      solid(12, 0, blue),
      empty(0, 0)
    )(
      nest(deepSquare)((a, _) => MaskGroup(deepCircle, a).moveBy(0.5, 0))
    ),
    masked("deep-turned-crops", "turned crops of turned crops, drawn ahead", solid(0, -8, blue))(
      nest(deepSquare)((a, i) => a.rotateBy(5).addToFront(deepDot(i)).crop(deepWindow))
    ).copy(awtVsCanvas = compoundedMasks, awtVsSvg = compoundedMasks),
    masked("deep-turned-wraps", "turned crops of wraps, drawn ahead", solid(0, 0, green))(
      nest(deepSquare)((a, _) =>
        a.wrapShift(3, 2).rotateBy(5).addToFront(deepCenter).crop(deepWindow)
      )
    ).copy(
      translucent = true,
      awtVsCanvas = compoundedResampling,
      awtVsSvg = compoundedResampling
    )
  )

  private val frames: Seq[ParityCase] = Seq(
    flat(
      "viewport-crop",
      "a viewport cutting through a shape",
      solid(20, 15, red),
      solid(20, 24.5, blue),
      empty(6, 6)
    )(
      within(Bounds(Pos(5, 5), Pos(35, 25)), Circle(Pos(20, 15), 10, fillWith(red), stroke(2)))
    ),
    flat("viewport-larger", "a viewport wider than the content", solid(0, 0, red), empty(-15, 0))(
      within(
        Bounds(Pos(-20, -10), Pos(20, 10)),
        Rectangle(10, 10, Pos(0, 0), fillWith(red), None)
      )
    ),
    flat("viewport-empty", "blank space with nothing in it", empty(3, 3))(
      Empty.crop(Bounds(Pos(0, 0), Pos(12, 8)))
    ),
    flat("frac-bounds", "a picture whose size is no whole number", solid(0, 0, red)):
      one(Rectangle(20.6, 10.4, Pos(0.3, 0.3), fillWith(red), None))
    ,
    // The stroke is wider than one pixel because Chromium 131, which Playwright 1.49 drives, draws
    // the closing edge of a one-pixel square-capped outline half a pixel off, on canvas and in
    // SVG alike. So does 148. The build Playwright 1.62 drives does not.
    flat("subpixel", "edges at half pixels", solid(0.5, 0.5, red))(
      within(
        Bounds(Pos(-8, -8), Pos(8, 8)),
        Rectangle(10, 10, Pos(0.5, 0.5), fillWith(red), stroke(2))
      )
    )
  )

  private def onGreen(element: Element): Element =
    Picture(element, Rectangle(12, 12, Pos(0, 0), fillWith(green), None))

  private val degenerates: Seq[ParityCase] = Seq(
    flat("empty", "a picture of nothing"):
      Empty
    ,
    flat("degenerate-point-polygon", "a one-point polygon paints nothing", solid(2.5, 1.5, green)):
      onGreen(Polygon(Pos(2, 1), Seq(Pos(0.5, 0.5)), fillWith(red), None))
    ,
    flat("degenerate-empty-polygon", "a polygon without points", solid(0, 0, green)):
      onGreen(Polygon(Pos(0, 0), Seq.empty, fillWith(red), stroke(3)))
    ,
    flat("degenerate-zero-arc", "an arc that sweeps nothing", solid(-3, -3, green)):
      onGreen(Arc(Pos(0, 0), 8, 8, -30, 0, fillWith(red), None))
    ,
    flat("degenerate-flat-rectangle", "a rectangle without height", solid(0, 4, green)):
      onGreen(Rectangle(8, 0, Pos(0, 0), fillWith(red), None))
    ,
    flat("degenerate-zero-line", "a line from a point to itself", solid(-5, -5, green)):
      onGreen(Line(Pos(0, 0), Pos(0, 0), StrokeStyle(blue, 4, Cap.Butt)))
  )

  /** At 1x, the SVG picture's left edge falls on the middle of a pixel. Chromium clips an SVG image
    * to its own box without antialiasing, on canvas and inside an SVG alike, and so leaves out the
    * column of pixels it half covers, which JSVG paints half covered. At 2x the edge is on a whole
    * device pixel, and at 1.5x a quarter into one, within the edge budget.
    */
  private val svgImageClip = KnownDiffers(
    "Chromium clips an SVG image to its box without antialiasing and leaves out a column the " +
      "picture half covers, which JSVG paints at half coverage: measured 16 of 202 edge pixels " +
      "unmatched, 10.5 pixels of ink less; bounded by a column of pixels along the edge: 12 % of " +
      "the edge pixels unmatched and a tenth of a pixel of ink per edge pixel",
    AA,
    AA,
    ParityMetric.Limits(edgeBudget = 0.12, massTolerance = Some(0.1))
  )

  /** The document `svg-picture` draws. It and the others below are each parsed once, so that its
    * decoding for canvas output is shared by every rendering of it through `svgPictures`.
    */
  private lazy val svgDocument: SvgPicture = SvgPicture.fromString(
    """<svg xmlns="http://www.w3.org/2000/svg" width="24" height="16" viewBox="0 0 24 16">""" +
      """<rect x="0" y="0" width="14" height="16" fill="#FF0000"/>""" +
      """<circle cx="17" cy="8" r="6" fill="#0000FF"/></svg>"""
  )

  /** What the documents of the SVG size cases draw, in a viewBox of 24 by 12 from `(x, y)`. It is a
    * red square from 2 to 10 on each axis and a blue circle of radius 4 around (17, 6), with
    * nothing between them and nothing within 2 of the box's edges. Drawn at the viewBox's own size
    * and centered at the origin, the square's middle is at (-6, 0) and the circle's at (5, 0).
    */
  private def marks(x: Double, y: Double): String =
    s"""<rect x="${x + 2}" y="${y + 2}" width="8" height="8" fill="#FF0000"/>""" +
      s"""<circle cx="${x + 17}" cy="${y + 6}" r="4" fill="#0000FF"/>"""

  /** A document of `marks` from the viewBox's origin `(x, y)`, with the root's other attributes. */
  private def marked(attributes: String, x: Double = 0, y: Double = 0): SvgPicture =
    SvgPicture.fromString(
      s"""<svg xmlns="http://www.w3.org/2000/svg" $attributes>${marks(x, y)}</svg>"""
    )

  /** Sized in millimeters, which makes 24 by 12 pixels at 96 to an inch. */
  private lazy val svgInMillimeters: SvgPicture =
    marked("""width="6.35mm" height="3.175mm" viewBox="0 0 24 12"""")

  /** A width in percent, which is no size, and a height of 24. The width keeps the viewBox's ratio,
    * so the picture is 48 by 24, the viewBox drawn twice its size.
    */
  private lazy val svgInPercent: SvgPicture =
    marked("""width="100%" height="24" viewBox="0 0 24 12"""")

  /** No size at all, so the picture is the viewBox's size, 24 by 12. */
  private lazy val svgWithoutSize: SvgPicture = marked("""viewBox="0 0 24 12"""")

  /** A viewBox that starts at (10, 20), with the marks there. It is the same picture as at the
    * origin.
    */
  private lazy val svgViewBoxOrigin: SvgPicture =
    marked("""width="24" height="12" viewBox="10 20 24 12"""", 10, 20)

  /** A viewBox half as tall as the size, which `xMidYMax meet` draws at its own size along the
    * bottom. The marks are in the lower half of the 24 by 24 picture, from y = 0 to 12.
    */
  private lazy val svgAspectMismatch: SvgPicture =
    marked("""width="24" height="24" viewBox="0 0 24 12" preserveAspectRatio="xMidYMax meet"""")

  /** A small file in the shape Inkscape saves one. It has a size in millimeters with a viewBox in
    * them, the editor's `sodipodi:namedview`, `metadata` with RDF, `inkscape:` and `sodipodi:`
    * attributes, and a layer moved by a transform. It draws the same marks as the size cases, 24 by
    * 12 pixels, at 3.7795 pixels to a millimeter.
    */
  private lazy val svgInkscapeFile: SvgPicture = SvgPicture.fromString(
    """<?xml version="1.0" encoding="UTF-8" standalone="no"?>
      |<!-- Created with Inkscape (http://www.inkscape.org/) -->
      |
      |<svg
      |   width="6.35mm"
      |   height="3.175mm"
      |   viewBox="0 0 6.35 3.175"
      |   version="1.1"
      |   id="svg1"
      |   inkscape:version="1.3.2 (091e20e, 2023-11-25)"
      |   sodipodi:docname="marks.svg"
      |   xmlns:inkscape="http://www.inkscape.org/namespaces/inkscape"
      |   xmlns:sodipodi="http://sodipodi.sourceforge.net/DTD/sodipodi-0.dtd"
      |   xmlns="http://www.w3.org/2000/svg"
      |   xmlns:svg="http://www.w3.org/2000/svg"
      |   xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
      |   xmlns:cc="http://creativecommons.org/ns#"
      |   xmlns:dc="http://purl.org/dc/elements/1.1/">
      |  <sodipodi:namedview
      |     id="namedview1"
      |     pagecolor="#ffffff"
      |     bordercolor="#000000"
      |     borderopacity="0.25"
      |     inkscape:showpageshadow="2"
      |     inkscape:pageopacity="0.0"
      |     inkscape:pagecheckerboard="0"
      |     inkscape:deskcolor="#d1d1d1"
      |     inkscape:document-units="mm"
      |     inkscape:zoom="16"
      |     inkscape:cx="12"
      |     inkscape:cy="6"
      |     inkscape:window-width="1440"
      |     inkscape:window-height="900"
      |     inkscape:current-layer="layer1" />
      |  <defs
      |     id="defs1" />
      |  <metadata
      |     id="metadata1">
      |    <rdf:RDF>
      |      <cc:Work
      |         rdf:about="">
      |        <dc:format>image/svg+xml</dc:format>
      |        <dc:type
      |           rdf:resource="http://purl.org/dc/dcmitype/StillImage" />
      |        <dc:title>marks</dc:title>
      |      </cc:Work>
      |    </rdf:RDF>
      |  </metadata>
      |  <g
      |     inkscape:label="Layer 1"
      |     inkscape:groupmode="layer"
      |     id="layer1"
      |     transform="translate(-20,-30)">
      |    <rect
      |       style="fill:#ff0000;fill-opacity:1;stroke:none;stroke-width:0.264583"
      |       id="rect1"
      |       width="2.1166666"
      |       height="2.1166666"
      |       x="20.529167"
      |       y="30.529167"
      |       inkscape:label="square" />
      |    <path
      |       style="fill:#0000ff;fill-opacity:1;stroke:none;stroke-width:0.264583"
      |       id="path1"
      |       sodipodi:type="arc"
      |       sodipodi:cx="24.497917"
      |       sodipodi:cy="31.5875"
      |       sodipodi:rx="1.0583333"
      |       sodipodi:ry="1.0583333"
      |       d="m 25.55625,31.5875 a 1.0583333,1.0583333 0 0 1 -1.058333,1.058333 1.0583333,1.0583333 0 0 1 -1.058334,-1.058333 1.0583333,1.0583333 0 0 1 1.058334,-1.058333 1.0583333,1.0583333 0 0 1 1.058333,1.058333 z" />
      |  </g>
      |</svg>
      |""".stripMargin
  )

  /** Every SVG document the cases draw, so that a platform that decodes them can wait for all. */
  lazy val svgPictures: Seq[SvgPicture] = Seq(
    svgDocument,
    svgInMillimeters,
    svgInPercent,
    svgWithoutSize,
    svgViewBoxOrigin,
    svgAspectMismatch,
    svgInkscapeFile
  )

  /** Where the marks of a size case's document show, drawn `times` their viewBox's size and moved
    * down by `down`. They are the square, the circle, and nothing between them or in the margin.
    */
  private def markProbes(times: Double = 1, down: Double = 0): Seq[InkProbe] = Seq(
    solid(-6 * times, down, red),
    solid(5 * times, down, blue),
    empty(0, down),
    empty(-11 * times, down - 5 * times)
  )

  /** A case of an SVG document, which each backend draws with its own SVG renderer, held to what
    * `svg-picture` is.
    */
  private def svgCase(id: String, catches: String, probes: Seq[InkProbe])(picture: => Element) =
    ParityCase(id, catches, () => picture, probes, AA, AA, AA, needs = Set(Needs.SvgImages))

  private val svgs: Seq[ParityCase] = Seq(
    svgCase("svg-units-mm", "a size in millimeters, at 96 pixels per inch", markProbes())(
      one(svgInMillimeters)
    ),
    svgCase(
      "svg-percent-size",
      "a width in percent, which takes the height and the viewBox's ratio",
      markProbes(times = 2)
    )(one(svgInPercent)),
    svgCase("svg-no-size", "a document with a viewBox and no size", markProbes())(
      one(svgWithoutSize)
    ),
    svgCase("svg-viewbox-origin", "a viewBox that does not start at the origin", markProbes())(
      one(svgViewBoxOrigin)
    ),
    // Drawn from the top, or in the middle, the square would cover y = -6 or y = -2.
    svgCase(
      "svg-aspect-mismatch",
      "a viewBox of another shape than the size, placed by preserveAspectRatio",
      markProbes(down = 6) ++ Seq(empty(-6, -6), empty(-6, -2))
    )(one(svgAspectMismatch)),
    svgCase(
      "svg-inkscape-file",
      "a file as Inkscape saves one, sized in millimeters",
      markProbes()
    )(
      one(svgInkscapeFile)
    ),
    // The marks' opacity lets the green through, so green shows where they are.
    masked(
      "svg-as-mask",
      "an SVG picture as the mask of other content",
      solid(-6, 0, green),
      solid(5, 0, green),
      empty(0, 0),
      empty(-11, -5)
    )(
      one(MaskGroup(svgWithoutSize, Rectangle(30, 16, Pos(0, 0), fillWith(green), None)))
    ).copy(needs = Set(Needs.SvgImages)),
    // A circle mask that holds the whole square and reaches past the circle's left edge.
    masked(
      "svg-masked",
      "an SVG picture as the content of a mask",
      solid(-6, 0, red),
      solid(-9, -3, red),
      solid(2, 0, blue),
      empty(5, 0),
      empty(8, 0)
    )(
      one(MaskGroup(Circle(Pos(-4, 0), 7.5, fillWith(black), None), svgWithoutSize))
    ).copy(needs = Set(Needs.SvgImages))
  )

  /** Colors that change slowly from pixel to pixel, as in a drawn sprite, where a resampled sprite
    * is within the rounding of two bilinear filters. `column` and `row` are from 0 to 6.
    */
  private def spriteColor(column: Int, row: Int): Color =
    Color(60 + 25 * column, 40 + 25 * row, 160)

  /** A 7 by 7 sprite centered at the origin, a whole position, so that its corner is at a half. */
  private def oddSprite: Bitmap = Bitmap(7, 7, spriteColor).moveTo(Pos(0, 0))

  /** The pixels of `oddSprite` at 1x, where it is copied one to one with its corner snapped from
    * -3.5 to -3, the nearest whole device pixel up the way, as
    * `smile.infrastructure.DeviceGrid.snap` rounds.
    */
  private val oddSpriteAtOne: Seq[InkProbe] = Seq(
    solid(-2.5, -2.5, spriteColor(0, 0), at = Seq(1.0)),
    solid(3.5, 3.5, spriteColor(6, 6), at = Seq(1.0)),
    empty(-3.6, 0, at = Seq(1.0))
  )

  /** Content whose left half is red and right half blue, 20 by 10 around the origin. */
  private def halves: Element = Picture(
    Rectangle(10, 10, Pos(-5, 0), fillWith(red), None),
    Rectangle(10, 10, Pos(5, 0), fillWith(blue), None)
  )

  /** The cases the display density brought. Each is at 1x, 2x and 1.5x, where the plan decides on
    * device pixels.
    */
  private def densities(font: String): Seq[ParityCase] = Seq(
    // A circle scaled three times wider. Its outline is three times as thick at the sides as at
    // the top, 6 pixels against 2.
    flat(
      "stroke-nonuniform",
      "a stroke scaled along each axis apart",
      solid(30, 0, blue),
      solid(27.5, 0, blue),
      solid(0, 9.6, blue),
      empty(0, 0),
      empty(25.5, 0)
    ):
      one(Circle(Pos(0, 0), 10, None, Some(StrokeStyle(blue, 2))).scaleBy(3, 1))
    ,
    // A wedge whose tip is mitered within the limit in its own frame, scaled to twice the length
    // and half the height. The miter reaches x = 68.7, where a bevel would end at 40.6. Moved
    // down by half a pixel, so that a row of pixels lies along its middle.
    ParityCase(
      "miter-nonuniform",
      "a miter of a shape scaled unevenly",
      () => one(wedge(16, stroke(4)).scaleBy(2, 0.5).moveBy(0, 0.5)),
      Seq(solid(50, 0.5, blue), solid(30, 0.5, blue), empty(70, 0.5)),
      Exact,
      AA,
      AA
    ),
    text("text-mirrored", "glyphs mirrored with their text")(
      one(Text(Pos(0, 0), None, "Hxg", font, 20, 400, fillWith(black), None).scaleBy(-1, 1))
    ),
    // The library has no italic face, so the glyphs are slanted the way an oblique face is, by a
    // shear that leans their tops to the right. The ink leaves the layout box on the right at the
    // top and on the left at the bottom, and has to stay within the bounds.
    text("text-slanted", "slanted glyphs against their bounds")(
      Text(Pos(0, 0), None, "Hxf", font, 20, 400, fillWith(black), None) match
        case shape: Shape =>
          one(shape.withAffine(shape.affine.andThen(Affine(1, 0, -0.25, 1, 0, 0))))
        case other => other
    ),
    // Turned, a clip is a layer with an antialiased edge on every backend.
    ParityCase(
      "clip-rotated",
      "a turned clip",
      () => within(Bounds(Pos(-10, -6), Pos(10, 6)), cross).rotateBy(30),
      // Turned 30 degrees counterclockwise, the arm along x rises to the right.
      Seq(solid(0, 0, blue), solid(6, -3.5, blue), empty(-12, -9), empty(12, 9)),
      AA,
      AA,
      AA
    ),
    // An axis-aligned clip keeps every device pixel its rectangle touches, so its edges are
    // whole device pixels at every scale.
    flat(
      "clip-fractional",
      "a clip whose edges fall between pixels",
      solid(-7.2, -4.5, red),
      solid(8.1, 5.3, red)
    )(
      within(
        Bounds(Pos(-7.3, -4.6), Pos(8.2, 5.4)),
        Rectangle(30, 20, Pos(0, 0), fillWith(red), None)
      )
    ),
    // Shifted by 3.5 in a period of 20, snapped to whole device pixels. That is 4 at 1x, 3.5 at 2x
    // and 10/3 at 1.5x. Red starts between -6.67 and -6 and ends between 3.33 and 4 at every scale.
    flat(
      "wrap-fractional-offset",
      "a wrap shifted by a fraction of a pixel",
      solid(0, 0, red),
      solid(-5.5, 0, red),
      solid(4.5, 0, blue),
      solid(6, 0, blue),
      solid(-9, 0, blue)
    )(halves.wrapShift(3.5, 0)),
    // With a 15.5-pixel period and a shift of 4, the copies meet at -3.75. A gap or a partially
    // transparent edge there exposes the background.
    flat(
      "wrap-fractional-period",
      "a wrap whose period is no whole number of pixels, with no seam where its copies meet",
      solid(-3.75, 0, red),
      solid(-3.75, 2.5, red),
      solid(1, 0, red)
    )(
      Picture(
        Rectangle(15.5, 6, Pos(0, 0), fillWith(red), None),
        Rectangle(2, 6, Pos(5, 0), fillWith(blue), None)
      ).wrapShift(4, 0)
    ),
    // Resampled smoothly, a crop shows its own pixels alone, and the filters round apart.
    raster("bitmap-crop-smooth", "a smoothed bitmap cropped between pixels", Exact)(
      checker
        .withResampling(ResamplingHint.Smooth)
        .scaleBy(2, 2)
        .crop(Bounds(Pos(-3.3, -2.7), Pos(4.1, 3.6)))
    ).copy(
      probes = Seq(InkProbe(Pos(0, 0), Ink.Opaque)),
      awtVsCanvas = Gradient,
      awtVsSvg = Gradient
    ),
    // At 1x the sprite is copied one to one with its corner snapped to a whole pixel. At 2x and
    // 1.5x it is resampled smoothly at its exact place, as its hint asks, and every backend has to
    // resample it alike.
    raster("sprite-odd-centered", "an odd-sized sprite centered on a whole position", Exact)(
      one(oddSprite)
    ).copy(
      probes = oddSpriteAtOne :+ InkProbe(Pos(0, 0), Ink.Opaque),
      awtVsCanvas = Gradient,
      awtVsSvg = Gradient
    ),
    // Pixelated, the sprite is copied as blocks. At 2x, from its corner at -7, a whole device
    // pixel, each of its pixels is two by two device pixels.
    raster("sprite-odd-pixelated", "an odd-sized pixelated sprite as whole-pixel blocks", Exact)(
      one(oddSprite.withResampling(ResamplingHint.Pixelated))
    ).copy(
      probes = oddSpriteAtOne ++ Seq(
        solid(-3.25, -3.25, spriteColor(0, 0), at = Seq(2.0)),
        solid(-2.75, -2.75, spriteColor(0, 0), at = Seq(2.0)),
        solid(2.75, 2.75, spriteColor(6, 6), at = Seq(2.0)),
        solid(3.25, 3.25, spriteColor(6, 6), at = Seq(2.0)),
        solid(-2.25, -3.25, spriteColor(1, 0), at = Seq(2.0))
      ),
      crisp = Some(
        Crisp(
          ParityCase.Scales,
          (for column <- 0 until 7; row <- 0 until 7 yield spriteColor(column, row)).toSet
        )
      )
    ),
    // Turned and drawn whole, at a size where no source pixel is wider than a device pixel at 1x.
    // Its edge fades out on the JVM, and at 2x and 1.5x is cut by an antialiased mask. Drawn
    // smaller, Chromium filters the SVG's image and the canvas's drawing of it apart, by up to 8
    // of 255 at 1x, so the smaller one agrees with its gradients' rounding on every pair.
    raster("sprite-turned", "a turned sprite's antialiased edges", Exact)(
      one(oddSprite.rotateBy(30))
    ).copy(
      probes = Seq(InkProbe(Pos(0, 0), Ink.Opaque)),
      awtVsCanvas = Gradient,
      awtVsSvg = Gradient,
      atScales = Seq(AtScale(1.5, Backend.Canvas, Backend.Svg, turnedCornerTips))
    ),
    // Pixelated, a turned sprite's pixels are nearest neighbors, and its edges are antialiased as
    // its extent's on every backend.
    raster("sprite-turned-pixelated", "a turned pixelated sprite's edges", Exact)(
      one(oddSprite.withResampling(ResamplingHint.Pixelated).rotateBy(30))
    ).copy(
      probes = Seq(InkProbe(Pos(0, 0), Ink.Opaque)),
      awtVsCanvas = AA,
      awtVsSvg = AA,
      atScales = Seq(AtScale(1.5, Backend.Canvas, Backend.Svg, turnedCornerTips))
    ),
    raster("sprite-turned-small", "a turned sprite drawn smaller than its pixels", Gradient)(
      one(oddSprite.scaleBy(0.6).rotateBy(-20))
    ).copy(probes = Seq(InkProbe(Pos(0, 0), Ink.Opaque))),
    masked(
      "clip-in-mask",
      "a clip inside a masked picture",
      solid(0, 0, blue),
      solid(0, 7, green),
      empty(10, 0),
      empty(0, -10)
    ):
      one(
        MaskGroup(
          Circle(Pos(0, 0), 12, fillWith(red), None),
          within(Bounds(Pos(-8, -8), Pos(8, 8)), cross)
        )
      )
    ,
    masked(
      "mask-in-clip",
      "a masked picture inside a clip",
      solid(0, 0, blue),
      solid(0, -9, green),
      solid(-5.5, 0, blue),
      empty(-5.5, -9)
    ):
      within(
        Bounds(Pos(-6, -10), Pos(10, 6)),
        MaskGroup(Circle(Pos(0, 0), 12, fillWith(red), None), cross)
      )
    ,
    // A static SVG document, which each backend draws with its own SVG renderer. That is JSVG on
    // the JVM, and the browser's in a canvas and as an image inside the SVG output.
    ParityCase(
      "svg-picture",
      "a static SVG picture placed and scaled",
      () => one(svgDocument.scaleBy(1.5, 1).moveBy(0.5, 0)),
      Seq(solid(-8, 0, red), solid(8, 0, blue), empty(16, -7)),
      AA,
      AA,
      AA,
      needs = Set(Needs.SvgImages),
      atScales = Seq(Backend.Canvas, Backend.Svg).map(AtScale(1.0, Backend.Awt, _, svgImageClip))
    )
  )

  /** Every case, with the text cases set in `font`. */
  def all(font: String): Seq[ParityCase] =
    arcs ++ polygons ++ strokes ++ transformations ++ gradients ++ texts(font) ++ bitmaps ++
      masks ++ deep ++ frames ++ degenerates ++ densities(font) ++ svgs

  /** Every case, with the text cases set in the first candidate font. For listing and for the cases
    * that involve no text. A suite that renders text picks the font itself.
    */
  lazy val all: Seq[ParityCase] = all(FontCandidates.head)

  def byId(id: String, font: String): ParityCase =
    all(font).find(_.id == id).getOrElse(throw new NoSuchElementException(s"no parity case '$id'"))
