package smile.infrastructure

import smile.RenderSettings
import smile.colors.{Color, LinearGradient}
import smile.modeling.{Bounds, Pos}
import smile.pictures.*
import smile.pictures.{FillStyle, ResamplingHint, StrokeStyle}

/** The elements every renderer of the draw plan draws in its suite, and the platform-independent
  * check of where their ink lands. A backend renders each case's
  * `smile.infrastructure.PlanRenderCases.framed` element, reads the device pixels back as packed
  * ARGB and passes them to `smile.infrastructure.PlanRenderCases.inkProblems`.
  */
object PlanRenderCases:

  /** An element to render, and whether its ink reaches every edge of its bounds (`tight`), as a
    * shape's does and a text's layout box need not. `measuresText` tells that making the element
    * lays out text, which jsdom cannot do, so a backend tested there skips the case. The element is
    * made when it is first asked for, so that a case that cannot be made fails only itself.
    */
  final class Case(
      val label: String,
      make: () => Element,
      val tight: Boolean,
      val measuresText: Boolean
  ):
    lazy val element: Element = make()

  object Case:
    def apply(
        label: String,
        element: => Element,
        tight: Boolean,
        measuresText: Boolean = false
    ): Case = new Case(label, () => element, tight, measuresText)

  val red: Color   = Color(255, 0, 0)
  val green: Color = Color(0, 255, 0)
  val blue: Color  = Color(0, 0, 255)

  private val redFill   = Some(FillStyle(red))
  private val greenFill = Some(FillStyle(green))

  /** A diagonal gradient that lies off the middle of the shapes it fills. */
  val gradient: LinearGradient = LinearGradient(Pos(4, 2), Pos(19, 9), red, blue)

  def stroke(
      width: Double,
      cap: StrokeStyle.Cap = StrokeStyle.Cap.Square,
      join: StrokeStyle.Join = StrokeStyle.Join.Miter
  ): Option[StrokeStyle] = Some(StrokeStyle(blue, width, cap, join))

  def pattern(x: Int, y: Int): Color =
    Color((x * 37) % 256, (y * 53) % 256, ((x + y) * 11) % 256)

  /** An opaque sprite of `width` by `height` pixels of `pattern`, centered at the origin. */
  def sprite(width: Int, height: Int): Bitmap = Bitmap(width, height, pattern)

  /** A text stretched onto a 40 x 20 box around the origin. jsdom lays out no text at all.
    */
  def boxedText(content: String): Element =
    Text(
      Pos.Origin,
      Some(Bounds(-20, -10, 20, 10)),
      content,
      "SansSerif",
      10,
      400,
      redFill,
      None
    )

  /** Two rectangles side by side, red and green, which fill their bounds. */
  private def pair(width: Double, height: Double): Element =
    Picture(
      Rectangle(width / 2, height, Pos(-width / 4, 0), redFill, None),
      Rectangle(width / 2, height, Pos(width / 4, 0), greenFill, None)
    )

  private val svgSource =
    """<svg xmlns="http://www.w3.org/2000/svg" width="24" height="12">""" +
      """<rect x="0" y="0" width="24" height="12" fill="#00ff00"/></svg>"""

  /** A filled circle, for a bitmap of a picture source. */
  private def circleSource: Element = Circle(Pos(0, 0), 6, Some(FillStyle(red)), None)

  /** A shape drawn after a bitmap drawn after a shape, the last one red and in front of all the
    * rest, so that every pixel a crop of it covers is red, whatever clip the bitmap's drawing
    * leaves behind.
    */
  def shapeBitmapShape: Element =
    Picture(
      Rectangle(40, 40, Pos.Origin, redFill, None),
      sprite(4, 4),
      Rectangle(4, 4, Pos(5.3, 5.6), greenFill, None)
    )

  val cases: Seq[Case] = Seq(
    Case(
      "a shape, a bitmap and a shape under an axis-aligned crop",
      shapeBitmapShape.crop(Bounds(-10, -10, 10, 10)),
      tight = true
    ),
    Case(
      "a shape, a bitmap and a shape under a turned crop",
      shapeBitmapShape.crop(Bounds(-10, -10, 10, 10)).rotateBy(30),
      tight = true
    ),
    Case(
      "a rectangle at a fractional position",
      Rectangle(30, 20, Pos(10.3, 7.6), redFill, None),
      tight = true
    ),
    Case(
      "a stroked rectangle turned by 30 degrees",
      Rectangle(30, 20, Pos.Origin, redFill, stroke(4)).rotateBy(30),
      tight = true
    ),
    Case(
      "a gradient-filled stroked circle",
      Circle(Pos(3.5, 2.25), 12, Some(FillStyle(gradient)), stroke(3)),
      tight = true
    ),
    Case(
      "a stroked circle scaled unevenly",
      Circle(Pos.Origin, 10, None, stroke(2)).scaleBy(3, 1),
      tight = true
    ),
    Case(
      "a partial arc with round caps",
      Arc(Pos.Origin, 40, 30, 20, 250, redFill, stroke(3, StrokeStyle.Cap.Round)),
      tight = true
    ),
    Case(
      "a gradient triangle turned by -37 degrees",
      Triangle(30, Some(FillStyle(gradient)), None).rotateBy(-37),
      tight = true
    ),
    Case(
      "a sheared rectangle",
      Rectangle(20, 10, Pos.Origin, redFill, stroke(2)).rotateBy(30).scaleBy(2, 1),
      tight = true
    ),
    Case(
      "a line with round caps",
      Line(
        Pos(0, 0),
        Pos(30, 17),
        StrokeStyle(blue, 3, StrokeStyle.Cap.Round, StrokeStyle.Join.Round)
      ),
      tight = true
    ),
    Case("a point at a fractional position", Point(Pos(4.3, 5.7), red), tight = true),
    Case(
      "a point scaled and turned",
      Point(Pos(4.3, 5.7), red).scaleBy(3).rotateBy(20),
      tight = true
    ),
    Case("a boxed text", boxedText("Plan"), tight = false, measuresText = true),
    Case(
      "a mirrored boxed text",
      boxedText("Plan").flipHorizontally,
      tight = false,
      measuresText = true
    ),
    Case("a sprite at a fractional position", sprite(7, 5).moveTo(Pos(10.3, 4.6)), tight = true),
    Case("a sprite turned by 30 degrees", sprite(12, 8).rotateBy(30), tight = true),
    Case("a sprite scaled by 1.5", sprite(12, 8).scaleBy(1.5), tight = true),
    Case(
      "a sprite in a picture turned by a quarter",
      Picture(sprite(12, 8), Rectangle(4, 4, Pos(20, 0), redFill, None)).rotateBy(90),
      tight = true
    ),
    // At 1.5x, the corner snap moves this crop by half a device pixel, which took the right edge
    // of its clip half a pixel past the device pixels its bounds cover.
    Case("a sprite's own crop", sprite(12, 8).crop(Bounds(-3, -2, 4, 3)), tight = true),
    Case("a sprite's own wrap shift", sprite(12, 8).wrapShift(5, 3), tight = true),
    Case(
      "a smoothed sprite in a picture scaled by 2.5",
      Picture(
        Bitmap(12, 8, pattern).withResampling(ResamplingHint.Smooth)
      ).scaleBy(2.5),
      tight = true
    ),
    Case(
      "a bitmap of a picture scaled by 3",
      Picture(Bitmap.of(circleSource)).scaleBy(3),
      tight = false
    ),
    Case(
      "a mask",
      MaskGroup(
        Rectangle(30, 30, Pos.Origin, redFill, None),
        Circle(Pos.Origin, 20, greenFill, None)
      ),
      tight = true
    ),
    Case(
      "an inverted mask",
      MaskGroup(
        Circle(Pos.Origin, 8, greenFill, None),
        Rectangle(30, 20, Pos.Origin, redFill, None),
        inverted = true
      ),
      tight = true
    ),
    Case(
      "an axis-aligned fractional clip",
      Circle(Pos.Origin, 20, redFill, None).crop(Bounds(-10.3, -12.7, 15.2, 8.1)),
      tight = true
    ),
    Case(
      "a turned clip",
      Circle(Pos.Origin, 20, redFill, None).crop(Bounds(-10.3, -12.7, 15.2, 8.1)).rotateBy(30),
      tight = true
    ),
    Case(
      "a turned clip seen through a small crop",
      Circle(Pos.Origin, 20, redFill, None)
        .crop(Bounds(-10.3, -12.7, 15.2, 8.1))
        .rotateBy(30)
        .crop(Bounds(-4.5, -3, 6, 4.25)),
      tight = true
    ),
    Case(
      "a clip inside a mask",
      MaskGroup(
        Circle(Pos.Origin, 14, greenFill, None),
        Rectangle(40, 40, Pos.Origin, redFill, None).crop(Bounds(-10, -16, 18, 16))
      ),
      tight = false
    ),
    Case(
      "a mask inside a clip",
      MaskGroup(
        Circle(Pos.Origin, 14, greenFill, None),
        Rectangle(40, 40, Pos.Origin, redFill, None)
      ).crop(Bounds(-7.5, -20, 20, 5.5)),
      tight = false
    ),
    Case("a wrap shift by whole pixels", pair(20, 10).wrapShift(7, 3), tight = true),
    Case("a wrap shift by a fractional offset", pair(20, 10).wrapShift(3.5, 1.25), tight = true),
    Case("a wrap shift with a fractional period", pair(20.5, 10.25).wrapShift(5, 2), tight = true),
    Case("a turned wrap shift", pair(20, 10).wrapShift(5, 2).rotateBy(30), tight = true),
    Case(
      "an SVG picture scaled by 1.5",
      SvgPicture.fromString(svgSource).scaleBy(1.5),
      tight = true
    ),
    Case(
      "an animation",
      AnimatedPicture(
        Seq(pair(20, 10), Rectangle(20, 10, Pos.Origin, greenFill, None)),
        Seq(100, 100),
        startTime = 0L
      ),
      tight = true
    )
  )

  /** Sprites turned by angles from steep to shallow and moved by sixteenths of a pixel, at their
    * own size and at 0.9 of it. A turned bitmap drawn whole that fades out its own edges must not
    * ink a pixel outside the device pixels its bounds cover, at any angle or offset. Rendered at
    * 1x.
    */
  val turnedSweep: Seq[Case] =
    for
      size  <- Seq(1.0, 0.9)
      angle <- Seq(1.0, 7.0, 30.0, 45.0, 61.0, 89.0)
      shift <- Seq(0.0, 1.0 / 16, 5.0 / 16, 0.5, 13.0 / 16)
    yield Case(
      s"a sprite at $size of its size turned by $angle, moved by $shift",
      sprite(9, 7).scaleBy(size).rotateBy(angle).moveBy(shift, shift / 2),
      tight = false
    )

  /** A smoothed 12 x 8 sprite, centered at the origin, with no blue at all inside the crop
    * `smile.infrastructure.PlanRenderCases.fence` and pure blue everywhere outside it, so that any
    * blue in a rendering of the crop was sampled from outside it.
    */
  def fencedSprite: Bitmap =
    def color(x: Int, y: Int): Color =
      if x >= 3 && x < 10 && y >= 2 && y < 7 then Color(40 + 20 * x, 40 + 30 * y, 0) else blue
    Bitmap(12, 8, color).withResampling(ResamplingHint.Smooth)

  /** The crop of `smile.infrastructure.PlanRenderCases.fencedSprite` that holds no blue, which is
    * its columns 3 to 9 and rows 2 to 6.
    */
  val fence: Bounds = Bounds(-3, -2, 4, 3)

  /** The crop of the fenced sprite resampled in several ways, at the scales to render each at. */
  def fencedCrops: Seq[(String, Element, Seq[Double])] =
    val crop = fencedSprite.crop(fence)
    Seq(
      ("the crop", crop, Seq(1.0, 1.5, 2.0)),
      ("the crop at a fractional position", crop.moveBy(0.3, 0.7), Seq(1.0, 1.5)),
      ("the crop turned slightly", crop.rotateBy(3), Seq(1.0, 1.5)),
      ("the crop scaled by 1.3", crop.scaleBy(1.3), Seq(1.0))
    )

  /** `element` with blank space around it, so that ink drawn outside its bounds lands on pixels of
    * the rendering and can be seen.
    */
  def framed(element: Element): Element =
    element.bounds match
      case None      => element
      case Some(box) =>
        val margin = blank(box.width + 20, box.height + 20).moveTo(box.center)
        Picture(element, margin)

  /** What is wrong with where `element`'s ink lands in `argb`, the packed ARGB pixels of its
    * `smile.infrastructure.PlanRenderCases.framed` element rendered at `scale` with `settings`, row
    * by row: ink outside the device pixels its bounds cover, and, for a `tight` element, an edge of
    * those pixels that its ink does not reach within one device pixel. Empty when nothing is wrong.
    *
    * The bounds are grown by the platform's [[smile.testing.Rasterizer.edgeSlack]] in device
    * pixels, as `smile.testing.InkAssertions` grows them. A browser shades the pixel past a curve
    * whose extreme lies on a pixel's edge. Chromium strokes a round cap 6 device pixels wide with
    * 36 of 255 in the row of pixels above its top, where a disc of the same radius and a
    * butt-capped stroke shade none, at every scale.
    */
  def inkProblems(
      element: Element,
      scale: Double,
      argb: Array[Int],
      tight: Boolean,
      settings: RenderSettings = RenderSettings.Default
  ): Seq[String] =
    val grid       = DeviceGrid(scale)
    val (frame, _) = DrawPlan.of(framed(element), grid, settings, 0L)
    val slack      = smile.testing.Rasterizer.edgeSlack / scale
    val bounds     = element.bounds.get
    val inside     = grid.covering(
      Bounds(
        bounds.upperLeftCorner.x - slack,
        bounds.upperLeftCorner.y - slack,
        bounds.lowerRightCorner.x + slack,
        bounds.lowerRightCorner.y + slack
      )
    )
    require(
      argb.length == frame.width * frame.height,
      s"${argb.length} pixels for a frame of ${frame.width} by ${frame.height}"
    )
    var (left, top, right, bottom) = (Int.MaxValue, Int.MaxValue, Int.MinValue, Int.MinValue)
    val outside                    = Vector.newBuilder[(Int, Int)]
    for row <- 0 until frame.height; column <- 0 until frame.width do
      if (argb(row * frame.width + column) >>> 24) != 0 then
        val (x, y) = (frame.left + column, frame.top + row)
        if x < inside.left || x >= inside.left + inside.width || y < inside.top ||
          y >= inside.top + inside.height
        then outside += ((x, y))
        left = left.min(x)
        top = top.min(y)
        right = right.max(x)
        bottom = bottom.max(y)
    val strays = outside.result()
    val within =
      if strays.isEmpty then Seq()
      else Seq(s"${strays.size} inked pixels outside $inside, such as ${strays.take(5)}")
    val reach =
      if !tight then Seq()
      else if left > right then Seq("no ink at all")
      else
        Seq(
          Option.when(left > inside.left + 1)(s"the ink starts at column $left"),
          Option.when(top > inside.top + 1)(s"the ink starts at row $top"),
          Option.when(right < inside.left + inside.width - 2)(s"the ink ends at column $right"),
          Option.when(bottom < inside.top + inside.height - 2)(s"the ink ends at row $bottom")
        ).flatten.map(_ + s" of $inside")
    within ++ reach
  end inkProblems
end PlanRenderCases
