package smile.rendering

import smile.colors.LinearGradient.CycleMethod
import smile.colors.{Color, LinearGradient}
import smile.modeling.{Bounds, Pos}
import smile.pictures.*
import smile.testing.Cases.{blue, red}

/** The pieces of shared rendering policy that exist to make the backends draw one picture, found
  * and pinned down with the parity harness (`parity/`): gradient cycles written out for the canvas,
  * bitmaps snapped to whole pixels, and masks laid on the pixel grid.
  */
class BackendAgreementSuite extends munit.FunSuite:

  /** `element` rendered on the pixels of `frame`, a box of whole pixels. */
  private def renderedIn(frame: Bounds, element: Element): Bitmap = element.crop(frame).toBitmap()

  /** The world corner of the box around the pixels that `element`, rendered on `frame`, paints. */
  private def inkCorner(frame: Bounds, element: Element): Pos =
    val rendered = renderedIn(frame, element)
    val pixels   = for
      y <- 0 until rendered.pixels().height
      x <- 0 until rendered.pixels().width
      if rendered.getColor(x, y).opacity > 0
    yield (x, y)
    Pos(
      frame.upperLeftCorner.x + pixels.map(_._1).min,
      frame.upperLeftCorner.y + pixels.map(_._2).min
    )

  private def gradient(cycleMethod: CycleMethod) =
    LinearGradient(Pos(0, 0), Pos(10, 0), red, blue, cycleMethod)

  private val area = Seq(Pos(-15, 0), Pos(22, 0), Pos(-15, 5), Pos(22, 5))

  test("a gradient without cycles is written out as it is") {
    val (start, end, stops) =
      gradient(CycleMethod.NoCycle).writtenOutOver(Pos(0, 0), Pos(10, 0), area).get
    assertEquals((start, end), (Pos(0, 0), Pos(10, 0)))
    assertEquals(stops, Seq(0.0 -> red, 1.0 -> blue))
  }

  test("a repeating gradient is written out cycle by cycle across the area") {
    // The area reaches from 1.5 gradient lengths before the start to 2.2 after it, so five cycles.
    val (start, end, stops) =
      gradient(CycleMethod.Repeat).writtenOutOver(Pos(0, 0), Pos(10, 0), area).get
    assertEquals((start, end), (Pos(-20, 0), Pos(30, 0)))
    assertEquals(stops.map(_._2), Seq.fill(5)(Seq(red, blue)).flatten)
    assertEquals(stops.map(_._1), Seq(0.0, 0.2, 0.2, 0.4, 0.4, 0.6, 0.6, 0.8, 0.8, 1.0))
  }

  test("a reflecting gradient mirrors every other cycle, counted from its own start") {
    val (start, end, stops) =
      gradient(CycleMethod.Reflect).writtenOutOver(Pos(0, 0), Pos(10, 0), area).get
    assertEquals((start, end), (Pos(-20, 0), Pos(30, 0)))
    // The cycle from the start to the end runs red to blue. Its neighbors run back.
    assertEquals(
      stops.map(_._2),
      Seq(red, blue, blue, red, red, blue, blue, red, red, blue)
    )
  }

  test("a gradient cycling too often to write out is left to its average color") {
    val fine = LinearGradient(Pos(0, 0), Pos(0.001, 0), red, blue, CycleMethod.Repeat)
    assertEquals(fine.writtenOutOver(Pos(0, 0), Pos(0.001, 0), area), None)
  }

  test("cycling gradients paint their cycles on this platform") {
    // Red to blue between x = 10 and x = 30 of a rectangle 60 wide, then by the cycle method.
    def colorAt(cycleMethod: CycleMethod, x: Int): Color =
      val paint = LinearGradient(Pos(10, 0), Pos(30, 0), red, blue, cycleMethod)
      Rectangle(60, 20, Pos(30, 10), fillWith(paint), None).toBitmap().getColor(x, 10)
    def assertNear(obtained: Color, expected: Color, clue: String): Unit =
      assert(
        (obtained.red - expected.red).abs <= 12 && (obtained.blue - expected.blue).abs <= 12,
        s"$clue: $obtained, expected about $expected"
      )

    assertNear(colorAt(CycleMethod.NoCycle, 35), blue, "past the end, without cycles")
    assertNear(colorAt(CycleMethod.Repeat, 35), Color(191, 0, 64), "a quarter into a repeat")
    assertNear(colorAt(CycleMethod.Repeat, 5), Color(64, 0, 191), "a quarter before the start")
    assertNear(colorAt(CycleMethod.Reflect, 35), Color(64, 0, 191), "a quarter into a reflection")
    assertNear(colorAt(CycleMethod.Reflect, 5), Color(191, 0, 64), "a quarter before the start")
  }

  test("a bitmap is blitted at the whole pixel nearest to its corner") {
    val bitmap                              = Bitmap(2, 2, Seq(red, blue, blue, red))
    def cornerAt(x: Double, y: Double): Pos =
      // The bitmap is centered on the origin, its corner at (-1, -1).
      inkCorner(Bounds(-8, -8, 8, 8), bitmap.moveBy(x + 1, y + 1))

    assertEquals(cornerAt(3, 4), Pos(3, 4))
    assertEquals(cornerAt(3.4, 4.6), Pos(3, 5))
    assertEquals(cornerAt(3.5, -0.5), Pos(4, 0))
    assertEquals(cornerAt(-1.25, -1.75), Pos(-1, -2))
  }

  test("a bitmap between pixels keeps its pixels") {
    // The bitmap is centered on the origin, its corner at (-1, -1).
    val bitmap   = Bitmap(2, 2, Seq(red, blue, blue, red)).moveBy(3.5, 2.25)
    val rendered = renderedIn(Bounds(Pos(0, 0), Pos(6, 6)), bitmap)

    assertEquals(rendered.getColor(3, 1), red)
    assertEquals(rendered.getColor(4, 1), blue)
    assertEquals(rendered.getColor(3, 2), blue)
    assertEquals(rendered.getColor(4, 2), red)
    assertEquals(rendered.getColor(2, 1).opacity, 0)
    assertEquals(rendered.getColor(5, 2).opacity, 0)
  }

  test("a bitmap is scaled smoothly by default, on this platform as on the other") {
    // Red beside blue, four times as wide. Interpolated, the pixels in between are mixtures.
    // Smooth scaling that averages areas enlarges into plain blocks, and fails this.
    val smooth = Bitmap(2, 1, Seq(red, blue)).scaleBy(4, 1).asBitmap
    assertEquals((smooth.pixels().width, smooth.pixels().height), (8, 1))
    assert(smooth.getColor(0, 0).red > 200, s"the left end stays red: ${smooth.getColor(0, 0)}")
    assert(smooth.getColor(7, 0).blue > 200, s"the right end stays blue: ${smooth.getColor(7, 0)}")
    val mixtures = (0 until 8)
      .map(smooth.getColor(_, 0))
      .count: color =>
        color.red > 24 && color.blue > 24
    assert(mixtures >= 2, s"expected mixed pixels in between, found $mixtures")

    // Nearest-neighbor scaling, when the output's settings ask for it, keeps the blocks.
    val blocks = Bitmap(2, 1, Seq(red, blue))
      .scaleBy(4, 1)
      .asBitmap
      .pixels(
        smile.RenderSettings(
          bufferScalingMethod = smile.ScalingMethod.NearestNeighbor,
          resamplingMode = smile.ResamplingMode.Stepwise
        )
      )
      .colors
    assertEquals(blocks, Seq.fill(4)(red) ++ Seq.fill(4)(blue))
  }

  test("the scaling method also decides how a bitmap is resampled when it is drawn") {
    // A checker of four pixels, drawn four times as large. The renderer resamples it as it
    // draws, by the method in the settings it is given, as when it scales a buffer.
    val green   = Color(0, 255, 0)
    val black   = Color(0, 0, 0)
    val checker = Bitmap(2, 2, Seq(red, blue, green, black)).moveBy(1, 1)
    def drawn(method: smile.ScalingMethod): Bitmap =
      smile.infrastructure
        .Renderer()
        .createBitmapFrom(checker, 4.0, smile.RenderSettings(bufferScalingMethod = method))
    def colors(bitmap: Bitmap) = for y <- 0 until 8; x <- 0 until 8 yield bitmap.getColor(x, y)
    val blocks                 =
      for y <- 0 until 8; x <- 0 until 8
      yield Seq(red, blue, green, black)(y / 4 * 2 + x / 4)

    // Nearest neighbor and fast give four blocks with hard edges, every pixel one of the four.
    assertEquals(colors(drawn(smile.ScalingMethod.NearestNeighbor)), blocks)
    assertEquals(colors(drawn(smile.ScalingMethod.Fast)), blocks)

    // With smooth, the default, the pixels between two blocks are mixtures of them.
    for method <- Seq(smile.ScalingMethod.Smooth) do
      val smooth = drawn(method)
      assertEquals((smooth.pixels().width, smooth.pixels().height), (8, 8))
      val mixtures = colors(smooth).count(color => !Seq(red, blue, green, black).contains(color))
      assert(mixtures >= 8, s"$method: expected mixed pixels between the blocks, found $mixtures")
      assert(smooth.getColor(0, 0).red > 200 && smooth.getColor(7, 0).blue > 200, method.toString)
    assertEquals(
      colors(smile.infrastructure.Renderer().createBitmapFrom(checker, 4.0)),
      colors(drawn(smile.ScalingMethod.Smooth)),
      "smooth is what a program that never touches the setting gets"
    )
  }

  test("a bitmap at whole pixels and its own size is copied pixel for pixel") {
    val colors   = Seq(red, blue, Color(0, 255, 0), Color(10, 20, 30, 128))
    val bitmap   = Bitmap(2, 2, colors).moveBy(3, 2)
    val rendered = renderedIn(Bounds(Pos(0, 0), Pos(6, 6)), bitmap)
    assertEquals(
      for y <- 1 to 2; x <- 2 to 3 yield rendered.getColor(x, y),
      colors.take(3) :+ rendered.getColor(3, 2)
    )
    assertEquals(rendered.getColor(3, 2).opacity, 128)
  }

  test("a mask between pixels keeps its edges where they are") {
    // The mask reaches from 2.5 to 7.5, which covers half of the pixels 2 and 7 on each axis.
    val group = MaskGroup(
      Rectangle(5, 5, Pos(5, 5), fillWith(red), None),
      Rectangle(20, 20, Pos(5, 5), fillWith(blue), None)
    )
    val rendered = renderedIn(Bounds(Pos(0, 0), Pos(10, 10)), group)

    assertEquals(rendered.getColor(5, 5), blue)
    for (x, y) <- Seq((2, 5), (7, 5), (5, 2), (5, 7)) do
      val opacity = rendered.getColor(x, y).opacity
      assert(opacity > 96 && opacity < 160, s"half-covered pixel ($x, $y): $opacity")
    assertEquals(rendered.getColor(1, 5).opacity, 0)
    assertEquals(rendered.getColor(8, 5).opacity, 0)
  }

  /** A smooth source scaled, turned, flipped and cropped. Drawn directly, it has to be what
    * resampling its source once with the composed map gives, which is what every backend's bilinear
    * filter computes. Edges, where the backends antialias their clips and the image's own borders
    * each their own way, are left out.
    */
  private val directRaster: Bitmap =
    val source = Bitmap.generateARGB(
      16,
      12,
      (x, y) => 0xff000000 | (x * 16 << 16) | (y * 20 << 8) | ((x + y) * 9)
    )
    val turned = source.scaleBy(1.6, 1.6) match
      case bitmap: Bitmap => bitmap.rotateAround(23, source.anchor + (2.0, 1.0)).flipHorizontally
      case other          => fail(s"a bitmap scaled by its own operation stays one, not $other")
    turned

  /** Whole pixels of the turned buffer, well inside the turned image. The turned buffer is not one
    * pixel per logical pixel, so the crop is the generic one, a clip over the bitmap.
    */
  private val directCrop: Bounds =
    val corner = directRaster.source.boundary.upperLeftCorner
    Bounds(corner.x + 7, corner.y + 7, corner.x + 24, corner.y + 20)

  /** The color a bilinear filter gives at the point `(u, v)` of the source, in its pixels. */
  private def bilinear(source: smile.infrastructure.NativeBuffer, u: Double, v: Double) =
    val x                    = u - 0.5
    val y                    = v - 0.5
    val x0                   = x.floor.toInt
    val y0                   = y.floor.toInt
    val fx                   = x - x0
    val fy                   = y - y0
    def at(px: Int, py: Int) =
      source.pixelColor(px.max(0).min(source.width - 1), py.max(0).min(source.height - 1))
    def mix(channel: Color => Int): Double =
      (1 - fy) * ((1 - fx) * channel(at(x0, y0)) + fx * channel(at(x0 + 1, y0))) +
        fy * ((1 - fx) * channel(at(x0, y0 + 1)) + fx * channel(at(x0 + 1, y0 + 1)))
    (mix(_.red), mix(_.green), mix(_.blue))

  /** How far `point` is inside the convex polygon `corners`, negative outside. */
  private def depthInside(point: Pos, corners: Vector[Pos]): Double =
    val signed = corners.indices.map: index =>
      val from   = corners(index)
      val to     = corners((index + 1) % corners.length)
      val length = math.hypot(to.x - from.x, to.y - from.y)
      ((to.x - from.x) * (point.y - from.y) - (to.y - from.y) * (point.x - from.x)) / length
    // The corners may run either way around.
    if signed.sum >= 0 then signed.min else -signed.max

  for scale <- Seq(1.0, 2.0) do
    test(s"a scaled, turned, flipped and cropped bitmap is resampled once at ${scale}x") {
      // The bitmap's own operations leave its affine the identity, so its source's plan lays its
      // pixels in the world.
      assertEquals(directRaster.affine, smile.modeling.Affine.Identity)
      val tile   = directRaster.source.plan.tiles.head
      val source = directRaster.source.sourcePixels(smile.RenderSettings.Default, 0L)
      val frame  = Bounds(Pos(-20, -20), Pos(20, 20))
      val drawn  = smile.infrastructure
        .Renderer()
        .createBitmapFrom(directRaster.crop(directCrop).crop(frame), scale)
      val cropCorners = Vector(
        directCrop.upperLeftCorner,
        Pos(directCrop.lowerRightCorner.x, directCrop.upperLeftCorner.y),
        directCrop.lowerRightCorner,
        Pos(directCrop.upperLeftCorner.x, directCrop.lowerRightCorner.y)
      )
      val inverse = invert(tile.toWorld)

      var compared = 0
      for
        y <- 0 until drawn.pixels().height
        x <- 0 until drawn.pixels().width
      do
        val world = Pos(
          frame.upperLeftCorner.x + (x + 0.5) / scale,
          frame.upperLeftCorner.y + (y + 0.5) / scale
        )
        val inSource      = inverse(world)
        val awayFromEdges =
          depthInside(world, cropCorners) > 1.5 &&
            tile.clip.forall(depthInside(world, _) > 1.5) &&
            inSource.x > 1.5 && inSource.y > 1.5 &&
            inSource.x < source.width - 1.5 && inSource.y < source.height - 1.5
        if awayFromEdges then
          compared += 1
          val (red, green, blue) = bilinear(source, inSource.x, inSource.y)
          val color              = drawn.getColor(x, y)
          assert(
            (color.red - red).abs <= 12 && (color.green - green).abs <= 12 &&
              (color.blue - blue).abs <= 12 && color.opacity == 255,
            s"pixel ($x, $y): $color, expected about ($red, $green, $blue)"
          )
      assert(compared > 100 * scale * scale, s"only $compared pixels compared")
    }

  private def invert(map: smile.modeling.Affine): smile.modeling.Affine =
    val determinant = map.a * map.d - map.b * map.c
    val a           = map.d / determinant
    val b           = -map.b / determinant
    val c           = -map.c / determinant
    val d           = map.a / determinant
    smile.modeling.Affine(a, b, c, d, -(a * map.e + c * map.f), -(b * map.e + d * map.f))
end BackendAgreementSuite
