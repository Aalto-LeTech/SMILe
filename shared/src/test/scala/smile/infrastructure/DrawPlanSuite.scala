package smile.infrastructure

import smile.{RenderSettings, ResamplingMode}
import smile.colors.Color
import smile.infrastructure.PlanListing.listed
import smile.modeling.{Affine, Bounds, Pos}
import smile.modeling.{TextLayout, TextShape}
import smile.pictures.*
import smile.pictures.{FillStyle, ResamplingHint, StrokeStyle}

/** The shared draw plan: what it snaps to device pixels, when it clips hard and when it needs a
  * layer, how it draws wraps, points, masks, text, animations and bitmaps, and the paints the
  * traversal finds and maps.
  */
class DrawPlanSuite extends munit.FunSuite:

  private val settings = RenderSettings.Default

  private def isWhole(v: Double): Boolean = math.abs(v - math.rint(v)) < 1e-9

  private def pattern(x: Int, y: Int): Color =
    Color((x * 37) % 256, (y * 53) % 256, ((x + y) * 11) % 256)

  private def sprite(width: Int, height: Int): Bitmap =
    Bitmap(width, height, pattern)

  private def plan(e: Element, scale: Double, time: Long = 0L): (DeviceBox, Vector[DrawOp]) =
    DrawPlan.of(e, DeviceGrid(scale), settings, time)

  /** Every operation in `ops`, nested ones included, outermost first. */
  private def flatten(ops: Vector[DrawOp]): Vector[DrawOp] =
    ops.flatMap: op =>
      op +: (op match
        case layer: DrawOp.Layer       => flatten(DrawOp.parts(layer).flatten)
        case DrawOp.HardClip(_, inner) => flatten(inner)
        case DrawOp.Tiles(tile, _, _)  => flatten(Vector(tile))
        case _                         => Vector())

  private def images(ops: Vector[DrawOp]): Vector[DrawOp.Image] =
    flatten(ops).collect { case image: DrawOp.Image => image }

  private def paths(ops: Vector[DrawOp]): Vector[DrawOp.Path] =
    flatten(ops).collect { case path: DrawOp.Path => path }

  private def layers(ops: Vector[DrawOp]): Vector[DrawOp.Layer] =
    flatten(ops).collect { case layer: DrawOp.Layer => layer }

  private def hardClips(ops: Vector[DrawOp]): Vector[DrawOp.HardClip] =
    flatten(ops).collect { case clip: DrawOp.HardClip => clip }

  /** The device corner of `image`, where its affine takes the upper left of its pixels. */
  private def corner(image: DrawOp.Image): Pos =
    val box = image.m.boxBounds(0, 0, 7, 7)
    box.upperLeftCorner

  private def assertBoxNear(actual: Bounds, expected: Bounds): Unit =
    for (a, e) <- Seq(
        actual.upperLeftCorner.x  -> expected.upperLeftCorner.x,
        actual.upperLeftCorner.y  -> expected.upperLeftCorner.y,
        actual.lowerRightCorner.x -> expected.lowerRightCorner.x,
        actual.lowerRightCorner.y -> expected.lowerRightCorner.y
      )
    do assertEqualsDouble(a, e, 1e-6, s"$actual is not $expected")

  private val red   = Some(FillStyle(Color(255, 0, 0)))
  private val green = Some(FillStyle(Color(0, 255, 0)))

  test("an odd 7x7 sprite centered at (10, 10) lands on a whole device corner at 1x") {
    val (_, ops) = plan(sprite(7, 7).moveTo(Pos(10, 10)), 1.0)
    val drawn    = images(ops)
    assertEquals(drawn.size, 1)
    val at = corner(drawn.head)
    assert(drawn.head.snapped)
    assert(isWhole(at.x) && isWhole(at.y), s"corner $at")
  }

  test("a resampled sprite is drawn at its exact place, never snapped") {
    for scale <- Seq(1.5, 2.0) do
      val drawn = images(plan(sprite(7, 7).moveTo(Pos(10.3, 4.6)), scale)._2)
      assertEquals(drawn.size, 1)
      assert(!drawn.head.snapped, s"at $scale")
      val at = corner(drawn.head)
      assertEqualsDouble(at.x, (10.3 - 3.5) * scale, 1e-6)
      assertEqualsDouble(at.y, (4.6 - 3.5) * scale, 1e-6)
  }

  test("a 1x sprite is snapped and drawn 1:1 at scale 1") {
    val drawn = images(plan(sprite(7, 7).moveTo(Pos(10.3, 4.6)), 1.0)._2)
    assertEquals(drawn.size, 1)
    assert(drawn.head.snapped)
    val at = corner(drawn.head)
    assert(isWhole(at.x) && isWhole(at.y), s"corner $at")
  }

  test("a 1x sprite at scale 1.5 is not snapped and keeps its resampling hint") {
    val smooth = Bitmap(7, 7, pattern).withResampling(ResamplingHint.Smooth)
    val drawn  = images(plan(smooth.moveTo(Pos(10, 10)), 1.5)._2)
    assertEquals(drawn.size, 1)
    assert(!drawn.head.snapped)
    assertEquals(drawn.head.interpolation, ResamplingHint.Smooth)
    assertEquals(drawn.head.m.a, 1.5)
  }

  test("a placed sprite clipped to a background is a hard clip, never a layer") {
    val background = Rectangle(100, 60, Pos(50, 30), green, None)
    val scene      = sprite(7, 7).moveTo(Pos(10.3, 20.7)).addToFront(background)
    for scale <- Seq(1.0, 1.5, 2.0) do
      val (_, ops) = plan(scene.crop(Bounds(0.25, 0.5, 80.4, 50.2)), scale)
      assert(layers(ops).isEmpty, s"at $scale: $ops")
      assertEquals(hardClips(ops).size, 1)
      assertEquals(images(ops).size, 1)
  }

  test("an axis-aligned clip covers the device pixels of its rectangle") {
    val clipped  = Rectangle(10, 10, red, None).crop(Bounds(0.25, 0.5, 3.4, 4.2))
    val (_, ops) = plan(clipped, 1.5)
    assertEquals(hardClips(ops).map(_.box), Vector(DeviceBox(0, 0, 6, 7)))
  }

  test("a clip rotated by 30 degrees is drawn through a layer with its clip shape") {
    val clipped  = Rectangle(10, 10, red, None).crop(Bounds(-3, -3, 3, 3)).rotateBy(30)
    val (_, ops) = plan(clipped, 1.0)
    val found    = layers(ops)
    assertEquals(found.size, 1)
    assert(hardClips(ops).isEmpty)
    val shape = found.head.clip.getOrElse(fail("no clip shape"))
    assertEquals(shape.rect, Bounds(-3, -3, 3, 3))
    assert(!shape.m.keepsAxes)
    assertEquals(paths(found.head.ops).size, 1)
  }

  test("the output box of a 5x5 picture at 1.5 is 8x8") {
    val (box, _) = plan(Rectangle(5, 5, Pos(2.5, 2.5), red, None), 1.5)
    assertEquals(box, DeviceBox(0, 0, 8, 8))
  }

  test("the empty picture plans a 0x0 box and no operations") {
    assertEquals(plan(Empty, 1.5), (DeviceBox(0, 0, 0, 0), Vector()))
  }

  test("a wrap at a fractional offset with a whole period draws four copies on whole offsets") {
    val content = Rectangle(100, 50, Pos(50, 25), red, None)
    val shifted = content.wrapShift(10.3, 5.6)
    for scale <- Seq(1.0, 1.5, 2.0) do
      val (_, ops) = plan(shifted, scale)
      val base     = paths(plan(content, scale)._2).head.m
      val copies   = paths(ops)
      assertEquals(copies.size, 4, s"at $scale")
      assertEquals(hardClips(ops).size, 1)
      assert(layers(ops).isEmpty)
      assertEquals(hardClips(ops).head.box, DeviceGrid(scale).covering(Bounds(0, 0, 100, 50)))
      for copy <- copies do
        assert(isWhole(copy.m.e - base.e) && isWhole(copy.m.f - base.f), s"$copy at $scale")
      // The copies stand exactly one device period apart on each axis.
      val xs = copies.map(_.m.e).distinct.sorted
      val ys = copies.map(_.m.f).distinct.sorted
      assertEquals(xs.size, 2)
      assertEquals(ys.size, 2)
      assertEquals(xs(1) - xs(0), 100 * scale)
      assertEquals(ys(1) - ys(0), 50 * scale)
  }

  test("a polygon of one point draws nothing, its stroke's caps included") {
    val dot = Polygon(Pos(2, 1), Seq(Pos(0.5, 0.5)), red, Some(StrokeStyle(Color(0, 0, 255), 3)))
    assertEquals(plan(dot, 1.0)._2, Vector())
  }

  test("a whole period whose edges fall between device pixels is a tile, so no seam shows") {
    // 15.5 pixels from -7.75 is 31 device pixels at 2x, from -15.5. Copies of content that fills
    // the period would meet at a half-covered pixel each, drawn over each other.
    val content  = Rectangle(15.5, 6, Pos(0, 0), red, None)
    val (_, ops) = plan(content.wrapShift(4, 0), 2.0)
    val tiles    = flatten(ops).collect { case tiles: DrawOp.Tiles => tiles }
    assertEquals(tiles.size, 1)
    assertEquals(tiles.head.tile.box, DeviceBox(0, 0, 31, 12))
    assertBoxNear(Support.box(paths(tiles.head.tile.ops).head), Bounds(0, 0, 31, 12))
  }

  test("a wrapped sprite at a fractional offset draws four snapped images") {
    val shifted  = sprite(20, 10).moveTo(Pos(10, 5)).wrapShift(3.3, 2.5)
    val (_, ops) = plan(shifted, 1.5)
    val drawn    = images(ops)
    assertEquals(drawn.size, 4)
    for image <- drawn do
      val at = image.m.boxBounds(0, 0, 20, 10).upperLeftCorner
      assert(isWhole(at.x) && isWhole(at.y), s"$at")
  }

  for scale <- Seq(1.0, 1.5, 2.0) do
    test(s"a wrap with a period of 100.3 at $scale is one whole-pixel tile, copies abutting") {
      val content  = Rectangle(100.3, 50.2, Pos(50.15, 25.1), red, None)
      val (_, ops) = plan(content.wrapShift(10.4, 5.3), scale)
      val tiles    = flatten(ops).collect { case tiles: DrawOp.Tiles => tiles }
      assertEquals(tiles.size, 1)
      val (tile, copies)  = (tiles.head.tile, tiles.head.copies)
      val (width, height) = (math.rint(100.3 * scale).toInt, math.rint(50.2 * scale).toInt)
      assertEquals(tile.box, DeviceBox(0, 0, width, height))
      assertEquals(paths(tile.ops).size, 1)
      // The content fills exactly the tile.
      assertBoxNear(Support.box(paths(tile.ops).head), Bounds(0, 0, width, height))
      val clip = hardClips(ops).map(_.box)
      assertEquals(clip, Vector(DeviceGrid(scale).covering(content.bounds.get)))
      // Every copy only moves, by whole device pixels.
      for copy <- copies do
        assert(copy.a == 1 && copy.b == 0 && copy.c == 0 && copy.d == 1, s"$copy")
        assert(isWhole(copy.e) && isWhole(copy.f), s"$copy")
      val xs = copies.map(_.e).distinct.sorted
      val ys = copies.map(_.f).distinct.sorted
      for (a, b) <- xs.zip(xs.tail) do assertEquals(b - a, width.toDouble)
      for (a, b) <- ys.zip(ys.tail) do assertEquals(b - a, height.toDouble)
      assertEquals(copies.size, xs.size * ys.size)
      assertEquals(copies.distinct.size, copies.size)
      // The copies meet edge to edge and cover the clip. Each copy intersects it.
      val box = clip.head
      assert(xs.head <= box.left && xs.last + width >= box.left + box.width, s"$xs over $box")
      assert(ys.head <= box.top && ys.last + height >= box.top + box.height, s"$ys over $box")
      assert(xs.head + width > box.left && xs.last < box.left + box.width)
      assert(ys.head + height > box.top && ys.last < box.top + box.height)
    }

  test("a wrap's content is folded only when the fold asks for it") {
    // A wrap's content is folded once, under the map the fold asks for, so a fractional wrap's
    // tile plans it in the tile's own frame and nested wraps plan their content once per level.
    var shapes  = 0
    val ignored = new Traversal.Fold[Unit]:
      def empty: Unit                                                                  = ()
      def shape(shape: Shape, m: Affine): Unit                                         = shapes += 1
      def bitmap(bitmap: Bitmap, m: Affine): Unit                                      = ()
      def svg(svg: SvgPicture, m: Affine): Unit                                        = ()
      def picture(picture: Picture, m: Affine, parts: Vector[Unit]): Unit              = ()
      def clip(clip: Clip, m: Affine, content: Unit): Unit                             = ()
      def foldsMasks: Boolean                                                          = false
      def mask(group: MaskGroup, m: Affine, masks: Vector[Unit], masked: Unit): Unit   = ()
      def animation(animation: AnimatedPicture, m: Affine, frames: Vector[Unit]): Unit = ()
      override def wrapContent(wrap: Wrap, m: Affine): Option[Affine]                  = None
      def wrap(wrap: Wrap, m: Affine, content: Option[Unit]): Unit                     = ()
    Traversal.fold(Rectangle(100.3, 50, Pos.Origin, red, None).wrapShift(10.4, 5), ignored)
    assertEquals(shapes, 0)
  }

  test("a turned wrap with a fractional period steps its tile by whole tiles in its frame") {
    val content  = Rectangle(100.3, 50, Pos(50.15, 25), red, None)
    val (_, ops) = plan(content.wrapShift(10.4, 5).rotateBy(30), 1.0)
    val tiles    = flatten(ops).collect { case tiles: DrawOp.Tiles => tiles }
    assertEquals(tiles.size, 1)
    assertEquals(tiles.head.tile.box, DeviceBox(0, 0, 100, 50))
    assertEquals(ops.collect { case layer: DrawOp.Layer => layer.clip.isDefined }, Vector(true))
    val copies = tiles.head.copies
    assertEquals(copies.size, 4)
    // Neighboring copies stand a side of the period apart, turned with the wrap.
    val origins = copies.map(c => c(Pos(0, 0)))
    val steps   = for a <- origins; b <- origins if a != b yield math.hypot(b.x - a.x, b.y - a.y)
    assert(steps.exists(d => math.abs(d - 100.3) < 1e-6), s"$steps")
    assert(steps.exists(d => math.abs(d - 50) < 1e-6), s"$steps")
  }

  test("a point that only moves is a pixel at its snapped position") {
    val (_, ops) = plan(Point(Pos(3.4, 7.6), Color(1, 2, 3)), 1.0)
    assertEquals(ops, Vector(DrawOp.Pixel(3, 7, Color(1, 2, 3))))
  }

  test("a point scaled by 3 is the path of its 3x3 square") {
    val (_, ops) = plan(Point(Pos(10, 10), Color(1, 2, 3)).scaleBy(3), 1.0)
    val drawn    = paths(ops)
    assertEquals(drawn.size, 1)
    // The corner 8.5 snaps to 9, as a bitmap's corner does.
    assertEquals(Support.box(drawn.head), Bounds(9, 9, 12, 12))
  }

  test("a point at scale 2 is a crisp 2x2 square on whole device pixels") {
    val (_, ops) = plan(Point(Pos(10.3, 4.1), Color(1, 2, 3)), 2.0)
    assertEquals(paths(ops).map(Support.box), Vector(Bounds(20, 7, 22, 9)))
  }

  test("a point at scale 1.5 is a 1.5-pixel square from a whole device corner") {
    val (_, ops) = plan(Point(Pos(10, 10), Color(1, 2, 3)), 1.5)
    // The corner 9.5 * 1.5 = 14.25 snaps to 14.
    assertEquals(paths(ops).map(Support.box), Vector(Bounds(14, 14, 15.5, 15.5)))
  }

  test("a plan rounds to device pixels where it puts something on whole device pixels") {
    def rounds(e: Element, scale: Double): Boolean =
      DrawPlan.roundsToDevicePixels(plan(e, scale)._2)
    val point  = Point(Pos(3.7, 2.2), Color(1, 2, 3))
    val square = Rectangle(10, 10, Pos(0, 0), red, None)
    val window = Bounds(-3, -3, 3, 3)
    for (element, scale, clue) <- Seq(
        (point, 1.0, "a point that only moves, a pixel"),
        (point, 2.0, "a point whose square keeps the axes, from a snapped corner"),
        (square.crop(window), 2.0, "a hard clip"),
        (MaskGroup(Circle(Pos(0, 0), 4, red, None), square), 2.0, "a mask's layer"),
        (square.crop(window).rotateBy(30), 2.0, "a turned clip's layer"),
        (sprite(7, 7), 1.0, "a snapped image"),
        (sprite(7, 7), 1.5, "an image clipped to the device pixels its centers cover")
      )
    do assert(rounds(element, scale), clue)
    for (element, scale, clue) <- Seq(
        (square, 2.0, "a path"),
        (point.rotateBy(30), 2.0, "a turned point"),
        (sprite(7, 7).rotateBy(30), 2.0, "a turned sprite"),
        (sprite(8, 8), 2.0, "an image on whole device pixels with no clip"),
        (
          SvgPicture.fromString(
            """<svg xmlns="http://www.w3.org/2000/svg" width="4" height="4"/>"""
          ),
          2.0,
          "an SVG document"
        )
      )
    do assert(!rounds(element, scale), clue)
    // Tiles are drawn on whole device pixels whatever they hold.
    val tile = new DrawOp.Layer(DeviceBox(0, 0, 3, 3), None, Vector(), Vector())
    assert(DrawPlan.roundsToDevicePixels(Vector(DrawOp.Tiles(tile, Vector(Affine.Identity)))))
  }

  test("a plan for SVG tiles every wrap, and keeps the vectors of a whole-pixel one") {
    def tiled(e: Element, svg: Boolean) =
      val (_, ops) = DrawPlan.of(e, DeviceGrid(1), settings, 0L, svg)
      flatten(ops).collect { case tiles: DrawOp.Tiles => tiles }
    val whole      = Rectangle(100, 40, Pos(50, 20), red, None).wrapShift(37, 23)
    val fractional = Rectangle(100.5, 40, Pos(50.25, 20), red, None).wrapShift(37, 23)
    // On a canvas a whole-pixel wrap is up to four copies of its content and no tile.
    assertEquals(tiled(whole, svg = false), Vector())
    val kept = tiled(whole, svg = true)
    assertEquals(kept.map(_.vector), Vector(true))
    assertEquals(kept.head.tile.box, DeviceBox(0, 0, 100, 40))
    for copy <- kept.head.copies do
      assertEquals((copy.a, copy.b, copy.c, copy.d), (1.0, 0.0, 0.0, 1.0))
      assertEquals(math.floorMod(copy.e.toInt, 100), 37)
      assertEquals(math.floorMod(copy.f.toInt, 40), 23)
    // A period that is not whole pixels is a raster tile under either plan, and the same one.
    assertEquals(tiled(fractional, svg = true).map(_.vector), Vector(false))
    assertEquals(tiled(fractional, svg = true), tiled(fractional, svg = false))
  }

  test("a picture is drawn back to front, its first child last") {
    val front    = Rectangle(10, 10, red, None)
    val back     = Rectangle(20, 20, green, None)
    val (_, ops) = plan(back.addToFront(front), 1.0)
    assertEquals(paths(ops).map(_.fill), Vector(green, red))
  }

  test("a mask is a layer over the group's device box with its inversion") {
    val masked   = Rectangle(20, 20, red, None)
    val mask     = Circle(Pos.Origin, 10, green, None)
    val (_, ops) = plan(MaskGroup(mask, masked, inverted = true), 1.5)
    assertEquals(ops.size, 1)
    ops.head match
      case DrawOp.Layer(box, clip, Vector((maskOps, inverted)), inner) =>
        assertEquals(box, DeviceBox(-15, -15, 30, 30))
        assertEquals(clip, None)
        assert(inverted)
        assertEquals(paths(maskOps).size, 1)
        assertEquals(paths(inner).size, 1)
      case other => fail(s"not a mask layer: $other")
  }

  test("a mask of a moved masked group is one layer of both masks, in order") {
    val masked       = Rectangle(20, 20, red, None)
    val (hole, spot) = (Circle(Pos.Origin, 4, green, None), Circle(Pos.Origin, 8, green, None))
    val inner        = MaskGroup(hole, masked, inverted = true).moveBy(2, 0)
    val (_, ops)     = plan(MaskGroup(spot, inner), 1.0)
    ops match
      case Vector(DrawOp.Layer(box, None, Vector((first, true), (second, false)), content)) =>
        assertEquals(box, DeviceBox(-8, -8, 16, 16))
        // The hole and the content moved with the group they were in, the spot as it is.
        assertEquals(paths(first).map(_.m), Vector(Affine.translation(2, 0)))
        assertEquals(paths(second).map(_.m), Vector(Affine.Identity))
        assertEquals(paths(content).map(_.m), Vector(Affine.translation(2, 0)))
      case other => fail(s"not one layer of two masks: $other")
  }

  test("text carries its layout under the mirrored affine") {
    // Mirrored by hand and seen through a crop, so that nothing measures the text. jsdom cannot.
    val layout   = TextLayout("Hi", "Serif", 12, 400)
    val mirrored = new Shape(TextShape(layout), red, None, Affine(-1, 0, 0, 1, 10, 10), Pos.Origin)
    val (_, ops) = plan(mirrored.crop(Bounds(0, 0, 20, 20)), 2.0)
    ops match
      case Vector(DrawOp.HardClip(_, Vector(DrawOp.Text(m, layout, fill, _)))) =>
        assertEquals(layout, TextLayout("Hi", "Serif", 12, 400))
        assert(m.determinant < 0)
        assertEquals(fill, red)
      case other => fail(s"not one text: $other")
  }

  test("an animation plans the frame on display at the given time") {
    val frames   = Seq(Rectangle(10, 10, red, None), Rectangle(10, 10, green, None))
    val animated = AnimatedPicture(frames, Seq(100, 100), startTime = 1000)
    assertEquals(paths(plan(animated, 1.0, 1050)._2).map(_.fill), Vector(red))
    assertEquals(paths(plan(animated, 1.0, 1150)._2).map(_.fill), Vector(green))
  }

  test("a bitmap plans on its own at scale 1, under a transform that only composes") {
    val (box, ops) = plan(sprite(7, 7).moveTo(Pos(3.5, 3.5)), 1.0)
    assertEquals(box, DeviceBox(0, 0, 7, 7))
    assertEquals(images(ops).map(_.m), Vector(Affine.Identity))
    val sheared = Picture(sprite(7, 7), Rectangle(1, 1, red, None)).scaleBy(2, 1).rotateBy(30)
    val drawn   = images(plan(sheared, 1.0)._2)
    assertEquals(drawn.size, 1)
    assert(!drawn.head.snapped && !drawn.head.m.keepsAxes)
  }

  test("a bitmap's own turn and crop are tiles of its plan with their clips") {
    val cropped  = sprite(20, 20).crop(Bounds(-5, -4, 5, 5))
    val (_, ops) = plan(cropped.rotateBy(90), 2.0)
    val drawn    = images(ops)
    assertEquals(drawn.size, 1)
    // Doubled, the crop is resampled, so it is drawn where it is. That is 10 by 9 around (0, 0.5),
    // turned a quarter around its middle and doubled, with its device corner at (-9, -9).
    assert(!drawn.head.snapped)
    val clip = drawn.head.clip.getOrElse(fail("no clip"))
    assertEquals(
      clip.map(p => (p.x, p.y)).toSet,
      Set((-9.0, -9.0), (9.0, -9.0), (9.0, 11.0), (-9.0, 11.0))
    )
  }

  test("a bitmap shifted by its own wrap draws two tiles, snapped together only when 1:1") {
    val shifted = sprite(8, 6).wrapShift(3, 0).moveTo(Pos(10.3, 7.6))
    for scale <- Seq(1.0, 2.0) do
      val drawn = images(plan(shifted, scale)._2)
      assertEquals(drawn.size, 2, s"at $scale")
      for image <- drawn do
        assertEquals(image.snapped, scale == 1.0, s"$image at $scale")
        val at = image.m.boxBounds(0, 0, 8, 6).upperLeftCorner
        if scale == 1.0 then assert(isWhole(at.x) && isWhole(at.y), s"$at at $scale")
        else assert(!isWhole(at.x), s"$at at $scale")
      // The tiles keep their seam. They stand the bitmap's width apart, in device pixels.
      val xs = drawn.map(_.m.e).sorted
      assertEqualsDouble(xs(1) - xs(0), 8 * scale, 1e-9)
  }

  test("a Stepwise output lays a bitmap's replayed pixels on their area under the affine") {
    val stepwise = RenderSettings(resamplingMode = ResamplingMode.Stepwise)
    val turned   = sprite(7, 7).rotateBy(30).moveTo(Pos(20.25, 10))
    val (_, ops) = DrawPlan.of(turned, DeviceGrid(2), stepwise, 0L)
    val drawn    = images(ops)
    assertEquals(drawn.size, 1)
    assert(drawn.head.replayed)
    val (width, height) = turned.source.pixelSize
    val area            = turned.source.pixelArea
    val laid            = drawn.head.m.boxBounds(0, 0, width, height)
    // Doubled and resampled, so the corner stays exactly where the affine puts it.
    assertEqualsDouble(laid.width, 2 * area.width, 1e-9)
    assertEqualsDouble(laid.height, 2 * area.height, 1e-9)
    val corner = area.upperLeftCorner + (turned.affine.e, turned.affine.f)
    assertEqualsDouble(laid.upperLeftCorner.x, 2 * corner.x, 1e-6)
    assertEqualsDouble(laid.upperLeftCorner.y, 2 * corner.y, 1e-6)
  }

  test("an SVG picture is a foreign document under its affine") {
    val svg = SvgPicture.fromString(
      """<svg xmlns="http://www.w3.org/2000/svg" width="10" height="4"><rect width="10" height="4"/></svg>"""
    )
    val (_, ops) = plan(svg.moveTo(Pos(5, 2)), 2.0)
    ops match
      case Vector(DrawOp.Foreign(_, m)) => assertEquals(m, Affine(2, 0, 0, 2, 0, 0))
      case other                        => fail(s"not one foreign document: $other")
  }

  test("paints are the fills and strokes of shapes, the masked content and bitmaps' pixels") {
    val stroke = Some(StrokeStyle(Color(0, 0, 255), 2))
    val scene  = Picture(
      Rectangle(10, 10, red, stroke),
      MaskGroup(
        Rectangle(5, 5, Some(FillStyle(Color(9, 9, 9))), None),
        Circle(Pos.Origin, 4, green, None)
      ),
      Bitmap(1, 1, (_, _) => Color(7, 7, 7))
    )
    assertEquals(
      Traversal.paints(scene),
      Set(Color(255, 0, 0), Color(0, 0, 255), Color(0, 255, 0), Color(7, 7, 7)).map(_.toARGBInt)
    )
  }

  test("mapPaints maps every paint but a mask's and keeps the geometry") {
    val white = Color(255, 255, 255).toARGBInt
    val scene = Picture(
      Rectangle(10, 10, red, None),
      MaskGroup(
        Rectangle(5, 5, Some(FillStyle(Color(9, 9, 9))), None),
        Circle(Pos.Origin, 4, green, None)
      )
        .wrapShift(1.5, 0),
      Bitmap(1, 1, (_, _) => Color(7, 7, 7))
    ).rotateBy(10)
    val mapped = Traversal.mapPaints(scene, _ => white)
    assertEquals(Traversal.paints(mapped), Set(white))
    assertEquals(mapped.bounds, scene.bounds)
    assertEquals(mapped.anchor, scene.anchor)
    val maskPaints = mapped match
      case p: Picture =>
        p.children.collect { case w: Wrap => w.child }.collect { case m: MaskGroup =>
          m.masks.head._1
        }
      case other => fail(s"$other")
    assertEquals(maskPaints.map(Traversal.paints), Vector(Set(Color(9, 9, 9).toARGBInt)))
  }

  test("an SVG picture has no paints and maps to itself") {
    val svg = SvgPicture.fromString(
      """<svg xmlns="http://www.w3.org/2000/svg" width="10" height="4"><rect width="10" height="4"/></svg>"""
    )
    assertEquals(Traversal.paints(svg), Set.empty[Int])
    assert(Traversal.mapPaints(svg, _ => 0) eq svg)
  }

  private object Support:
    /** The device box of a path op's local box under its affine. */
    def box(path: DrawOp.Path): Bounds =
      val local = path.shape.localBox
      path.m.boxBounds(
        local.upperLeftCorner.x,
        local.upperLeftCorner.y,
        local.lowerRightCorner.x,
        local.lowerRightCorner.y
      )

  // Deep enough that a recursive move of the deep content overflows Node's stack of about 1 MB.
  // The JVM's test stack is larger, so only the JS run catches such a recursion.
  private val DeepSteps = 10000

  /** The plans of every deep fixture at 2000 steps, each bounded to every nesting checked. */
  private lazy val boundedPlans: Seq[(String, Int, DrawPlan.Plan)] =
    for
      d <- DeepTrees.nested
      built = d.build(2000)
      nesting <- Seq(1, 2, 16)
    yield (d.name, nesting, DrawPlan.planned(built, DeviceGrid(1), settings, 0L, nesting))

  /** The slots that `ops` draws, nested ones included, walked with a stack of its own. */
  private def prepared(ops: Vector[DrawOp]): Vector[Int] =
    listed(ops).collect { case (DrawOp.Prepared(slot, _), _) => slot }

  /** `op` moved by a move that recurses once per level, which the iterative
    * `smile.infrastructure.DrawOp.movedBy` is checked against.
    */
  private def recursivelyMoved(op: DrawOp, dx: Int, dy: Int): DrawOp =
    val move                                     = Affine.translation(dx, dy)
    def all(ops: Vector[DrawOp]): Vector[DrawOp] = ops.map(recursivelyMoved(_, dx, dy))
    def box(b: DeviceBox): DeviceBox             = b.copy(left = b.left + dx, top = b.top + dy)
    op match
      case DrawOp.Path(m, shape, fill, stroke)  => DrawOp.Path(m.andThen(move), shape, fill, stroke)
      case DrawOp.Text(m, layout, fill, stroke) =>
        DrawOp.Text(m.andThen(move), layout, fill, stroke)
      case image: DrawOp.Image =>
        image.copy(
          m = image.m.andThen(move),
          clip = image.clip.map(_.map(p => Pos(p.x + dx, p.y + dy)))
        )
      case DrawOp.Foreign(document, m) => DrawOp.Foreign(document, m.andThen(move))
      case DrawOp.Pixel(x, y, paint)   => DrawOp.Pixel(x + dx, y + dy, paint)
      case layer: DrawOp.Layer         =>
        new DrawOp.Layer(
          box(layer.box),
          layer.clip.map(c => c.copy(m = c.m.andThen(move))),
          layer.masks.map((ops, inverted) => (all(ops), inverted)),
          all(layer.ops)
        )
      case DrawOp.HardClip(b, ops)            => DrawOp.HardClip(box(b), all(ops))
      case DrawOp.Tiles(tile, copies, vector) =>
        DrawOp.Tiles(tile, copies.map(_.andThen(move)), vector)
      case prepared: DrawOp.Prepared => fail(s"$prepared is not planned by DrawPlan.of")

  test("a shallow plan is passed through as it is") {
    for scale <- Seq(1.0, 2.0, 1.5); c <- PlanRenderCases.cases do
      val element          = PlanRenderCases.framed(c.element)
      val (box, ops)       = plan(element, scale)
      val (slots, bounded) = DrawPlan.bounded(ops, box, DrawPlan.MaxNesting)
      assert(bounded eq ops, s"${c.label} at $scale")
      assertEquals(slots, Vector(), s"${c.label} at $scale")
      assertEquals(
        DrawPlan.planned(element, DeviceGrid(scale), settings, 0L),
        DrawPlan.Plan(box, Vector(), ops),
        s"${c.label} at $scale"
      )
  }

  for d <- DeepTrees.nested do
    test(s"${d.name}: every slot and the ops nest at most twice the limit") {
      for (_, nesting, plan) <- boundedPlans.filter(_._1 == d.name) do
        assert(DrawPlan.nestingOf(plan.ops) <= 2 * nesting + 1, s"the ops at $nesting")
        for (slot, i) <- plan.slots.zipWithIndex do
          assert(DrawPlan.nestingOf(Vector(slot.layer)) <= 2 * nesting + 1, s"slot $i at $nesting")
    }

  test("slots refer only to earlier slots, each once") {
    for (name, nesting, plan) <- boundedPlans do
      val inSlots = plan.slots.zipWithIndex.flatMap: (slot, i) =>
        val drawn = prepared(Vector(slot.layer))
        assert(drawn.forall(_ < i), s"$name at $nesting: slot $i draws $drawn")
        drawn
      assertEquals(
        (inSlots ++ prepared(plan.ops)).sorted,
        plan.slots.indices.toVector,
        s"$name at $nesting"
      )
  }

  test("the slots ops draw are the ones they refer to, and the ones those refer to") {
    val box                 = DeviceBox(0, 0, 10, 10)
    def layer(ops: DrawOp*) = new DrawOp.Layer(box, None, Vector(), ops.toVector)
    val dot                 = DrawOp.Pixel(1, 1, Color(255, 0, 0))
    val slots               =
      Vector(layer(dot), layer(DrawOp.Prepared(0, box)), layer(dot)).map(DrawPlan.Slot(_, box))
    val tiles = DrawOp.Tiles(layer(DrawOp.Prepared(1, box)), Vector(Affine.Identity))
    val ops   = Vector(tiles, DrawOp.Prepared(2, box))
    def drawn(ops: Vector[DrawOp], opens: DrawOp.Tiles => Boolean = _ => true) =
      DrawPlan.drawnSlots(slots, ops, opens).toVector
    assertEquals(drawn(ops), Vector(true, true, true))
    assertEquals(drawn(Vector(tiles)), Vector(true, true, false))
    assertEquals(drawn(ops, _ => false), Vector(false, false, true))
  }

  test("a hoisted layer's region is what the painter would give it") {
    val dot = DrawOp.Pixel(25, 27, Color(255, 0, 0))
    // A chain of three masks, each the mask of the layer around it, under a hard clip.
    val innermost =
      new DrawOp.Layer(DeviceBox(5, 25, 100, 10), None, Vector((Vector(dot), false)), Vector(dot))
    val middle = new DrawOp.Layer(
      DeviceBox(20, 20, 40, 40),
      None,
      Vector((Vector(innermost), false)),
      Vector(dot)
    )
    val outer =
      new DrawOp.Layer(
        DeviceBox(-10, -10, 40, 40),
        None,
        Vector((Vector(middle), true)),
        Vector(dot)
      )
    val ops              = Vector(DrawOp.HardClip(DeviceBox(0, 0, 50, 50), Vector(outer)))
    val (slots, bounded) = DrawPlan.bounded(ops, DeviceBox(0, 0, 60, 60), nesting = 1)
    // The clip shows (0, 0) to (50, 50), the outer layer the part of it from (0, 0) to (30, 30),
    // the middle one the part from (20, 20) to (30, 30) and the innermost the part from (20, 25).
    val (outerRegion, middleRegion, innermostRegion) =
      (DeviceBox(0, 0, 30, 30), DeviceBox(20, 20, 10, 10), DeviceBox(20, 25, 10, 5))
    assertEquals(
      slots,
      Vector(
        DrawPlan.Slot(innermost, innermostRegion),
        DrawPlan.Slot(
          middle.copy(masks = Vector((Vector(DrawOp.Prepared(0, innermostRegion)), false))),
          middleRegion
        ),
        DrawPlan.Slot(
          outer.copy(masks = Vector((Vector(DrawOp.Prepared(1, middleRegion)), true))),
          outerRegion
        )
      )
    )
    assertEquals(
      bounded,
      Vector(DrawOp.HardClip(DeviceBox(0, 0, 50, 50), Vector(DrawOp.Prepared(2, outerRegion))))
    )
  }

  test(
    "a hard clip directly inside a hard clip is one clip of both, siblings keeping their order"
  ) {
    val (x, y, z, w) = (
      DrawOp.Pixel(1, 1, Color(255, 0, 0)),
      DrawOp.Pixel(12, 12, Color(0, 255, 0)),
      DrawOp.Pixel(2, 2, Color(0, 0, 255)),
      DrawOp.Pixel(41, 41, Color(0, 0, 0))
    )
    val outer    = DeviceBox(0, 0, 20, 20)
    val inner    = DeviceBox(10, 10, 20, 20)
    val disjoint = DrawOp.HardClip(DeviceBox(40, 40, 5, 5), Vector(w))
    val ops      =
      Vector(DrawOp.HardClip(outer, Vector(x, DrawOp.HardClip(inner, Vector(y, disjoint)), z)))
    assertEquals(
      DrawPlan.bounded(ops, DeviceBox(0, 0, 50, 50), nesting = 1),
      (
        Vector(),
        Vector(
          DrawOp.HardClip(outer, Vector(x)),
          DrawOp.HardClip(DeviceBox(10, 10, 10, 10), Vector(y)),
          DrawOp.HardClip(outer, Vector(z))
        )
      )
    )
  }

  test("a deep tree in a whole-pixel wrap plans") {
    val shift = DeepTrees.nested
      .find(_.name == "a shift of turned crops")
      .getOrElse(fail("No fixture is a shift of turned crops"))
    // The shifted content covers 30 by 30 pixels from (-15, -15), which the shift by (3, 2) draws
    // moved by these whole pixels.
    val moves = for column <- Vector(0, 1); row <- Vector(0, 1)
    yield (3 - 30 * column, 2 - 30 * row)
    // The content planned on its own, and the copies of it that the wrap draws.
    def copies(steps: Int): (Vector[DrawOp], Vector[Vector[DrawOp]]) =
      shift.build(steps) match
        case wrap: Wrap =>
          assertEquals(wrap.affine, Affine.Identity)
          val content = plan(wrap.child, 1.0)._2
          plan(wrap, 1.0)._2 match
            case Vector(DrawOp.HardClip(_, drawn)) =>
              (content, drawn.grouped(content.size).toVector)
            case other => fail(s"The wrap draws ${listed(other).take(3)}")
        case _ => fail("The fixture is not a wrap")
    val (content, drawn) = copies(DeepSteps)
    assertEquals(drawn.size, moves.size)
    // The content holds no tiles, whose own operations a move keeps, so each of its operations
    // moves on its own.
    for ((dx, dy), copy) <- moves.zip(drawn) do
      assertEquals(
        listed(copy),
        listed(content).map((op, inner) => (recursivelyMoved(op, dx, dy), inner)),
        s"moved by ($dx, $dy)"
      )
    val (shallowContent, shallowDrawn) = copies(20)
    for ((dx, dy), copy) <- moves.zip(shallowDrawn) do
      assertEquals(copy, shallowContent.map(recursivelyMoved(_, dx, dy)), s"moved by ($dx, $dy)")
  }

  test("quarter-turned shifts plan a constant number of operations") {
    val names = Seq("shifting and turning by quarter turns", "shifting a crop of a shift")
    for name <- names do
      val fold = DeepFoldSuite.Folds.find(_.name == name).getOrElse(fail(s"No fold is $name"))
      def count(steps: Int): Int = listed(plan(fold.flat(steps), 1.0)._2).size
      // Taken where the turns end the same way and no offset is whole. The crop's shifts add up to
      // whole periods after 400 steps, which leaves the shift wrapping a crop from then on.
      assertEquals(count(1010), count(410), name)
  }

  test("a move leaves out the layers and hard clips that show none of the pixels shown") {
    val shown                                  = DeviceBox(0, 0, 10, 10)
    def box(left: Int): DeviceBox              = DeviceBox(left, 0, 10, 10)
    def dot(x: Int): DrawOp                    = DrawOp.Pixel(x, 1, Color(255, 0, 0))
    def layer(left: Int, ops: DrawOp*): DrawOp =
      new DrawOp.Layer(box(left), None, Vector(), ops.toVector)
    def clip(left: Int, ops: DrawOp*): DrawOp = DrawOp.HardClip(box(left), ops.toVector)
    // Moved by 5, the clip keeps the pixels from 5 to 10 of those shown. A layer moved to 8 meets
    // them, and a layer moved to -7 and a clip moved to -9 meet only pixels shown that the clip
    // hides. The dots draw where they are.
    val op = clip(0, dot(1), layer(3, dot(4)), layer(-12, dot(2)), clip(-14, dot(3)))
    assertEquals(op.movedBy(5, 0, shown), Some(clip(5, dot(6), layer(8, dot(9)))))
    assertEquals(clip(20, dot(21)).movedBy(5, 0, shown), None)
    assertEquals(dot(30).movedBy(5, 0, shown), Some(dot(35)))
    // Not moved, an operation within the pixels shown is kept as it is, and one reaching past them
    // is walked.
    assert(op.movedBy(0, 0, shown).exists(_ eq op))
    val wide = clip(-3, dot(1), clip(-14, dot(3)))
    assertEquals(wide.movedBy(0, 0, shown), Some(clip(-3, dot(1))))
  }

  test("nested whole-pixel wraps plan only the copies that show") {
    def count(e: Element): Int = listed(plan(e, 1.0)._2).size
    val turning                = DeepFoldSuite.Folds
      .find(_.name == "shifting and turning by quarter turns")
      .getOrElse(fail("No fold shifts and turns"))
    def turned(steps: Int): Int = count(turning.nested(steps))
    // Four quarter turns take the shifts back where they started, so the copies cut the clip into
    // the same pieces at every level from the fourth on.
    assertEquals(turned(12) - turned(11), turned(6) - turned(5))
    val square                       = Rectangle(400, 400, Some(FillStyle(Color(0, 128, 0))), None)
    def shifted(e: Element): Element =
      e.bounds.fold(e)(box => new Wrap(box, e, 3, 2, Affine.Identity, e.anchor))
    def added(steps: Int): Int = count(DeepFoldSuite.steps(steps, square)((a, _) => shifted(a)))
    // Shifts that add up cut the clip into one more row and column of pieces at each level, so
    // twice the levels plan a few times the operations, where four copies a level planned 4^12
    // times as many.
    assert(added(24) < 16 * added(12), s"${added(24)} at 24 levels, ${added(12)} at 12")
  }

  test("a layer of no pixels is left out with its operations") {
    val dot    = DrawOp.Pixel(1, 1, Color(255, 0, 0))
    val hidden = new DrawOp.Layer(DeviceBox(30, 30, 10, 10), None, Vector(), Vector(dot))
    val shown  = new DrawOp.Layer(DeviceBox(0, 0, 10, 10), None, Vector(), Vector(dot))
    val ops    = Vector(DrawOp.HardClip(DeviceBox(0, 0, 20, 20), Vector(hidden, shown)))
    assertEquals(
      DrawPlan.bounded(ops, DeviceBox(0, 0, 50, 50), nesting = 1),
      (
        Vector(DrawPlan.Slot(shown, DeviceBox(0, 0, 10, 10))),
        Vector(
          DrawOp.HardClip(
            DeviceBox(0, 0, 20, 20),
            Vector(DrawOp.Prepared(0, DeviceBox(0, 0, 10, 10)))
          )
        )
      )
    )
  }

  test("a long chain of hard clips with siblings is bounded in linear time") {
    val chain = DeepTrees.nested
      .find(_.name == "scaling, adding in front and cropping")
      .getOrElse(fail("No fixture is a chain of crops"))
    val (box, ops) = plan(chain.build(40000), 1.0)
    // Copying each level's clips into the level above it took several seconds here, and the walk
    // that adds them where they end up takes tens of milliseconds.
    val start            = System.nanoTime()
    val (slots, bounded) = DrawPlan.bounded(ops, box, DrawPlan.MaxNesting)
    val millis           = (System.nanoTime() - start) / 1e6
    assert(millis < 1500, s"$millis ms")
    assertEquals(slots, Vector())
    assertEquals(bounded.size, 40000)
    assert(DrawPlan.nestingOf(bounded) == 1)
  }

  test("an operation that draws a slot cannot be moved") {
    val box = DeviceBox(0, 0, 1, 1)
    val _   = intercept[IllegalStateException](DrawOp.Prepared(0, box).movedBy(1, 0, box))
  }

  /** Pixels that tell apart where the operations of nested clips land. */
  private val (x, y, p, q, r) = (
    DrawOp.Pixel(1, 1, Color(255, 0, 0)),
    DrawOp.Pixel(2, 2, Color(0, 255, 0)),
    DrawOp.Pixel(12, 12, Color(0, 0, 255)),
    DrawOp.Pixel(13, 13, Color(0, 0, 0)),
    DrawOp.Pixel(22, 22, Color(255, 255, 0))
  )

  test("a chain of three clips keeps the order of the operations between them") {
    val (a, b, c) = (DeviceBox(0, 0, 40, 40), DeviceBox(10, 10, 40, 40), DeviceBox(20, 20, 40, 40))
    val ops       = Vector(
      DrawOp.HardClip(
        a,
        Vector(x, DrawOp.HardClip(b, Vector(p, DrawOp.HardClip(c, Vector(r)), q)), y)
      )
    )
    val (inB, inC) = (DeviceBox(10, 10, 30, 30), DeviceBox(20, 20, 20, 20))
    assertEquals(
      DrawPlan.bounded(ops, DeviceBox(0, 0, 60, 60), nesting = 1),
      (
        Vector(),
        Vector(
          DrawOp.HardClip(a, Vector(x)),
          DrawOp.HardClip(inB, Vector(p)),
          DrawOp.HardClip(inC, Vector(r)),
          DrawOp.HardClip(inB, Vector(q)),
          DrawOp.HardClip(a, Vector(y))
        )
      )
    )
  }

  test("a clip in a clip directly inside a layer lands among the layer's operations") {
    val (a, b) = (DeviceBox(0, 0, 20, 20), DeviceBox(10, 10, 20, 20))
    val layer  = new DrawOp.Layer(
      DeviceBox(0, 0, 50, 50),
      None,
      Vector(),
      Vector(DrawOp.HardClip(a, Vector(x, DrawOp.HardClip(b, Vector(p)))))
    )
    assertEquals(
      DrawPlan.bounded(Vector(layer), DeviceBox(0, 0, 50, 50), nesting = 2),
      (
        Vector(),
        Vector(
          layer.copy(ops =
            Vector(
              DrawOp.HardClip(a, Vector(x)),
              DrawOp.HardClip(DeviceBox(10, 10, 10, 10), Vector(p))
            )
          )
        )
      )
    )
  }

  test("tiles whose tile has no pixels are left out, wherever their copies go") {
    val tile  = new DrawOp.Layer(DeviceBox(0, 0, 0, 5), None, Vector(), Vector(p))
    val tiles = DrawOp.Tiles(tile, Vector(Affine.translation(3, 3)))
    val clip  = DeviceBox(0, 0, 20, 20)
    assertEquals(
      DrawPlan.bounded(
        Vector(DrawOp.HardClip(clip, Vector(tiles, x))),
        DeviceBox(0, 0, 20, 20),
        nesting = 1
      ),
      (Vector(), Vector(DrawOp.HardClip(clip, Vector(x))))
    )
  }
end DrawPlanSuite
