package smile.infrastructure

import smile.RenderSettings
import smile.colors.Color
import smile.modeling.{Bounds, Pos}
import smile.pictures.*
import smile.pictures.{FillStyle, ResamplingHint}

import java.awt.RenderingHints
import java.awt.image.BufferedImage
import scala.util.control.NonFatal

/** The Java2D renderer of the shared draw plan. Covers ink within bounds for every shared case at
  * several scales, strokes and glyphs under the affine, grid-keeping content at 2x, masks, clips,
  * wraps and the density of picture sources.
  */
class JVMPlanRendererSuite extends munit.FunSuite:

  import PlanRenderCases.*

  /** A buffer compared by identity, as a set member. */
  private final class IdentityKey(val buffer: BufferedImage):
    override def hashCode: Int               = System.identityHashCode(buffer)
    override def equals(other: Any): Boolean = other match
      case key: IdentityKey => key.buffer eq buffer
      case _                => false

  private val settings = RenderSettings.Default

  /** The packed ARGB pixels of `e` rendered at `scale` with its plan's nesting bounded by
    * `nesting`, with their width and height.
    */
  private def render(
      e: Element,
      scale: Double = 1.0,
      nesting: Int = DrawPlan.MaxNesting
  ): (Int, Int, Array[Int]) =
    val pixels = JVMPlanRenderer.render(e, scale, settings, 0L, nesting).buffer(settings, 0L)
    (pixels.width, pixels.height, pixels.argbData)

  private def alpha(argb: Int): Int = argb >>> 24

  for scale <- Seq(1.0, 2.0, 1.5); c <- cases do
    test(s"${c.label} draws within its bounds at $scale") {
      val (width, height, argb) = render(framed(c.element), scale)
      val (frame, _)            = DrawPlan.of(framed(c.element), DeviceGrid(scale), settings, 0L)
      assertEquals((width, height), (frame.width, frame.height))
      val problems = inkProblems(c.element, scale, argb, c.tight, settings)
      assert(problems.isEmpty, problems.mkString("; "))
    }

  test("a turned sprite inks no pixel outside its bounds at any angle or offset") {
    val problems =
      for
        c       <- turnedSweep
        problem <- inkProblems(c.element, 1.0, render(framed(c.element))._3, c.tight, settings)
      yield s"${c.label}: $problem"
    assert(problems.isEmpty, problems.mkString("\n"))
  }

  test("the empty picture renders as an empty bitmap") {
    val bitmap = JVMPlanRenderer.render(Empty, 1.0, settings)
    assert(bitmap.isEmpty, s"$bitmap")
  }

  test("the buffer is the device box and the bounds are its logical box") {
    val e      = Rectangle(10, 10, Pos(0.25, 0.25), Some(FillStyle(red)), None)
    val bitmap = JVMPlanRenderer.render(e, 1.5, settings)
    val pixels = bitmap.buffer(settings, 0L)
    // -4.75 to 5.25 logical is -7.125 to 7.875 device, which rounds out to -8 to 8.
    assertEquals((pixels.width, pixels.height), (16, 16))
    assertEqualsDouble(bitmap.boundary.upperLeftCorner.x, -8 / 1.5, 1e-9)
    assertEqualsDouble(bitmap.boundary.lowerRightCorner.y, 8 / 1.5, 1e-9)
  }

  test("a stroked circle scaled 3x along x is 3x thicker at its sides than at its top") {
    val circle                 = Circle(Pos.Origin, 20, None, stroke(4)).scaleBy(3, 1)
    val (width, height, p)     = render(circle)
    def opaque(x: Int, y: Int) = alpha(p(y * width + x)) >= 128
    val middleRow              = height / 2
    val side                   = (0 until width).takeWhile(x => !opaque(x, middleRow)).length
    val sideRun                = (side until width).takeWhile(x => opaque(x, middleRow)).length
    val middleColumn           = width / 2
    val top                    = (0 until height).takeWhile(y => !opaque(middleColumn, y)).length
    val topRun                 = (top until height).takeWhile(y => opaque(middleColumn, y)).length
    assertEquals(topRun, 4)
    assertEquals(sideRun, 12)
  }

  test("a mirrored text's pixels are the mirror of the text's") {
    val text                                      = boxedText("Mirror")
    val (width, height, ahead)                    = render(text)
    val (mirroredWidth, mirroredHeight, mirrored) = render(text.flipHorizontally)
    assertEquals((mirroredWidth, mirroredHeight), (width, height))
    var worst = 0
    for y <- 0 until height; x <- 0 until width do
      val (a, b) = (ahead(y * width + x), mirrored(y * width + width - 1 - x))
      for shift <- Seq(0, 8, 16, 24) do
        worst = worst.max((((a >>> shift) & 0xff) - ((b >>> shift) & 0xff)).abs)
    assert(worst <= 1, s"a channel differs by $worst")
    assert(ahead.exists(alpha(_) == 255), "the text draws nothing opaque")
  }

  test("grid-keeping content at 2x is its 1x rendering with every pixel doubled") {
    // The sprites are pixelated. A smooth bitmap doubled is resampled with blending.
    val content = Picture(
      sprite(7, 5).withResampling(ResamplingHint.Pixelated).moveTo(Pos(10.5, 4.5)),
      Rectangle(6, 4, Pos(-3, 2), Some(FillStyle(green)), None),
      Point(Pos(20.5, 0.5), blue),
      sprite(3, 4)
        .withResampling(ResamplingHint.Pixelated)
        .rotateBy(90)
        .flipHorizontally
        .moveTo(Pos(0, -6.5)),
      Rectangle(8, 6, Pos(0, 10), Some(FillStyle(red)), None).crop(Bounds(-2, 8, 3, 12))
    )
    val (width, height, once)    = render(content, 1.0)
    val (width2, height2, twice) = render(content, 2.0)
    assertEquals((width2, height2), (2 * width, 2 * height))
    for y <- 0 until height2; x <- 0 until width2 do
      assertEquals(
        twice(y * width2 + x),
        once((y / 2) * width + x / 2),
        s"pixel ($x, $y) at 2x is not ${(x / 2, y / 2)} at 1x"
      )
  }

  test("a smoothed crop samples nothing from outside it") {
    for (label, element, scales) <- fencedCrops; scale <- scales do
      val (_, _, p) = render(element, scale)
      val blue      = p.count(argb => (argb & 0xff) != 0)
      assertEquals(blue, 0, s"$label at $scale has $blue pixels with blue from outside it")
      assert(p.exists(alpha(_) == 255), s"$label at $scale draws nothing opaque")
  }

  test("a mask keeps the content where the mask is opaque, and an inverted one where it is not") {
    val square       = Rectangle(30, 30, Pos.Origin, Some(FillStyle(red)), None)
    val disc         = Circle(Pos.Origin, 8, Some(FillStyle(green)), None)
    val (w, _, kept) = render(MaskGroup(disc, square))
    assertEquals(kept((w / 2) * w + w / 2), red.toARGBInt)
    assertEquals(alpha(kept(0)), 0)
    val (wi, _, cut) = render(MaskGroup(disc, square, inverted = true))
    assertEquals(alpha(cut((wi / 2) * wi + wi / 2)), 0)
    assertEquals(cut(0), red.toARGBInt)
  }

  test("an axis-aligned clip is hard on whole device pixels and a turned one is antialiased") {
    val disc         = Circle(Pos.Origin, 40, Some(FillStyle(red)), None)
    val (w, h, hard) = render(disc.crop(Bounds(-10.3, -12.7, 15.2, 8.1)))
    assert(hard.forall(_ == red.toARGBInt), "a fractional axis-aligned clip left partial pixels")
    assertEquals((w, h), (27, 22))
    val (_, _, soft) = render(disc.crop(Bounds(-10, -10, 10, 10)).rotateBy(30))
    assert(soft.exists(p => alpha(p) > 0 && alpha(p) < 255), "the turned clip is not antialiased")
  }

  test("a bitmap loaded from a file keeps its bordered copies from one frame to the next") {
    val file = java.nio.file.Files.createTempFile("smile-sprite", ".png")
    try
      val image = new java.awt.image.BufferedImage(9, 7, java.awt.image.BufferedImage.TYPE_INT_ARGB)
      for y <- 0 until 7; x <- 0 until 9 do image.setRGB(x, y, 0xff000000 | (x * 25) << 16 | y * 30)
      assert(javax.imageio.ImageIO.write(image, "png", file.toFile))
      val loaded = Bitmap(file.toString)
      // Drawn with a fading edge at 1x and through a layer at 2x, each frame turned a little more.
      for scale <- Seq(1.0, 2.0) do
        val _      = render(loaded.rotateBy(30), scale)
        val before = JVMBufferAdapter.builtBorders.get
        for frame <- 1 to 3 do
          val _ = render(loaded.rotateBy(30 + frame), scale)
        assertEquals(JVMBufferAdapter.builtBorders.get - before, 0L, s"at $scale")
    finally
      val _ = java.nio.file.Files.deleteIfExists(file)
  }

  test("a bordered copy repeats the edge pixels around the pixels it copies") {
    val buffer = new JVMBufferAdapter(3, 2)
    buffer.setARGBData(Array(1, 2, 3, 4, 5, 6).map(_ | 0xff000000))
    val bordered = buffer.borderedImage
    val rows     = (0 until 4).map(y => (0 until 5).map(x => bordered.getRGB(x, y) & 0xffffff))
    assertEquals(
      rows,
      Vector(
        Vector(1, 1, 2, 3, 3),
        Vector(1, 1, 2, 3, 3),
        Vector(4, 4, 5, 6, 6),
        Vector(4, 4, 5, 6, 6)
      )
    )
    val window = buffer.borderedImage(1, 1, 2, 1)
    assertEquals((0 until 4).map(x => window.getRGB(x, 1) & 0xffffff), Vector(5, 5, 6, 6))
  }

  test("a bordered copy is made once for each revision of the pixels, and never once escaped") {
    val buffer = new JVMBufferAdapter(4, 4)
    val first  = buffer.borderedImage
    assert(buffer.borderedImage eq first, "the same pixels were copied again")
    assert(buffer.borderedImage(1, 1, 2, 2) eq buffer.borderedImage(1, 1, 2, 2))
    buffer.setRGBA(0, 0, red)
    assert(!(buffer.borderedImage eq first), "changed pixels kept their old copy")
    assertEquals(buffer.borderedImage.getRGB(0, 0), red.toARGBInt)
    val _ = buffer.get
    assert(!(buffer.borderedImage eq buffer.borderedImage), "an escaped image was cached")
  }

  test(
    "a premultiplied copy is made once for each revision of the pixels, and never once escaped"
  ) {
    val buffer = new JVMBufferAdapter(4, 4)
    val first  = buffer.premultipliedImage
    assertEquals(first.getType, java.awt.image.BufferedImage.TYPE_INT_ARGB_PRE)
    assert(buffer.premultipliedImage eq first, "the same pixels were copied again")
    buffer.setRGBA(0, 0, red)
    val afterColor = buffer.premultipliedImage
    assert(!(afterColor eq first), "a pixel set kept the old copy")
    assertEquals(afterColor.getRGB(0, 0), red.toARGBInt)
    buffer.setARGBData(Array.fill(16)(blue.toARGBInt))
    val afterData = buffer.premultipliedImage
    assert(!(afterData eq afterColor), "new pixel data kept the old copy")
    assertEquals(afterData.getRGB(3, 3), blue.toARGBInt)
    buffer.withGraphics2D(settings): g =>
      g.setColor(java.awt.Color.GREEN)
      g.fillRect(0, 0, 4, 4)
    val afterDrawing = buffer.premultipliedImage
    assert(!(afterDrawing eq afterData), "drawing on the pixels kept the old copy")
    assertEquals(afterDrawing.getRGB(1, 2), green.toARGBInt)
    val raw = buffer.get
    assert(buffer.premultipliedImage eq raw, "an escaped image was copied rather than drawn")
  }

  test("a translucent bitmap resampled along the axes draws the same from its premultiplied copy") {
    val (w, h) = (9, 7)
    val argb   = Array.tabulate(w * h): i =>
      val (x, y) = (i % w, i / w)
      ((20 + 26 * x) << 24) | ((37 * x) % 256 << 16) | ((53 * y) % 256 << 8) | ((x + y) * 29 % 256)
    val bitmap               = Bitmap.fromARGB(w, h, argb).moveBy(0.25, 0.5)
    val scales               = Seq(1.5, 2.0, 1.3)
    def drawn(scale: Double) = render(bitmap, scale)._3.toVector
    val before               = JVMBufferAdapter.builtPremultiplied.get
    val throughCopy          = scales.map(drawn)
    assertEquals(JVMBufferAdapter.builtPremultiplied.get - before, 1L, "one copy, drawn from")
    // Escaped, the pixels themselves are drawn, and no copy is made.
    val _ = bitmap.nativeBuffer(settings).get
    assertEquals(scales.map(drawn), throughCopy)
    assertEquals(JVMBufferAdapter.builtPremultiplied.get - before, 1L, "an escaped image copied")
  }

  test("only four distinct corners of a box with some area clip as a rectangle") {
    def corners(points: (Double, Double)*) = points.toVector.map(Pos(_, _))
    assertEquals(
      JVMPlanRenderer.rectangleOf(corners((2, 1), (5, 1), (5, 4), (2, 4))),
      Some(DeviceBox(2, 1, 3, 3))
    )
    assertEquals(
      JVMPlanRenderer.rectangleOf(corners((5, 4), (5, 1), (2, 1), (2, 4))),
      Some(DeviceBox(2, 1, 3, 3))
    )
    for degenerate <- Seq(
        corners((0, 0), (2, 0), (2, 2), (2, 0)),
        corners((0, 0), (2, 0), (2, 0), (0, 0)),
        corners((1, 1), (1, 1), (1, 1), (1, 1)),
        corners((0, 0), (2, 0), (0, 2), (2, 2)),
        corners((0, 0), (2.5, 0), (2.5, 2), (0, 2))
      )
    do assertEquals(JVMPlanRenderer.rectangleOf(degenerate), None, s"$degenerate")
  }

  test("a turned bitmap's edges are antialiased, as a canvas draws them") {
    // Java2D clips and draws a turned image without antialiasing.
    val (_, _, p) = render(sprite(8, 8).rotateBy(30))
    assert(p.exists(argb => alpha(argb) > 0 && alpha(argb) < 255), "the turned edges are jagged")
  }

  test("a crop drawn at fewer device pixels than its own covers every device pixel of its clip") {
    // 12 by 12 pixels in 4 by 4 logical ones are 6 by 6 device pixels at 1.5x, which the source
    // pixels whose centers the clip's pixels lie on reach only 5 of.
    val dense     = Renderer().createBitmapFrom(Picture(sprite(4, 4)), 3.0).moveBy(1, -1)
    val (_, _, p) = render(dense.crop(Bounds(-6, -6, 6, 6)), 1.5)
    assertEquals(p.count(alpha(_) == 255), 36)
  }

  test("a wrap with a fractional period has no seam") {
    val filled = Rectangle(20.5, 10.25, Pos.Origin, Some(FillStyle(red)), None).wrapShift(5.3, 2)
    // A whole number of device pixels at 2x, from a half one.
    val halfway = Rectangle(15.5, 6, Pos.Origin, Some(FillStyle(red)), None).wrapShift(4, 0)
    for element <- Seq(filled, halfway); scale <- Seq(1.0, 1.5, 2.0) do
      val (w, h, p) = render(element, scale)
      // Every pixel but the outermost ones, which the period's edges may cover partly, is full.
      for y <- 1 until h - 1; x <- 1 until w - 1 do
        assertEquals(alpha(p(y * w + x)), 255, s"pixel ($x, $y) at $scale")
  }

  test("a whole-pixel wrap shift moves the pixels exactly") {
    val b                 = sprite(12, 8)
    val (w, h, shifted)   = render(b.wrapShift(0.0, 0.0).wrapShift(5, 3))
    val (_, _, reference) = render(b.wrapShift(5, 3))
    assertEquals((w, h), (12, 8))
    assert(shifted.sameElements(reference), "the generic wrap differs from the bitmap's own")
  }

  test("a bitmap of a picture is rendered at the density its scaling needs") {
    val picture   = Circle(Pos(0, 0), 2, Some(FillStyle(red)), None)
    val bitmap    = Bitmap.of(picture).withResampling(ResamplingHint.Pixelated)
    val (w, h, p) = render(Picture(bitmap).scaleBy(4))
    // Drawn from its 1x pixels, every 4x4 block would be one color.
    val blocky = (0 until h / 4).forall: row =>
      (0 until w / 4).forall: column =>
        val first = p(row * 4 * w + column * 4)
        (0 until 4).forall(dy =>
          (0 until 4).forall(dx => p((row * 4 + dy) * w + column * 4 + dx) == first)
        )
    assert(!blocky, "the picture was drawn from its 1x pixels")
  }

  test("a bitmap's pixels under a transform no step expresses are rendered through the plan") {
    val b = sprite(10, 6).scaleBy(0.01).rotateBy(90).scaleBy(100) match
      case bitmap: Bitmap => bitmap
      case other          => fail(s"$other is not a bitmap")
    val pixels = b.pixels(settings)
    assertEquals((pixels.width, pixels.height), (6, 10))
    // The source's pixel at column 3 and row 4 lands at column 4 and row 6.
    assertEquals(b.getColor(4, 6, settings), pattern(3, 4))
  }
  test("ops after a bitmap under a crop keep the crop, not a clip moved by the bitmap's map") {
    for scale <- Seq(1.0, 1.5, 2.0) do
      val (w, h, p) = render(shapeBitmapShape.crop(Bounds(-10, -10, 10, 10)), scale)
      assertEquals((w, h), (math.ceil(20 * scale).toInt, math.ceil(20 * scale).toInt))
      val wrong = p.count(_ != red.toARGBInt)
      assertEquals(wrong, 0, s"$wrong pixels are not red at $scale")
  }

  /** A bitmap of a 5000 x 5000 picture. */
  private def world: Bitmap =
    Bitmap.of(Rectangle(5000, 5000, Pos(2500, 2500), Some(FillStyle(green)), None))

  private def cropOf(b: Bitmap, left: Double, top: Double): Bitmap =
    b.crop(Bounds(left, top, left + 800, top + 600)) match
      case cropped: Bitmap => cropped
      case other           => fail(s"$other is not a bitmap")

  test("a crop of a large picture source renders only the part around what is drawn") {
    val view      = cropOf(world, 1000, 2000)
    val (w, h, p) = render(Picture(view), 2.0)
    assertEquals((w, h), (1600, 1200))
    assert(p.forall(_ == green.toARGBInt), "the crop is not all the picture's green")
    val rendered = view.source.renderedSourcePixelCount
    // The part drawn, rounded out to the tile grid at 2x, is at most 3x the pixels shown.
    assert(rendered <= 3L * 1600 * 1200, s"$rendered source pixels rendered for 1600 x 1200")
  }

  test("scrolling a crop of a picture source by a few pixels reuses its rendering") {
    // Counts on RasterCache.shared keeping what was rendered between the draws, which fits
    // its budget many times over.
    val b      = world
    val first  = cropOf(b, 1030, 2030)
    val _      = render(Picture(first), 2.0)
    val before = first.source.renderedSourcePixelCount
    for step <- 1 to 5 do
      val _ = render(Picture(cropOf(b, 1030 + 3 * step, 2030 + 2 * step)), 2.0)
    assertEquals(first.source.renderedSourcePixelCount, before)
  }

  test("a layer takes the smallest kept buffer it fits in") {
    val pool  = new JVMPlanRenderer.LayerPool(1000000)
    val large = pool.acquire(200, 200)
    val small = pool.acquire(50, 50)
    val wide  = pool.acquire(300, 40)
    pool.release(large)
    pool.release(small)
    pool.release(wide)
    assert(pool.acquire(40, 40) eq small)
    assert(pool.acquire(150, 60) eq large)
    assert(pool.acquire(250, 10) eq wide)
  }

  test("the pool keeps buffers within its budget, the newest first") {
    val pool      = new JVMPlanRenderer.LayerPool(25000)
    val (a, b, c) = (pool.acquire(100, 100), pool.acquire(100, 100), pool.acquire(100, 100))
    Seq(a, b, c).foreach(pool.release)
    assertEquals(pool.keptCount, 2)
    assert(pool.acquire(100, 100) eq c)
    assert(pool.acquire(100, 100) eq b)
    val huge =
      new java.awt.image.BufferedImage(200, 200, java.awt.image.BufferedImage.TYPE_INT_ARGB)
    pool.release(huge)
    assertEquals(pool.keptCount, 0)
  }

  test("a layer that a hard clip mostly hides takes a buffer of only what can show") {
    // A large mask seen through a small crop, drawn into a pool of its own.
    val mask   = Circle(Pos.Origin, 1500, Some(FillStyle(Color(0, 0, 0))), None)
    val masked = MaskGroup(mask, Rectangle(3000, 3000, Some(FillStyle(Color(255, 0, 0))), None))
    val plan   = DrawPlan.planned(masked.crop(Bounds(-5, -5, 5, 5)), DeviceGrid(1.0), settings, 0L)
    val pool   = new JVMPlanRenderer.LayerPool(1000000)
    val image  = new BufferedImage(plan.box.width, plan.box.height, BufferedImage.TYPE_INT_ARGB)
    val g      = image.createGraphics()
    try
      new JVMPlanRenderer.Painter(settings, pool, 0L, 1.0).drawAll(
        g,
        plan.slots,
        plan.ops,
        -plan.box.left,
        -plan.box.top,
        plan.box
      )
    finally g.dispose()
    assert(
      pool.largestKept > 0 && pool.largestKept <= 10 * 10,
      s"the largest buffer kept has ${pool.largestKept} pixels"
    )
  }

  /** Runs `work` on `count` threads at once and waits for all of them, failing with what any of
    * them threw.
    */
  private def onThreads(count: Int)(work: => Unit): Unit =
    val failures           = new java.util.concurrent.ConcurrentLinkedQueue[Throwable]
    val runnable: Runnable = () =>
      try work
      catch
        case NonFatal(e) =>
          val _ = failures.add(e)
    val threads = Vector.fill(count)(new Thread(runnable))
    threads.foreach(_.start())
    threads.foreach(_.join())
    assert(failures.isEmpty, s"a thread failed: ${failures.peek()}")

  test("threads share one pool within its budget") {
    val pool = new JVMPlanRenderer.LayerPool(25000)
    onThreads(4):
      for _ <- 1 to 50 do pool.release(pool.acquire(100, 100))
    assert(pool.keptCount <= 2, s"${pool.keptCount} buffers are kept")
  }

  test("a buffer is never handed out twice at once") {
    val pool = new JVMPlanRenderer.LayerPool(1000000)
    val out  = java.util.concurrent.ConcurrentHashMap.newKeySet[IdentityKey]
    onThreads(8):
      for _ <- 1 to 200 do
        val buffer = pool.acquire(10, 10)
        assert(out.add(IdentityKey(buffer)), "a buffer in use was handed out again")
        Thread.`yield`()
        assert(out.remove(IdentityKey(buffer)))
        pool.release(buffer)
  }

  test("every rendering takes its layers from the one shared pool") {
    val mask   = Circle(Pos.Origin, 20, Some(FillStyle(Color(0, 0, 0))), None)
    val masked = MaskGroup(mask, Rectangle(60, 60, Some(FillStyle(Color(255, 0, 0))), None))
    val _      = JVMPlanRenderer.render(masked, 1.0, settings, 0L)
    assert(JVMPlanRenderer.LayerPool.shared.largestKept > 0)
  }

  test("drawing tiles leaves the interpolation of the graphics as it was") {
    // The plan puts tiles under a clip of their own today, but an operation drawn after them on
    // the same graphics, such as an SVG document that JSVG draws, would inherit it.
    val content    = Rectangle(20.5, 10.25, Pos.Origin, Some(FillStyle(Color(255, 0, 0))), None)
    val (box, ops) = DrawPlan.of(content.wrapShift(5, 2), DeviceGrid(1.0), settings, 0L)
    val tiles      = ops.collect { case DrawOp.HardClip(_, inner) => inner }.flatten.collect {
      case tiles: DrawOp.Tiles => tiles
    }
    assertEquals(tiles.size, 1)
    val image   = new BufferedImage(box.width, box.height, BufferedImage.TYPE_INT_ARGB)
    val g       = image.createGraphics()
    val key     = RenderingHints.KEY_INTERPOLATION
    val bicubic = RenderingHints.VALUE_INTERPOLATION_BICUBIC
    try
      g.setRenderingHint(key, bicubic)
      new JVMPlanRenderer.Painter(settings, new JVMPlanRenderer.LayerPool(1000000), 0L, 1.0)
        .drawAll(g, Vector(), tiles, -box.left, -box.top, box)
      assertEquals(g.getRenderingHint(key), bicubic)
    finally g.dispose()
  }

  for d <- DeepTrees.nested; scale <- Seq(1.0, 1.5) do
    test(s"${d.name} draws the same at any nesting limit at $scale") {
      val tree                  = d.build(40)
      val (width, height, argb) = render(tree, scale, nesting = Int.MaxValue)
      for nesting <- Seq(1, 2, 16) do
        val (w, h, bounded) = render(tree, scale, nesting)
        assertEquals((w, h), (width, height), s"at nesting $nesting")
        assert(bounded.sameElements(argb), s"the pixels differ at nesting $nesting")
    }

  /** Fifty chains of 200 masks of masks, in a grid of 10 by 5. */
  private def fiftyMaskChains: Element = Picture(
    (0 until 50).map(i => DeepTrees.masksOfMasks.build(200).moveBy(i % 10 * 50.0, i / 10 * 50.0))*
  )

  test("fifty deep mask chains side by side draw as they did") {
    val chains                = fiftyMaskChains
    val (width, height, argb) = render(chains)
    // 200 levels of recursion still fit on the test thread's stack.
    val (w, h, unbounded) = render(chains, nesting = Int.MaxValue)
    assertEquals((width, height), (w, h))
    assert(argb.sameElements(unbounded), "the pixels differ")
  }

  test("the buffers of slots that no operation composites return to the pool") {
    val plan = DrawPlan.planned(fiftyMaskChains, DeviceGrid(1.0), settings, 0L)
    // The count of the slots that the plan's own operations composite. Drawing no operations
    // leaves them to the painter's finally block. Each is held until then, so each is a buffer of
    // its own.
    val waiting = PlanListing.listed(plan.ops).count(_._1.isInstanceOf[DrawOp.Prepared])
    assert(waiting > 1, s"$waiting slots wait for the operations")
    val pool  = new JVMPlanRenderer.LayerPool(1000000)
    val image = new BufferedImage(plan.box.width, plan.box.height, BufferedImage.TYPE_INT_ARGB)
    val g     = image.createGraphics()
    try
      new JVMPlanRenderer.Painter(settings, pool, 0L, 1.0).drawAll(
        g,
        plan.slots,
        Vector(),
        -plan.box.left,
        -plan.box.top,
        plan.box
      )
    finally g.dispose()
    assert(pool.keptCount >= waiting, s"${pool.keptCount} buffers kept, $waiting waited")
  }

  /** A `width` by `height` bitmap of varied colors, opaque or translucent, made fresh, so that no
    * raster of it is kept yet.
    */
  private def varied(width: Int, height: Int, translucent: Boolean): Bitmap =
    val argb = Array.tabulate(width * height): i =>
      val (x, y) = (i % width, i / width)
      val alpha  = if translucent then 20 + 19 * ((x + 2 * y) % 12) else 255
      (alpha << 24) | ((37 * x) % 256 << 16) | ((53 * y) % 256 << 8) | ((x + y) * 29 % 256)
    Bitmap.fromARGB(width, height, argb)

  /** The packed ARGB pixels of `e` rendered at `scale` with `drawing`. */
  private def pixelsOf(e: Element, scale: Double, drawing: RenderSettings): Vector[Int] =
    JVMPlanRenderer.render(e, scale, drawing, 0L).buffer(drawing, 0L).argbData.toVector

  /** How many more rasters the shared cache keeps after `body` than before it, and what `body`
    * gave.
    */
  private def keptBy[A](body: => A): (Int, A) =
    val before = RasterCache.shared.keptCount
    val result = body
    (RasterCache.shared.keptCount - before, result)

  /** Pictures of a bitmap at its own size, `dx` and `dy` device pixels from its place at `scale`.
    * The bitmap is alone, over a translucent backdrop, under a crop of both, and cropped itself.
    */
  private def scenes(scale: Double, dx: Int, dy: Int): Seq[(String, Bitmap => Element)] =
    def placed(b: Bitmap): Element = b.moveBy(dx / scale, dy / scale)
    val backdrop = Rectangle(40, 30, Pos.Origin, Some(FillStyle(Color(0, 128, 255, 100))), None)
    Seq(
      "alone"                  -> placed,
      "over a backdrop"        -> (b => Picture(placed(b), backdrop)),
      "under a crop"           -> (b => Picture(placed(b), backdrop).crop(Bounds(-3, -2, 4, 3))),
      "under a crop of itself" -> (b => placed(b).crop(Bounds(-3, -2, 4, 3)))
    )

  test("a bitmap drawn from its kept scaled raster has the pixels of one drawn directly") {
    // Drawing at the first ask, which keeps no raster, at the second, which makes one and keeps
    // it, at later asks from it, from one made again and from one not kept gives the pixels of
    // drawing the bitmap directly. The directly drawn bitmap's pixels have escaped and so are
    // never kept scaled. The crops start inside the bitmap, where Java2D starts stepping through
    // the source positions.
    val nearest    = settings.copy(bufferScalingMethod = smile.ScalingMethod.NearestNeighbor)
    val hints      = Seq(ResamplingHint.Auto, ResamplingHint.Smooth, ResamplingHint.Pixelated)
    val keptFirst  = scala.collection.mutable.Map[Double, Int]().withDefaultValue(0)
    val keptSecond = scala.collection.mutable.Map[Double, Int]().withDefaultValue(0)
    val keptLater  = scala.collection.mutable.Map[Double, Int]().withDefaultValue(0)
    val differing  =
      for
        translucent      <- Seq(false, true)
        scale            <- Seq(2.0, 4.0)
        (dx, dy)         <- Seq((0, 0), (3, 6), (-9, 3), (7, -1), (-4, -5), (12, 8))
        hint             <- hints
        drawing          <- Seq(settings, nearest)
        (label, sceneOf) <- scenes(scale, dx, dy)
        problem          <-
          def fresh            = varied(12, 8, translucent).withResampling(hint)
          def drawn(b: Bitmap) = pixelsOf(sceneOf(b), scale, drawing)
          val escaped          = fresh
          val _                = escaped.nativeBuffer(drawing).get
          val direct           = drawn(escaped)
          val bitmap           = fresh
          val (kept1, first)   = keptBy(drawn(bitmap))
          val (kept2, second)  = keptBy(drawn(bitmap))
          val (kept3, later)   = keptBy((drawn(bitmap), drawn(bitmap)))
          keptFirst(scale) += kept1
          keptSecond(scale) += kept2
          keptLater(scale) += kept3
          RasterCache.shared.withBudget(0)(())
          val emptied = drawn(bitmap)
          val none    = RasterCache.shared.withBudget(0):
            val unkept = fresh
            (drawn(unkept), drawn(unkept))
          Seq(
            "first"           -> first,
            "second"          -> second,
            "third"           -> later(0),
            "fourth"          -> later(1),
            "emptied"         -> emptied,
            "none, the first" -> none(0),
            "none, the next"  -> none(1)
          ).collect:
            case (phase, pixels) if pixels != direct =>
              val wrong = pixels.indices.count(i => pixels(i) != direct(i))
              s"$label, translucent $translucent, at $scale moved ($dx, $dy), $hint, " +
                s"${drawing.bufferScalingMethod}: $phase differs in $wrong pixels"
      yield problem
    assert(differing.isEmpty, differing.mkString("\n"))
    for scale <- Seq(2.0, 4.0) do
      assertEquals(keptFirst(scale), 0, s"a raster was kept at the first ask at $scale")
      assert(keptSecond(scale) > 0, s"no scaled raster was kept at $scale, so none was drawn from")
      assertEquals(keptLater(scale), 0, s"a later ask at $scale kept another raster")
  }

  test("a bitmap drawn once keeps no scaled raster, and one drawn again keeps one") {
    val bitmap         = varied(12, 8, translucent = true)
    val (first, drawn) = keptBy(pixelsOf(bitmap, 2.0, settings))
    assertEquals(first, 0, "the first rendering kept a raster")
    val (second, again) = keptBy(pixelsOf(bitmap, 2.0, settings))
    assertEquals(second, 1, "the second rendering kept no raster")
    val (third, later) = keptBy(pixelsOf(bitmap, 2.0, settings))
    assertEquals(third, 0, "the third rendering kept another raster")
    assertEquals(again, drawn)
    assertEquals(later, drawn)
  }

  test("a bitmap made anew for every frame keeps no scaled raster") {
    val (kept, _) = keptBy:
      for frame <- 0 until 5 do pixelsOf(varied(12, 8 + frame, translucent = true), 2.0, settings)
    assertEquals(kept, 0)
  }

  test("a bitmap asked for under another key between two asks keeps no raster yet") {
    // The buffer remembers only the most recent raster key.
    val bitmap    = varied(12, 8, translucent = true)
    val nearest   = settings.copy(bufferScalingMethod = smile.ScalingMethod.NearestNeighbor)
    val (kept, _) = keptBy:
      Seq(settings, nearest, settings).map(pixelsOf(bitmap, 2.0, _))
    assertEquals(kept, 0)
    assertEquals(keptBy(pixelsOf(bitmap, 2.0, settings))._1, 1)
  }

  /** How many more rasters the shared cache keeps after `e` is rendered twice at `scale`. */
  private def keptByTwo(e: Element, scale: Double): Int =
    keptBy(Seq.fill(2)(pixelsOf(e, scale, settings)))._1

  test("a bitmap off the grid or scaled by the program keeps no raster") {
    val bitmap = varied(12, 8, translucent = true)
    for (label, e) <- Seq(
        "at a fractional position" -> bitmap.moveBy(0.25, 0),
        "scaled"                   -> bitmap.scaleBy(1.25),
        "turned"                   -> bitmap.rotateBy(30)
      )
    do assertEquals(keptByTwo(e, 2.0), 0, label)
  }

  test("a bitmap keeps no raster at 1x, below it or at a density that is not a power of two") {
    // At 1.5 and 3, a raster kept would differ from drawing directly under a crop that starts
    // inside the bitmap.
    for
      scale            <- Seq(0.5, 1.0, 1.5, 3.0)
      (label, sceneOf) <- scenes(scale, 0, 0)
    do
      val kept = keptByTwo(sceneOf(varied(12, 8, translucent = true)), scale)
      assertEquals(kept, 0, s"$label at $scale")
  }

  test("a raster over a quarter of the budget is not kept") {
    // A 12 x 8 bitmap at 2x is a raster of 24 x 16 pixels, 1536 bytes.
    val bitmap = varied(12, 8, translucent = false)
    // Emptied first, so that no raster another test kept goes when this one is kept.
    RasterCache.shared.withBudget(0)(())
    RasterCache.shared.withBudget(4 * 1536 - 4):
      assertEquals(keptByTwo(bitmap, 2.0), 0)
    RasterCache.shared.withBudget(4 * 1536):
      assertEquals(keptByTwo(bitmap, 2.0), 1)
  }
end JVMPlanRendererSuite
