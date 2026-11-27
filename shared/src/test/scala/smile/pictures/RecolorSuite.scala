package smile.pictures

import scala.collection.mutable.ArrayBuffer

import smile.{RenderSettings, ResamplingMode, TransformMethod}
import smile.colors.{Color, LinearGradient}
import smile.infrastructure.{Renderer, Traversal}
import smile.modeling.{Bounds, Pos}
import smile.testing.Rasterizer

/** Recoloring a picture calls the color function exactly as a bitmap's own color operation does,
  * and gives exactly its pixels. Where the function maps every color the picture paints with
  * consistently, and the recolored picture renders to those same pixels, the result keeps the
  * recolored picture as its source, so that a `Direct` output draws it as sharply as the picture.
  */
class RecolorSuite extends munit.FunSuite:

  /** No antialiasing, normalized strokes and stepwise resampling. */
  private val stepwiseSettings = RenderSettings(
    drawingIsAntiAliased = false,
    normalizesStrokes = true,
    bufferScalingMethod = Rasterizer.replayScaling,
    bufferTransformMethod = TransformMethod.Bilinear,
    resamplingMode = ResamplingMode.Stepwise
  )

  private val red   = Color(255, 0, 0)
  private val blue  = Color(0, 0, 255)
  private val green = Color(0, 160, 0)

  /** Swaps the red and blue channels. */
  private val swap: Int => Int = argb =>
    (argb & 0xff00ff00) | ((argb >>> 16) & 0xff) | ((argb & 0xff) << 16)

  private def argb(pixels: Pixels): Vector[Int] = pixels.argbData.toVector

  /** The pixels of today's color operation, which renders the picture and transforms each pixel. */
  private def today(picture: Element, f: Int => Int, settings: RenderSettings): Vector[Int] =
    argb(picture.toBitmap(settings).transformARGB(f, settings).pixels())

  private def isSeeded(bitmap: Bitmap): Boolean = bitmap.source.source match
    case BitmapSource.Content(_) => bitmap.source.checkpoint.isDefined
    case _                       => false

  private def contentOf(bitmap: Bitmap): Element = bitmap.source.source match
    case BitmapSource.Content(picture) => picture
    case other                         => fail(s"Expected a picture source, was $other")

  private def render(picture: Element, scale: Double, settings: RenderSettings): Vector[Int] =
    argb(Renderer().createBitmapFrom(picture, scale, settings).pixels(settings))

  /** A red circle well inside a blue square, which fills the frame. */
  private def circleOverSquare(circle: Color, square: Color): Element =
    Picture(
      Circle(Pos(20, 20), 11.3, fillWith(circle), None),
      Rectangle(40, 40, Pos(20, 20), fillWith(square), None)
    )

  /** The parts of a picture, front first: its children, or the element itself. */
  private def partsOf(e: Element): Vector[Element] = e match
    case picture: Picture => picture.children
    case other            => Vector(other)

  /** `e`, which an operation of a bitmap's own gives, as the bitmap it is. */
  private def asBitmap(e: Element): Bitmap = e match
    case bitmap: Bitmap => bitmap
    case other          => fail(s"Expected a bitmap, was $other")

  test("a channel swap keeps the shapes, drawn sharply at scale 2 and exact in Stepwise") {
    val picture = circleOverSquare(red, blue)
    val result  = Recolor(picture, swap, stepwiseSettings)

    assert(isSeeded(result), "the result keeps the recolored picture")
    assertEquals(argb(result.pixels(stepwiseSettings)), today(picture, swap, stepwiseSettings))
    // Another settings renders the recolored picture with them.
    val antialiased = stepwiseSettings.copy(drawingIsAntiAliased = true)
    assertEquals(argb(result.pixels(antialiased)), today(picture, swap, antialiased))
    assertEquals(result.bounds, picture.toBitmap(stepwiseSettings).bounds)

    // Drawn directly at scale 2, the result is the recolored circle's own antialiased render.
    val direct   = render(result, 2, RenderSettings.Default)
    val expected = render(circleOverSquare(blue, red), 2, RenderSettings.Default)
    assertEquals(direct.length, expected.length)
    val differing = direct.indices.count: i =>
      (0 to 24 by 8).exists(shift =>
        (((direct(i) >>> shift) & 0xff) - ((expected(i) >>> shift) & 0xff)).abs > 2
      )
    assertEquals(differing, 0)
    val blended = direct.count: pixel =>
      ((pixel >>> 16) & 0xff) > 0 && (pixel & 0xff) > 0
    assert(blended > 40, s"antialiased edge pixels: $blended")
  }

  test("the function is called once per pixel of the 1x render, in row order") {
    val pictures = Seq(
      circleOverSquare(red, blue),
      // A translucent overlap, which falls back.
      Picture(
        Rectangle(10, 10, Pos(8, 8), fillWith(Color(255, 0, 0, 128)), None),
        Rectangle(10, 10, Pos(12, 12), fillWith(blue), None)
      ).crop(Bounds(Pos(0, 0), Pos(25, 20)))
    )
    val f: Int => Int = pixel => swap(pixel) ^ 0x00000100
    for picture <- pictures do
      val inputs = ArrayBuffer.empty[Int]
      val result = Recolor(
        picture,
        pixel =>
          inputs += pixel; f(pixel)
        ,
        stepwiseSettings
      )
      val rendered = picture.toBitmap(stepwiseSettings).pixels()
      assertEquals(inputs.length, rendered.width * rendered.height)
      assertEquals(inputs.toVector, argb(rendered))
      assertEquals(argb(result.pixels(stepwiseSettings)), today(picture, f, stepwiseSettings))
  }

  test("a function that answers differently for two neighboring pixels of one color falls back") {
    val picture = circleOverSquare(red, blue)
    // The first two pixels of the top row are both of the square.
    val odd           = swap(blue.toARGBInt) ^ 0x00000100
    var calls         = 0
    val f: Int => Int = pixel =>
      calls += 1
      if calls == 2 then odd else swap(pixel)
    val result = Recolor(picture, f, stepwiseSettings)
    assert(!isSeeded(result))
    val expected = argb(picture.toBitmap(stepwiseSettings).pixels()).map(swap).updated(1, odd)
    assertEquals(argb(result.pixels(stepwiseSettings)), expected)
  }

  test("a color seen in two separate runs maps once and keeps the shapes") {
    def stripes(a: Color, b: Color): Element = Picture(
      Rectangle(30, 4, Pos(15, 2), fillWith(a), None),
      Rectangle(30, 4, Pos(15, 6), fillWith(b), None),
      Rectangle(30, 4, Pos(15, 10), fillWith(a), None)
    )
    val picture = stripes(red, blue)
    val result  = Recolor(picture, swap, stepwiseSettings)
    assert(isSeeded(result), "the result keeps the recolored picture")
    assertEquals(argb(result.pixels(stepwiseSettings)), today(picture, swap, stepwiseSettings))
    // Drawn directly at scale 2, the result is the recolored stripes' own render.
    val direct = render(result, 2, RenderSettings.Default)
    assertEquals(direct.length, 4 * 30 * 12)
    assertEquals(direct, render(stripes(blue, red), 2, RenderSettings.Default))

    // A function with one answer for every color still keeps each color in the table.
    val black = Recolor(picture, _ => 0xff000000, stepwiseSettings)
    assert(isSeeded(black), "the result keeps the recolored picture")
    assertEquals(argb(black.pixels(stepwiseSettings)), Vector.fill(30 * 12)(0xff000000))
  }

  test("a bitmap of pixels, an animation and an empty picture are recolored without a table") {
    val still    = circleOverSquare(red, blue)
    val pictures = Seq[Element](
      Bitmap.generateARGB(6, 4, (x, y) => 0xff000000 | (x * 40 << 16) | y * 60),
      AnimatedPicture(Seq(still, still), Seq(100, 100), startTime = 0),
      Picture.empty
    )
    // A function that answers differently on every call, as no table could keep.
    def counting(calls: Array[Int]): Int => Int = pixel =>
      calls(0) += 1
      swap(pixel) ^ calls(0)
    for picture <- pictures do
      val calls    = Array(0)
      val result   = Recolor(picture, counting(calls), stepwiseSettings)
      val rendered = picture.toBitmap(stepwiseSettings).pixels()
      assertEquals(calls(0), rendered.width * rendered.height)
      assert(!isSeeded(result))
      val expected =
        picture.toBitmap(stepwiseSettings).transformARGB(counting(Array(0)), stepwiseSettings)
      assertEquals(argb(result.pixels(stepwiseSettings)), argb(expected.pixels(stepwiseSettings)))
  }

  test("a picture of one bitmap of pixels is recolored as its pixels, as before") {
    val bitmap = Bitmap.generateARGB(6, 4, (x, y) => 0xff000000 | (x * 40 << 16) | y * 60)
    val calls  = ArrayBuffer.empty[Int]
    val result = Recolor(
      bitmap,
      pixel =>
        calls += pixel; swap(pixel)
      ,
      stepwiseSettings
    )
    assert(!isSeeded(result))
    assertEquals(calls.toVector, argb(bitmap.pixels()))
    assertEquals(
      argb(result.pixels(stepwiseSettings)),
      argb(bitmap.transformARGB(swap, stepwiseSettings).pixels())
    )
  }

  test("a random function falls back to its pixels") {
    def randomly: Int => Int =
      val random = new scala.util.Random(7)
      _ => random.nextInt()
    val picture = circleOverSquare(red, blue)
    val result  = Recolor(picture, randomly, stepwiseSettings)
    assert(!isSeeded(result))
    assertEquals(argb(result.pixels(stepwiseSettings)), today(picture, randomly, stepwiseSettings))
    assertEquals(result.resampling, ResamplingHint.Auto)
  }

  test("an animation under a mask or a wrap falls back to its pixels") {
    val still     = circleOverSquare(red, blue)
    val animation = AnimatedPicture(Seq(still, still), Seq(100, 100), startTime = 0)
    val cover     = Rectangle(40, 40, Pos(20, 20), fillWith(green), None)
    val around    = Seq[Element => Element](
      MaskGroup(cover, _),
      MaskGroup(_, still),
      _.wrapShift(3, 2)
    )
    for place <- around do
      // The same picture with the frame in place of the animation keeps its shapes.
      assert(isSeeded(Recolor(place(still), swap, stepwiseSettings)))
      val result = Recolor(place(animation), swap, stepwiseSettings)
      assert(!isSeeded(result))
      assertEquals(
        argb(result.pixels(stepwiseSettings)),
        today(place(animation), swap, stepwiseSettings)
      )
  }

  test("a translucent overlap falls back to its pixels") {
    val picture = Picture(
      Rectangle(10, 10, Pos(8, 8), fillWith(Color(255, 0, 0, 128)), None),
      Rectangle(10, 10, Pos(12, 12), fillWith(blue), None)
    ).crop(Bounds(Pos(0, 0), Pos(25, 20)))
    // Squaring the red channel does not commute with blending.
    val squareRed: Int => Int = pixel =>
      val r = (pixel >>> 16) & 0xff
      (pixel & 0xff00ffff) | ((r * r / 255) << 16)
    val result = Recolor(picture, squareRed, stepwiseSettings)
    assert(!isSeeded(result))
    assertEquals(argb(result.pixels(stepwiseSettings)), today(picture, squareRed, stepwiseSettings))
  }

  test("a function that lowers alpha, which would reveal a hidden shape, falls back") {
    val picture = Picture(
      Rectangle(20, 20, Pos(10, 10), fillWith(blue), None),
      Rectangle(10, 10, Pos(10, 10), fillWith(red), None)
    )
    val halfAlpha: Int => Int = pixel => (pixel & 0x00ffffff) | 0x80000000
    val result                = Recolor(picture, halfAlpha, stepwiseSettings)
    assert(!isSeeded(result))
    assertEquals(argb(result.pixels(stepwiseSettings)), today(picture, halfAlpha, stepwiseSettings))
  }

  test("a gradient of 5000 colors falls back to its pixels") {
    val stops    = 40
    val colors   = (0 until stops).map(i => Color((i * 97) % 256, (i * 57) % 256, (i * 31) % 256))
    val gradient = LinearGradient(
      Pos(0, 0),
      Pos(6000, 0),
      (0 until stops).map(i => i.toDouble / (stops - 1)).zip(colors)
    )
    val picture = Rectangle(6000, 2, Pos(3000, 1), fillWith(gradient), None)
    val result  = Recolor(picture, swap, stepwiseSettings)
    assert(!isSeeded(result))
    assertEquals(argb(result.pixels(stepwiseSettings)), today(picture, swap, stepwiseSettings))
  }

  test("more than 4096 colors fall back, even when they would map") {
    def withColors(count: Int): Element =
      val raster = Bitmap.generateARGB(100, 50, (x, y) => 0xff000000 | ((y * 100 + x) % count) * 3)
      Picture(raster).crop(raster.bounds.get)
    val many = withColors(5000)
    val few  = withColors(4000)
    assert(!isSeeded(Recolor(many, swap, stepwiseSettings)))
    assertEquals(
      argb(Recolor(many, swap, stepwiseSettings).pixels(stepwiseSettings)),
      today(many, swap, stepwiseSettings)
    )
    assert(isSeeded(Recolor(few, swap, stepwiseSettings)))
    assertEquals(
      argb(Recolor(few, swap, stepwiseSettings).pixels(stepwiseSettings)),
      today(few, swap, stepwiseSettings)
    )
  }

  test("a visible color for transparent pixels puts a rectangle of it behind the content") {
    val white   = 0xffffffff
    val picture = Rectangle(10, 10, Pos(5, 5), fillWith(red), None)
      .crop(Bounds(Pos(0, 0), Pos(20, 20)))
    val f: Int => Int = pixel => if pixel == 0 then white else swap(pixel)
    val result        = Recolor(picture, f, stepwiseSettings)

    assert(isSeeded(result))
    assertEquals(argb(result.pixels(stepwiseSettings)), today(picture, f, stepwiseSettings))
    // The rebuilt picture is clipped to the frame of the render.
    val rebuilt = contentOf(result) match
      case clip: Clip => clip.child
      case other      => other
    val backgrounds = partsOf(rebuilt).collect:
      case shape: Shape if shape.fill.exists(_.paint == new Color(white)) => shape.bounds.get
    assertEquals(backgrounds, Seq(Bounds(Pos(0, 0), Pos(20, 20))))
    val direct = render(result, 1, RenderSettings.Default)
    assertEquals(direct(15 * 20 + 15), white)
    assertEquals(direct(5 * 20 + 5), blue.toARGBInt)
  }

  test("a recolored raster replays later operations on its observed pixels in Stepwise") {
    val picture  = circleOverSquare(red, blue)
    val seeded   = Recolor(picture, swap, stepwiseSettings)
    val fallback = picture.toBitmap(stepwiseSettings).transformARGB(swap, stepwiseSettings)
    assert(isSeeded(seeded))

    assertEquals(
      argb(asBitmap(seeded.scaleBy(1.5, 1.5)).pixels(stepwiseSettings)),
      argb(asBitmap(fallback.scaleBy(1.5, 1.5)).pixels(stepwiseSettings))
    )
    assertEquals(
      argb(seeded.rotateAround(30, Pos(3, 4)).pixels(stepwiseSettings)),
      argb(fallback.rotateAround(30, Pos(3, 4)).pixels(stepwiseSettings))
    )
    def placed(bitmap: Element): Element =
      bitmap.moveBy(10.5, 0).crop(Bounds(Pos(-5, -5), Pos(60, 50)))
    assertEquals(
      render(placed(seeded), 1, stepwiseSettings),
      render(placed(fallback), 1, stepwiseSettings)
    )
    assertEquals(
      render(placed(seeded.scaleBy(1.5, 1.5)), 1, stepwiseSettings),
      render(placed(fallback.scaleBy(1.5, 1.5)), 1, stepwiseSettings)
    )

  }

  test("the observed pixels are for the recolor's settings only") {
    // Another Stepwise output renders the recolored picture with its own settings.
    val picture     = circleOverSquare(red, blue)
    val seeded      = Recolor(picture, swap, stepwiseSettings)
    val antialiased = stepwiseSettings.copy(drawingIsAntiAliased = true)
    assert(isSeeded(seeded))
    val recolored = Bitmap.of(contentOf(seeded))
    // On the JVM these differ from the observed pixels, which are not antialiased. A canvas
    // antialiases either way.
    assertEquals(argb(seeded.pixels(antialiased)), argb(recolored.pixels(antialiased)))
    assertEquals(
      argb(asBitmap(seeded.scaleBy(1.5, 1.5)).pixels(antialiased)),
      argb(asBitmap(recolored.scaleBy(1.5, 1.5)).pixels(antialiased))
    )
  }

  test("paints cover fills, strokes, text, gradient stops and embedded rasters, not masks") {
    val gradient = LinearGradient(Pos(0, 0), Pos(10, 0), Color(1, 2, 3), Color(4, 5, 6))
    val raster   = Bitmap.fromARGB(2, 1, Array(0xff102030, 0xff405060))
    val text     = Text(
      Pos(0, 0),
      Some(Bounds(Pos(0, 0), Pos(8, 12))),
      "A",
      "Arial",
      12,
      400,
      fillWith(green),
      None
    )
    val picture = Picture(
      Rectangle(4, 4, Pos(0, 0), fillWith(red), lineWith(blue, 2, round = false)),
      Circle(Pos(0, 0), 3, fillWith(gradient), None),
      text,
      Point(Pos(1, 1), Color(9, 9, 9)),
      raster,
      MaskGroup(
        Circle(Pos(0, 0), 3, fillWith(Color(7, 7, 7)), None),
        Rectangle(2, 2, Pos(0, 0), fillWith(Color(8, 8, 8)), None)
      )
    )
    assertEquals(
      Traversal.paints(picture),
      Set(red, blue, Color(1, 2, 3), Color(4, 5, 6), green, Color(9, 9, 9), Color(8, 8, 8))
        .map(_.toARGBInt) ++ Set(0xff102030, 0xff405060)
    )

    val mapped = Traversal.mapPaints(picture, swap)
    assertEquals(Traversal.paints(mapped), Traversal.paints(picture).map(swap))
    val maskGroup = partsOf(mapped).collectFirst { case group: MaskGroup => group }.get
    assertEquals(maskGroup.masks.head._1.asInstanceOf[Shape].fill, fillWith(Color(7, 7, 7)))
    val mappedRaster = partsOf(mapped).collectFirst { case bitmap: Bitmap => bitmap }.get
    assertEquals(
      argb(mappedRaster.pixels()),
      argb(raster.transformARGB(swap, stepwiseSettings).pixels())
    )
    assertEquals(mapped.bounds, picture.bounds)
  }

  test("a picture with a raster of a few colors is recolored, raster and all") {
    val raster =
      Bitmap.generateARGB(8, 6, (x, y) => if (x + y) % 2 == 0 then 0xffff0000 else 0xff00ff00)
    val picture =
      Picture(raster.moveBy(3, 2), Rectangle(20, 12, Pos(8, 5), fillWith(blue), None))
    val result = Recolor(picture, swap, stepwiseSettings)
    assert(isSeeded(result))
    assertEquals(argb(result.pixels(stepwiseSettings)), today(picture, swap, stepwiseSettings))
  }

  test("recoloring the shapes verifies or falls back, and never changes a pixel") {
    val stroke                         = lineWith(green, 3, round = true)
    val shapes: Seq[(String, Element)] = Seq(
      "circle"         -> Circle(Pos(20, 20), 11.3, fillWith(red), None),
      "stroked circle" -> Circle(Pos(20, 20), 9, fillWith(red), stroke),
      "ellipse"        -> Ellipse(Pos(10, 10), 14, 6, fillWith(red), None),
      "rectangle"      -> Rectangle(17, 9, Pos(3, 4), fillWith(red), None),
      "square"         -> Rectangle(12, Pos(6.5, 6.5), fillWith(red), stroke),
      "triangle"       -> Triangle(10, Pos(100, 0), fillWith(red), None),
      "isosceles"      -> Triangle(14, 9, fillWith(red), stroke),
      "pentagon"       -> Pentagon(20, 20, Pos.Origin, fillWith(red), None),
      "star"           -> Star5(10, 4, Pos.Origin, fillWith(red), stroke),
      "arc"            -> Arc(Pos(5, 5), 20, 14, 30, 200, fillWith(red), None),
      "line"           -> Line(0, 0, 17, 9, StrokeStyle(blue, 2)),
      "gradient"       -> Rectangle(
        30,
        10,
        Pos.Origin,
        fillWith(LinearGradient(Pos(0, 0), Pos(30, 0), red, blue)),
        None
      ),
      "translucent" -> Circle(Pos(8, 8), 6, fillWith(Color(255, 0, 0, 100)), None),
      "point"       -> Point(Pos(2, 2), red)
    )
    val stepwise = RenderSettings(
      bufferScalingMethod = Rasterizer.replayScaling,
      resamplingMode = ResamplingMode.Stepwise
    )
    val outcomes =
      for
        settings      <- Seq(stepwiseSettings, RenderSettings.Default)
        (name, shape) <- shapes
        picture       <- Seq(
          shape,
          Picture(shape, Rectangle(50, 40, Pos(10, 5), fillWith(blue), None))
        )
      yield
        val result   = Recolor(picture, swap, settings)
        val observed = today(picture, swap, settings)
        assertEquals(argb(result.pixels(settings)), observed, name)
        // Other settings render the recolored picture, which only the result's settings verified.
        for other <- Seq(stepwise, stepwiseSettings) do
          val expected =
            if isSeeded(result) && other != settings then
              argb(Bitmap.of(contentOf(result)).pixels(other))
            else observed
          assertEquals(argb(result.pixels(other)), expected, name)
        assertEquals(result.bounds, picture.toBitmap(settings).bounds, name)
        isSeeded(result)
    val verified = outcomes.count(identity)
    // A canvas loses some of a translucent pixel's color when its pixels are written back, so
    // antialiased edges over nothing verify on the JVM only.
    assert(verified >= 10, s"only $verified of ${outcomes.length} verified")
  }

  /** How deep pictures nest inside bitmaps of pictures in `element`. */
  private def depth(element: Element): Int = element match
    case bitmap: Bitmap =>
      bitmap.source.source match
        case BitmapSource.Content(content) => 1 + depth(content)
        case BitmapSource.Pixels(_)        => 0
    case group: MaskGroup => (group.masked +: group.masks.map(_._1)).map(depth).max
    case picture: Picture => picture.children.map(depth).maxOption.getOrElse(0)
    case clip: Clip       => depth(clip.child)
    case _                => 0

  /** How many pixels the buffers held anywhere in `element` have, which is what it keeps alive. */
  private def heldPixels(element: Element): Long = element match
    case bitmap: Bitmap =>
      val checkpoint = bitmap.source.checkpoint.fold(0L): checkpoint =>
        checkpoint.pixels.width.toLong * checkpoint.pixels.height
      checkpoint + (bitmap.source.source match
        case BitmapSource.Content(content) => heldPixels(content)
        case BitmapSource.Pixels(buffer)   => buffer.width.toLong * buffer.height)
    case group: MaskGroup => (group.masked +: group.masks.map(_._1)).map(heldPixels).sum
    case picture: Picture => picture.children.map(heldPixels).sum
    case clip: Clip       => heldPixels(clip.child)
    case _                => 0L

  test("a picture recolored again and again stays one level deep and keeps its size") {
    val raster =
      Bitmap.generateARGB(8, 6, (x, y) => if (x + y) % 2 == 0 then 0xffff0000 else 0xff00ff00)
    val start =
      Picture(raster.moveBy(3, 2), Rectangle(40, 30, Pos(20, 15), fillWith(blue), None))
    val first = Recolor(start, swap, stepwiseSettings)
    assert(isSeeded(first))
    val held = heldPixels(first)

    val today1: Element  = start.toBitmap(stepwiseSettings).transformARGB(swap, stepwiseSettings)
    val (last, expected) = (2 to 50).foldLeft((first, today1)): (previous, _) =>
      val (bitmap, oracle) = previous
      val next             = Recolor(bitmap, swap, stepwiseSettings)
      assert(isSeeded(next), "every recolor keeps the picture")
      assertEquals(depth(next), 1, "nesting")
      assertEquals(heldPixels(next), held)
      (next, oracle.toBitmap(stepwiseSettings).transformARGB(swap, stepwiseSettings))
    assertEquals(
      argb(last.pixels(stepwiseSettings)),
      argb(expected.toBitmap(stepwiseSettings).pixels(stepwiseSettings))
    )
    val moved = Recolor(last.moveBy(7, 5), swap, stepwiseSettings)
    assert(isSeeded(moved), "a moved recolor is recolored as its picture, moved")
    assertEquals(depth(moved), depth(last))
    assertEquals(
      argb(moved.pixels(stepwiseSettings)),
      today(last.moveBy(7, 5), swap, stepwiseSettings)
    )
  }

  /** Each operation a bitmap records, in turn, all of which keep its pixels exact. */
  private val operations: Vector[Bitmap => Bitmap] = Vector(
    _.flipHorizontally,
    bitmap => asBitmap(bitmap.scaleBy(2, 2)),
    bitmap => bitmap.rotateAround(90, bitmap.bounds.get.center),
    bitmap => asBitmap(bitmap.scaleBy(0.5, 0.5)),
    bitmap => asBitmap(bitmap.crop(bitmap.bounds.get)),
    _.flipVertically
  )

  test("a picture recolored again and again with operations between stays bounded") {
    val raster =
      Bitmap.generateARGB(8, 6, (x, y) => if (x + y) % 2 == 0 then 0xffff0000 else 0xff00ff00)
    val start =
      Picture(raster.moveBy(3, 2), Rectangle(40, 30, Pos(20, 15), fillWith(blue), None))
    val first = Recolor(start, swap, stepwiseSettings)
    val held  = heldPixels(first)

    val today1           = start.toBitmap(stepwiseSettings).transformARGB(swap, stepwiseSettings)
    val (last, expected) = (0 until 60).foldLeft((first, today1)): (previous, tick) =>
      val (bitmap, oracle) = previous
      val operation        = operations(tick % operations.length)
      val next             = Recolor(operation(bitmap), swap, stepwiseSettings)
      // A canvas scales smoothly, which blends the checkered pixels, so the function never sees
      // their colors and the recolor keeps only its pixels.
      if Rasterizer.averagesAreas then assert(isSeeded(next), s"tick $tick keeps the picture")
      assert(depth(next) <= 2, s"tick $tick nested ${depth(next)} deep")
      assert(heldPixels(next) <= 8 * held, s"tick $tick holds ${heldPixels(next)} pixels")
      val today = operation(oracle).transformARGB(swap, stepwiseSettings)
      assertEquals(
        argb(next.pixels(stepwiseSettings)),
        argb(today.pixels(stepwiseSettings)),
        s"tick $tick"
      )
      (next, today)
    assertEquals(last.bounds, expected.bounds)
  }
  test("a picture recolored again and again among other elements nests at most 9 deep") {
    val square = Rectangle(10, 10, Pos(5, 5), fillWith(green), None)
    val start  = circleOverSquare(red, blue)
    val first  = Recolor(start, swap, stepwiseSettings)
    val (_, _) = (0 until 30).foldLeft((first, first.transformARGB(identity, stepwiseSettings))):
      (previous, tick) =>
        val (bitmap, oracle) = previous
        val next             = Recolor(Picture(square, bitmap), swap, stepwiseSettings)
        assert(depth(next) <= 9, s"tick $tick nested ${depth(next)} deep")
        val today =
          Picture(square, oracle).toBitmap(stepwiseSettings).transformARGB(swap, stepwiseSettings)
        assertEquals(
          argb(next.pixels(stepwiseSettings)),
          argb(today.pixels(stepwiseSettings)),
          s"tick $tick"
        )
        (next, today)
  }

  test("a chain of bitmaps that each draw the one before twice is walked once per level") {
    // Each level holds the level before twice, so a walk that follows every bitmap to its picture
    // walks 2 to the 60th pictures.
    val square = Rectangle(40, 40, Pos(20, 20), fillWith(green), None)
    val chain  = DeepTrees.lazyLevels(60, square)((a, _) => a.addToFront(a.moveBy(3, 2))).last
    assertEquals(Traversal.paints(chain), Set(green.toARGBInt))
    val result = Recolor(chain, swap, stepwiseSettings)
    // Nested deeper than a rebuilt picture may be, so the result is only the recolored pixels.
    assert(!isSeeded(result))
    assertEquals(argb(result.pixels(stepwiseSettings)), today(chain, swap, stepwiseSettings))
  }
end RecolorSuite
