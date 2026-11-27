package smile.infrastructure

import org.scalajs.dom
import smile.colors.Color
import smile.modeling.{Bounds, Pos}
import smile.pictures.{Bitmap, Element, Picture, PixelSource, Rasters, asBitmap}

/** A buffer's pixels live in one place, which filled it. That is an array for a buffer filled with
  * its setters and a canvas for one drawn. The other is made from it at most once, so a canvas is
  * never read back twice. The PNG a bitmap's SVG embeds is encoded once per state of its pixels. It
  * is reused while they stay the same and is never stale after any way of changing them.
  */
class JSBufferAdapterSuite extends munit.FunSuite:

  private val red  = Color(255, 0, 0)
  private val blue = Color(0, 0, 255)

  private def redBuffer(): JSBufferAdapter =
    val buffer = JSBufferAdapter(4, 3)
    buffer.setARGBData(Array.fill(4 * 3)(red.toARGBInt))
    buffer

  private def bitmapOf(buffer: JSBufferAdapter): Bitmap =
    Rasters.of(PixelSource.fromBuffer(buffer, Bounds(Pos(0, 0), buffer.width, buffer.height)))

  /** The `href` of the one `image` an SVG of the bitmap holds. */
  private def svgPng(
      bitmap: Element,
      settings: smile.RenderSettings = smile.RenderSettings.Default
  ): String =
    JSRenderer
      .createSvgFrom(bitmap, settings)
      .querySelector("image")
      .getAttributeNS("http://www.w3.org/1999/xlink", "href")

  /** What the buffer's pixels encode to right now, bypassing the cache. */
  private def freshPng(buffer: JSBufferAdapter): String =
    buffer.readOnlyCanvas.toDataURL("image/png")

  /** A buffer drawn red, with a blue pixel in its corner. */
  private def drawnBuffer(): JSBufferAdapter =
    JSBufferAdapter.drawn(4, 3): ctx =>
      ctx.fillStyle = red.toString
      ctx.fillRect(0, 0, 4, 3)
      ctx.fillStyle = blue.toString
      ctx.fillRect(0, 0, 1, 1)

  test("a stored buffer builds its canvas once") {
    val buffer = redBuffer()
    assert(buffer.readOnlyCanvas eq buffer.readOnlyCanvas)
  }

  test("reading a stored buffer never touches its canvas") {
    val buffer = redBuffer()
    val _      = buffer.readOnlyCanvas
    for _ <- 1 to 100 do assertEquals(buffer.pixelColor(1, 1), red)
    assertEquals(buffer.argbData.toSeq, Seq.fill(4 * 3)(red.toARGBInt))
    assertEquals(buffer.readBacks, 0)
  }

  test("a drawn buffer is read back once, on the first read") {
    val buffer = drawnBuffer()
    val target = JSBufferAdapter.drawn(4, 3)(_.drawImage(buffer.readOnlyCanvas, 0, 0))
    assertEquals(buffer.readBacks, 0)
    assertEquals(buffer.pixelColor(0, 0), blue)
    assertEquals(buffer.readBacks, 1)
    assertEquals(buffer.argbData.toSeq, blue.toARGBInt +: Seq.fill(4 * 3 - 1)(red.toARGBInt))
    assertEquals(buffer.pixelColor(3, 2), red)
    assertEquals(buffer.readBacks, 1)
    assertEquals(target.argbData.toSeq, buffer.argbData.toSeq)
  }

  test("a new canvas is a copy of the pixels each time") {
    for buffer <- Seq(redBuffer(), drawnBuffer()) do
      val first  = buffer.newCanvas
      val second = buffer.newCanvas
      assert(first ne second)
      assert(first ne buffer.readOnlyCanvas)
      first.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D].clearRect(0, 0, 4, 3)
      assertEquals(second.toDataURL("image/png"), freshPng(buffer))
  }

  test("an unchanged bitmap is encoded once, however many wrappers share its buffer") {
    val buffer = redBuffer()
    val bitmap = bitmapOf(buffer)

    val first = svgPng(bitmap)
    assertEquals(first, freshPng(buffer))
    assertEquals(svgPng(bitmap), first)
    assertEquals(svgPng(bitmap.moveBy(10, 5)), first)
    assertEquals(buffer.pngEncodingCount, 1)
  }

  test("a scaled bitmap replays its pixels once and is encoded once, however often it is drawn") {
    val stepwise = smile.RenderSettings(resamplingMode = smile.ResamplingMode.Stepwise)
    val scaled   = bitmapOf(redBuffer()).scaleBy(2, 2).asBitmap
    val first    = svgPng(scaled, stepwise)
    val pixels   = scaled.nativeBuffer(stepwise)

    assertEquals(svgPng(scaled, stepwise), first)
    assertEquals(svgPng(scaled.moveBy(10, 5), stepwise), first)
    assertEquals(pixels.pngEncodingCount, 1)
  }

  test("a bitmap drawn directly embeds its source, encoded once, however it is scaled and moved") {
    val buffer = redBuffer()
    val source = bitmapOf(buffer)
    val first  = svgPng(source.scaleBy(2, 2))

    // Resampled, the source is embedded with a border that repeats its edge pixels.
    assertEquals(first, buffer.paddedCanvas.toDataURL("image/png"))
    assertEquals(svgPng(source.scaleBy(2, 2).moveBy(10, 5)), first)
    assertEquals(svgPng(source.scaleBy(3, 4).moveBy(-2, 1)), first)
    assertEquals(buffer.pngEncodingCount, 1)
  }

  private val mutations: Seq[(String, JSBufferAdapter => Unit)] = Seq(
    "setARGBData"         -> (_.setARGBData(Array.fill(4 * 3)(blue.toARGBInt))),
    "setRGBA"             -> (_.setRGBA(1, 1, blue)),
    "setARGBByLocation"   -> (_.setARGBByLocation((x, _) => if x == 0 then blue.toARGBInt else 0)),
    "setColorsByLocation" -> (_.setColorsByLocation((_, y) => if y == 0 then blue else red)),
    "setColorsFromSeq"    -> (_.setColorsFromSeq(Seq.fill(4 * 3)(blue)))
  )

  private val kinds: Seq[(String, () => JSBufferAdapter)] =
    Seq("stored" -> (() => redBuffer()), "drawn" -> (() => drawnBuffer()))

  for (name, mutate) <- mutations; (kind, make) <- kinds do
    test(s"a change through $name of a $kind buffer shows in the next canvas and SVG") {
      val buffer = make()
      val bitmap = bitmapOf(buffer)
      val before = svgPng(bitmap)

      mutate(buffer)

      val after = svgPng(bitmap.moveBy(1, 1))
      assertNotEquals(after, before)
      assertEquals(after, freshPng(buffer))
      assertEquals(buffer.newCanvas.toDataURL("image/png"), after)
    }

  test("a buffer drawn into by the renderer is encoded with what was drawn") {
    val picture = Picture(bitmapOf(redBuffer()))
    val drawn   = JSRenderer.createBitmapFrom(picture)
    val buffer  = drawn.nativeBuffer()

    assertEquals(svgPng(drawn), freshPng(buffer))
    // The bitmap's corner lies at -1.5, which snaps to the whole pixel at -1, the rendering's
    // second row.
    assertEquals(drawn.pixels().colorAt(0, 1).get, red)
  }
end JSBufferAdapterSuite
