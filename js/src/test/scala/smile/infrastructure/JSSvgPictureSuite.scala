package smile.infrastructure

import org.scalajs.dom
import smile.colors.Color
import smile.modeling.{Bounds, Pos}
import smile.RenderSettings
import smile.pictures.{
  Bitmap,
  BitmapSource,
  Element,
  Recolor,
  FillStyle,
  Picture,
  Rectangle,
  SvgPicture,
  SvgSanitizingSuite,
  ready,
  toBitmap
}

import java.io.IOException
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Future, Promise}
import scala.scalajs.js

class JSSvgPictureSuite extends munit.FunSuite:
  /** Completes once `condition` holds, polled every 10 ms, and fails after 10 seconds. */
  private def eventually(what: String)(condition: => Boolean): Future[Unit] =
    val outcome      = Promise[Unit]()
    val deadline     = System.currentTimeMillis() + 10000
    def poll(): Unit =
      if condition then outcome.success(())
      else if System.currentTimeMillis() > deadline then
        outcome.failure(new AssertionError(s"Not within 10 seconds: $what"))
      else
        val _ = dom.window.setTimeout(() => poll(), 10)
    poll()
    outcome.future

  /** A blue 4 by 4 square centered at `center`. */
  private def square(center: Pos) =
    Rectangle(4, 4, center, Some(FillStyle(Color(0, 0, 255))), None)

  private def asBitmap(element: Element): Bitmap = element match
    case bitmap: Bitmap => bitmap
    case other          => fail(s"$other is not a bitmap")

  private def isContent(bitmap: Bitmap): Boolean = bitmap.source.source match
    case BitmapSource.Content(_) => true
    case BitmapSource.Pixels(_)  => false

  /** Every pixel's color of `bitmap`. */
  private def colors(bitmap: Bitmap): Seq[Color] =
    for x <- 0 until bitmap.width.toInt; y <- 0 until bitmap.height.toInt
    yield bitmap.getColor(x, y)

  private val source =
    """<svg xmlns="http://www.w3.org/2000/svg" width="2" height="2" viewBox="0 0 2 2"><path fill="red" d="M0 0H.25V2H0Z"/></svg>"""

  test("SVG source remains vector after transformation and SVG export") {
    val original = SvgPicture.fromString(source)
    val changed  = original.scaleBy(16, 8).rotateAround(90, Pos.Origin).moveTo(Pos(100, 50))
    assertEquals(original.anchor, Pos.Origin)
    assertEquals(original.bounds.get, Bounds(-1, -1, 1, 1))
    assertEqualsDouble(changed.bounds.get.width, 16, 1e-8)
    assertEqualsDouble(changed.bounds.get.height, 32, 1e-8)
    assertEquals(changed.anchor, Pos(100, 50))
    val exported = JSRenderer.createSvgFrom(changed)
    val image    = exported.querySelector("image")
    val url      = image.getAttributeNS("http://www.w3.org/1999/xlink", "href")
    assert(url.startsWith("data:image/svg+xml;"))
    assert(js.URIUtils.decodeURIComponent(url.substring(url.indexOf(',') + 1)).contains("<path"))
    assert(!exported.outerHTML.contains("image/png"))
    // The document is defined once and drawn with a use element under the picture's affine.
    assert(exported.querySelector("use").getAttribute("transform").startsWith("matrix("))
  }

  test("world scaling and rotation agree with transformed viewport corners") {
    val svg    = SvgPicture.fromString(source).moveTo(Pos(5, 4))
    val result = svg.scaleAround(-2, 3, Pos.Origin).rotateAround(90, Pos.Origin)
    assertEqualsDouble(result.anchor.x, 12, 1e-8)
    assertEqualsDouble(result.anchor.y, 10, 1e-8)
    assertEqualsDouble(result.bounds.get.width, 6, 1e-8)
    assertEqualsDouble(result.bounds.get.height, 4, 1e-8)
  }

  test("viewBox supplies dimensions and nonzero origins remain in the SVG") {
    val svg = SvgPicture.fromString(
      """<svg xmlns="http://www.w3.org/2000/svg" viewBox="10 20 30 40"><rect x="10" y="20" width="30" height="40"/></svg>"""
    )
    assertEquals(svg.bounds.get, Bounds(-15, -20, 15, 20))
  }

  test("malformed documents and bad sizes are refused") {
    Seq(
      "<not-svg/>",
      """<svg xmlns="http://www.w3.org/2000/svg" viewBox="bad 0 2 2"/>""",
      """<svg xmlns="http://www.w3.org/2000/svg" width="0" height="2"/>"""
    ).foreach(svg => intercept[IOException](SvgPicture.fromString(svg)))
  }

  test("nonfinite transformations are rejected") {
    val svg = SvgPicture.fromString(source)
    val _   = intercept[IllegalArgumentException](svg.scaleBy(Double.NaN, 1))
    val _   = intercept[IllegalArgumentException](svg.moveTo(Pos(Double.PositiveInfinity, 0)))
    intercept[IllegalArgumentException](svg.rotateAround(Double.NaN, Pos.Origin))
  }

  test("fromString then toBitmap before ready draws nothing and does not throw") {
    val svg    = SvgPicture.fromString(source).scaleBy(8, 8)
    val bitmap = svg.toBitmap()
    assertEquals(svg.bounds, Some(Bounds(-8, -8, 8, 8)))
    assertEquals((bitmap.width, bitmap.height), (16.0, 16.0))
    for x <- 0 until 16; y <- 0 until 16 do assertEquals(bitmap.getColor(x, y).opacity, 0)
    assertEquals(JSRenderer.createBitmapFrom(svg).getColor(1, 10).opacity, 0)
  }

  test("decoding starts when the document is made") {
    val svg = SvgPicture.fromString(source)
    // Only renderings are watched. The picture's ready future is not, because asking for it would
    // start a lazy decoding itself.
    eventually("a rendering of the SVG drew all of it"):
      Completeness.tracked(JSRenderer.createBitmapFrom(svg))(1)
  }

  test("a lazy bitmap of an SVG not decoded yet keeps nothing, and shows the SVG once it is") {
    val svg = SvgPicture.fromString(source).scaleBy(8, 8)
    // The SVG in a picture in a bitmap, and that bitmap in a picture in another bitmap.
    val inner = Bitmap.of(Picture(svg, square(Pos(20, 0))))
    val outer = Bitmap.of(Picture(inner, square(Pos(40, 0))))
    for bitmap <- Seq(inner, outer) do
      val before = bitmap.source.renderedSourcePixelCount
      assertEquals(bitmap.getColor(1, 10).opacity, 0)
      assertEquals(JSRenderer.createBitmapFrom(bitmap).getColor(1, 10).opacity, 0)
      val once = bitmap.source.renderedSourcePixelCount
      assertEquals(bitmap.getColor(1, 10).opacity, 0)
      assert(once > before && bitmap.source.renderedSourcePixelCount > once, "it was kept")
    svg match
      case picture: SvgPicture =>
        picture.ready.map: _ =>
          for bitmap <- Seq(inner, outer) do
            val _    = bitmap.getColor(1, 10)
            val once = bitmap.source.renderedSourcePixelCount
            val _    = bitmap.getColor(1, 10)
            assertEquals(bitmap.source.renderedSourcePixelCount, once, "it was not kept")
          for bitmap <- Seq(inner, outer) do
            assert(bitmap.getColor(1, 10).opacity > 240)
            val kept    = JSRenderer.createBitmapFrom(bitmap)
            val emptied = RasterCache.shared.withBudget(0)(JSRenderer.createBitmapFrom(bitmap))
            assertEquals(colors(emptied), colors(kept))
            assert(kept.getColor(1, 10).opacity > 240)
            assert(JSRenderer.createBitmapFrom(bitmap, 2).getColor(2, 20).opacity > 240)
      case other => fail(s"$other is not an SVG picture")
  }

  test("a lazy bitmap of an SVG not decoded yet is not flattened into those pixels") {
    val svg = SvgPicture.fromString(source)
    // Alternating crops and wraparound shifts multiply the tiles past the cap of 8.
    def chain(start: Bitmap): Bitmap =
      (1 to 40).foldLeft(start): (raster, step) =>
        if step % 2 == 1 then
          val box = raster.bounds.get
          asBitmap(raster.crop(Bounds(box.upperLeftCorner, box.lowerRightCorner - (1.0, 0.0))))
        else asBitmap(raster.wrapShift(3, 1))
    val early = chain(Bitmap.of(Picture(svg.scaleBy(32, 32), square(Pos(40, 0)))))
    assert(isContent(early), "it was flattened into pixels that leave the SVG out")
    assert(early.source.plan.tiles.length > 8)
    svg.ready.map: _ =>
      val late = chain(Bitmap.of(Picture(svg.scaleBy(32, 32), square(Pos(40, 0)))))
      assert(!isContent(late), "a chain of whole pixels is flattened")
      assertEquals(colors(early), colors(late))
  }

  test("a recolor of an SVG not decoded yet takes no checkpoint of those pixels") {
    val svg     = SvgPicture.fromString(source).scaleBy(8, 8)
    val picture = Picture(svg, square(Pos(20, 0)))
    val green   = (argb: Int) => if argb == 0xff0000ff then 0xff00ff00 else argb
    val early   = Recolor(picture, green, RenderSettings.Default)
    assert(early.source.checkpoint.isEmpty, "it kept pixels that leave the SVG out")
    svg match
      case document: SvgPicture =>
        document.ready.map: _ =>
          val late = Recolor(picture, green, RenderSettings.Default)
          assert(late.source.checkpoint.isDefined, "a recolor of whole pixels keeps them")
          assert(late.getColor(1, 10).opacity > 240)
      case other => fail(s"$other is not an SVG picture")
  }

  test("browser renders subpixel SVG detail sharply after enlargement and at high DPI") {
    val svg = SvgPicture.fromString(source)
    svg.ready.map: _ =>
      val enlarged = svg.scaleBy(16, 16)
      val bitmap   = JSRenderer.createBitmapFrom(enlarged)
      assert(bitmap.getColor(1, 10).opacity > 240)
      assertEquals(bitmap.getColor(6, 10).opacity, 0)
      val dense = JSRenderer.createBitmapFrom(enlarged, 2)
      assert(dense.getColor(3, 20).opacity > 240)
      assertEquals(dense.getColor(12, 20).opacity, 0)
      assertEquals(svg.bounds.get, Bounds(-1, -1, 1, 1))
  }

  test("browser paints mirrored and rotated SVG geometry at its transformed bounds") {
    val svg = SvgPicture.fromString(source)
    svg.ready.map: _ =>
      val mirrored = JSRenderer.createBitmapFrom(svg.scaleBy(-16, 16))
      assert(mirrored.getColor(30, 10).opacity > 240)
      assertEquals(mirrored.getColor(20, 10).opacity, 0)
      val rotated =
        JSRenderer.createBitmapFrom(svg.scaleBy(16, 16).rotateAround(90, Pos.Origin))
      assert(rotated.getColor(10, 30).opacity > 240)
      assertEquals(rotated.getColor(10, 20).opacity, 0)
  }

  test("browser preserves SVG gradients and clip paths") {
    val svg = SvgPicture.fromString(
      """<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 8 8"><defs><linearGradient id="paint"><stop stop-color="red"/><stop offset="1" stop-color="blue"/></linearGradient><clipPath id="clip"><rect width="4" height="8"/></clipPath></defs><rect width="8" height="8" fill="url(#paint)" clip-path="url(#clip)"/></svg>"""
    )
    svg.ready.map: _ =>
      val bitmap = JSRenderer.createBitmapFrom(svg)
      assert(bitmap.getColor(1, 4).opacity > 240)
      assertEquals(bitmap.getColor(6, 4).opacity, 0)
      assert(bitmap.getColor(1, 4).red > bitmap.getColor(1, 4).blue)
  }

  test("SvgPicture(path) before load says to await load") {
    val error = intercept[UnsupportedOperationException](SvgPicture("never-loaded.svg"))
    assertEquals(
      error.getMessage,
      "SVG 'never-loaded.svg' has not been loaded. Browser loading is asynchronous: await " +
        "SvgPicture.load(\"never-loaded.svg\") first; afterwards SvgPicture(\"never-loaded.svg\") " +
        "serves the cached document."
    )
  }

  test("browser load gives a decoded picture, and SvgPicture(path) then serves its document") {
    assume(js.typeOf(js.Dynamic.global.fetch) != "undefined", "needs fetch, which jsdom lacks")
    val path = "data:image/svg+xml," + js.URIUtils.encodeURIComponent(source)
    SvgPicture
      .load(path)
      .map: svg =>
        assert(svg.document.ready.isCompleted)
        val bitmap = JSRenderer.createBitmapFrom(svg.scaleBy(16, 16))
        assert(bitmap.getColor(1, 10).opacity > 240)
        assert(JSRenderer.svgString(svg).contains("data:image/svg+xml"))
        val cached = SvgPicture(path)
        assert(cached.document eq svg.document)
        assertEquals(cached, svg)
  }

  test("browser draws an Inkscape file and an Illustrator file's switch as the JVM does") {
    val inkscape    = SvgPicture.fromString(SvgSanitizingSuite.InkscapeFile)
    val illustrator = SvgPicture.fromString(SvgSanitizingSuite.IllustratorFile)
    for _ <- inkscape.ready; _ <- illustrator.ready yield
      val page = JSRenderer.createBitmapFrom(inkscape)
      assertEquals(page.getColor(400, 300), Color(255, 0, 0))
      assertEquals(page.getColor(400, 800).opacity, 0)
      val switch = JSRenderer.createBitmapFrom(illustrator)
      assertEquals(switch.getColor(5, 5), Color(255, 0, 0))
      assertEquals(switch.getColor(15, 5), Color(0, 0, 255))
  }

  test("browser does not draw stripped content") {
    val svg = SvgPicture.fromString(
      """<svg xmlns="http://www.w3.org/2000/svg" xmlns:x="urn:x" width="20" height="10">
        <x:g><rect width="20" height="10" fill="blue"/></x:g>
        <rect width="10" height="10" fill="red"><set attributeName="fill" to="blue"/></rect>
      </svg>"""
    )
    svg.ready.map: _ =>
      val bitmap = JSRenderer.createBitmapFrom(svg)
      assertEquals(bitmap.getColor(5, 5), Color(255, 0, 0))
      assertEquals(bitmap.getColor(15, 5).opacity, 0)
  }
