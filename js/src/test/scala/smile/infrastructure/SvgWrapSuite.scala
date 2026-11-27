package smile.infrastructure

import org.scalajs.dom
import smile.RenderSettings
import smile.colors.Color
import smile.modeling.Pos
import smile.pictures.*

import scala.concurrent.{Future, Promise}
import scala.concurrent.ExecutionContext.Implicits.global

/** The pixels of the SVG written for a wrapped picture, which is one tile repeated as the
  * `<pattern>` fill of one rectangle.
  *
  * Three groups of tests rasterize the written SVG with the environment's own decoder, through
  * `pixels`. The first pins the phase and the orientation of the repeated tile. The second pins
  * that an exported wrap shows no seam between its copies when it is displayed at another size. The
  * third pins that translucent shapes that overlap in the tile blend as they do on a canvas.
  *
  * The group named "the plan for SVG paints as the canvas's own does under ..." paints both plans
  * with the canvas renderer, so it needs no SVG decoding.
  */
class SvgWrapSuite extends munit.FunSuite:
  private val settings = RenderSettings.Default

  private def rect(x: Double, y: Double, w: Double, h: Double, color: Color): Element =
    Rectangle(w, h, Pos(x + w / 2, y + h / 2), Some(FillStyle(color)), None)

  // An 18 by 12 tile, front first. Its marks differ from side to side and from top to bottom, so
  // a repetition that is mirrored or that starts at the wrong phase changes pixels.
  private val tile = Picture(
    rect(7, 5, 2, 3, Color(255, 0, 255)),
    rect(0, 0, 18, 2, Color(0, 255, 0)),
    rect(0, 0, 3, 12, Color(255, 0, 0)),
    rect(13, 0, 5, 12, Color(0, 0, 255)),
    rect(0, 0, 18, 12, Color(70, 80, 90))
  )

  /** The ARGB pixels of `svg`, row by row, as the environment decodes and draws it at its declared
    * size.
    */
  private def pixels(svg: dom.Element): Future[Array[Int]] =
    val result = Promise[Array[Int]]()
    val image  = dom.document.createElement("img").asInstanceOf[dom.html.Image]
    image.onload = _ =>
      val canvas = dom.document.createElement("canvas").asInstanceOf[dom.html.Canvas]
      canvas.width = image.naturalWidth
      canvas.height = image.naturalHeight
      val ctx = canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]
      ctx.drawImage(image, 0, 0)
      val rgba = ctx.getImageData(0, 0, canvas.width, canvas.height).data
      result.success(Array.tabulate(canvas.width * canvas.height): i =>
        val offset = 4 * i
        val r      = rgba(offset)
        val g      = rgba(offset + 1)
        val b      = rgba(offset + 2)
        val a      = rgba(offset + 3)
        (a << 24) | (r << 16) | (g << 8) | b)
    image.addEventListener(
      "error",
      (_: dom.Event) =>
        val _ = result.tryFailure(new AssertionError("exported SVG failed to decode"))
    )
    val text = new dom.XMLSerializer().serializeToString(svg)
    image.src =
      "data:image/svg+xml;charset=utf-8," + scala.scalajs.js.URIUtils.encodeURIComponent(text)
    result.future

  test("two-axis wrapping preserves asymmetric colors and phase") {
    val scene = tile.wrapShift(5, 3)
    pixels(SvgPlanRenderer.render(scene, settings)).map: actual =>
      // The tile's own pixels. A wrap shifted by (5, 3) shows at each pixel the tile's pixel that
      // far back, around the tile's 18 by 12.
      def original(x: Int, y: Int): Int =
        if x >= 7 && x < 9 && y >= 5 && y < 8 then 0xffff00ff
        else if y < 2 then 0xff00ff00
        else if x < 3 then 0xffff0000
        else if x >= 13 then 0xff0000ff
        else 0xff46505a
      val expected = (for y <- 0 until 12; x <- 0 until 18
      yield original((x - 5 + 18) % 18, (y - 3 + 12) % 12)).toArray
      assertEquals(actual.toSeq, expected.toSeq)
  }

  for (label, transform) <- Seq[(String, Element => Element)](
      "reflection and nonuniform scale" -> (_.scaleBy(-2, 3)),
      "nonzero origin"                  -> (_.moveBy(7, -4)),
      "fractional period"               -> (_.scaleBy(1.17, 1.23)),
      "rotation"                        -> (_.rotateBy(23)),
      "a wrap of a wrap"                -> (_.scaleBy(1.17).wrapShift(4, 2))
    )
  do
    // A plan for SVG tiles every wrap, where the canvas's own plan draws a whole-pixel wrap as
    // copies of its content. The two have to paint the same pixels. The test runs without a
    // browser, because both plans are painted by the canvas renderer.
    test(s"the plan for SVG paints as the canvas's own does under $label") {
      val scene  = transform(tile.wrapShift(5, 3))
      val normal = DrawPlan.planned(scene, DeviceGrid(1), settings, 0L, nesting = 2)
      val svg    = DrawPlan.planned(scene, DeviceGrid(1), settings, 0L, nesting = 2, svg = true)
      def raster(plan: DrawPlan.Plan) =
        CanvasPlanRenderer.render(plan, 1, settings, 0L).buffer(settings, 0L).argbData
      val actual   = raster(svg)
      val expected = raster(normal)
      assertEquals(actual.length, expected.length)
      // Drawing through an extra offscreen tile can round an antialiased channel one step apart.
      for ((a, b), i) <- actual.zip(expected).zipWithIndex; shift <- Seq(0, 8, 16, 24) do
        assert(
          math.abs(((a >>> shift) & 255) - ((b >>> shift) & 255)) <= 1,
          s"pixel $i, channel $shift"
        )
      // Writing it exercises definition reuse and single-use prepared slots, and gives a pattern.
      val written = SvgPlanRenderer.render(scene, settings, nesting = 2)
      assert(written.querySelectorAll("pattern").length >= 1)
      assertEquals(written.querySelectorAll("rect[fill^='url(']").length >= 1, true)
    }

  for opacity <- Seq(255, 128); scale <- Seq(0.39, 1.073, 1.5, 2.147) do
    // At these scales the seam between the copies falls between device pixels. Copies drawn one
    // by one are each antialiased along it. A pattern fill is one shape and has no edge there.
    test(s"resizing an exported wrap to $scale leaves no seam at opacity $opacity") {
      val solid  = rect(0, 0, 1000, 400, Color(80, 160, 220, opacity)).wrapShift(37, 23)
      val svg    = SvgPlanRenderer.render(solid, settings)
      val width  = math.round(1000 * scale).toInt
      val height = math.round(400 * scale).toInt
      svg.setAttribute("width", width.toString)
      svg.setAttribute("height", height.toString)
      pixels(svg).map: actual =>
        // The antialiased outer edge is left out. Every interior pixel is covered whole, so its
        // alpha is the fill's. A seam between the copies changes the alpha along it.
        for y <- 2 until height - 2; x <- 2 until width - 2 do
          assertEquals(actual(y * width + x) >>> 24, opacity, s"at ($x,$y)")
    }

  test("overlapping translucent shapes blend once within each repeated tile") {
    val scene = Picture(
      rect(0, 0, 18, 12, Color(255, 0, 0, 128)),
      rect(4, 0, 7, 12, Color(0, 0, 255, 128))
    ).wrapShift(5, 3)
    // The canvas blends the blue shape over the red one once. The SVG has to match it within a
    // rounding step per channel.
    val reference = CanvasPlanRenderer.render(scene, 1, settings, 0L).buffer(settings, 0L).argbData
    pixels(SvgPlanRenderer.render(scene, settings)).map: actual =>
      for (a, b) <- actual.zip(reference); shift <- Seq(0, 8, 16, 24) do
        assert(math.abs(((a >>> shift) & 255) - ((b >>> shift) & 255)) <= 1)
  }
