package smile.rendering

import smile.colors.LinearGradient.CycleMethod
import smile.colors.{Color, LinearGradient}
import smile.infrastructure.JSRenderer
import smile.modeling.{Bounds, Pos}
import smile.pictures.*

/** What the SVG renderer writes so that its picture matches the canvas renderer's and the JVM's.
  * The cases were found with the parity harness (`parity/`), which compares the rasterized
  * pictures, and these tests pin down the markup. The structure of the markup itself is
  * `SvgPlanRendererSuite`'s.
  */
class SvgAgreementSuite extends munit.FunSuite:

  private val red  = Color(255, 0, 0)
  private val blue = Color(0, 0, 255)

  test("the public SVG output embeds rasters at the density asked for") {
    val scene = Picture(Bitmap.of(Circle(Pos(0, 0), 6, fillWith(red), None))).scaleBy(2)
    def embeddedWidth(svg: org.scalajs.dom.Element): Double =
      svg.querySelector("defs image").getAttribute("width").toDouble
    val once  = embeddedWidth(JSRenderer.createSvgFrom(scene))
    val twice = embeddedWidth(JSRenderer.createSvgFrom(scene, density = 2.0))
    assertEqualsDouble(twice, 2 * once, 4.0)
    assert(JSRenderer.svgString(scene, density = 2.0).startsWith("<svg"))
  }

  test("an SVG is as many whole pixels large as the bitmap of the same picture") {
    // The bounds reach from -10 to 10.6 and from -4.9 to 5.5, which the whole pixels from -10 to
    // 11 and from -5 to 6 cover.
    val picture = Rectangle(20.6, 10.4, Pos(0.3, 0.3), fillWith(red), None)
    val bitmap  = picture.toBitmap()
    val svg     = JSRenderer.createSvgFrom(picture)

    assertEquals((bitmap.pixels().width, bitmap.pixels().height), (21, 11))
    assertEquals(svg.getAttribute("width"), "21")
    assertEquals(svg.getAttribute("height"), "11")
    assertEquals(svg.getAttribute("viewBox"), "-10 -5 21 11")
  }

  test("a fractional crop shows the whole pixels its rectangle covers, as its bitmap does") {
    val window  = Bounds(Pos.Origin, 20.6, 10.4)
    val picture = Circle(Pos.Origin, 2, fillWith(red), None).crop(window)
    val bitmap  = picture.toBitmap()
    val svg     = JSRenderer.createSvgFrom(picture)

    assertEquals((bitmap.pixels().width, bitmap.pixels().height), (22, 12))
    assertEquals(svg.getAttribute("width"), "22")
    assertEquals(svg.getAttribute("height"), "12")
    assertEquals(svg.getAttribute("viewBox"), "-11 -6 22 12")
  }

  test("a crop on whole pixels keeps exactly its size") {
    val picture = Circle(Pos.Origin, 2, fillWith(red), None)
      .crop(Bounds(Pos.Origin, 420, 500))
    val svg = JSRenderer.createSvgFrom(picture)
    assertEquals(svg.getAttribute("width"), "420")
    assertEquals(svg.getAttribute("height"), "500")
    assertEquals(svg.getAttribute("viewBox"), "-210 -250 420 500")
  }

  test("blank space of no width never becomes non-finite SVG coordinates") {
    val svg    = JSRenderer.createSvgFrom(blank(0, 10.4))
    val values = svg.getAttribute("viewBox").split(" ").map(_.toDouble)
    assert(values.forall(_.isFinite), svg.outerHTML)
    for width <- Seq(Double.NaN, Double.PositiveInfinity, -1.0) do
      val _ = intercept[IllegalArgumentException](blank(width, 10.4))
  }

  test("the SVG of the empty picture is one transparent pixel") {
    val svg = JSRenderer.createSvgFrom(Empty)

    assertEquals(svg.getAttribute("width"), "1")
    assertEquals(svg.getAttribute("height"), "1")
    assertEquals(svg.getAttribute("viewBox"), "0 0 1 1")
    assertEquals(svg.childElementCount, 0)
  }

  private def gradientOf(paint: LinearGradient) =
    JSRenderer
      .createSvgFrom(Picture(Rectangle(60, 20, Pos(0, 0), fillWith(paint), None)))
      .querySelector("linearGradient")

  test("SVG gradient stops keep their exact offsets") {
    val paint = LinearGradient(
      Pos(0, 0),
      Pos(60, 0),
      Seq(0.0 -> red, 0.125 -> red, 0.5078125 -> blue, 1.0 -> blue)
    )
    val stops   = gradientOf(paint).querySelectorAll("stop")
    val offsets = (0 until stops.length).map(stops(_))

    // Whole percents would have made these 0, 12, 50 and 100.
    assertEquals(offsets.map(_.getAttribute("offset").toDouble), Seq(0.0, 0.125, 0.5078125, 1.0))
  }

  test("SVG gradients carry their cycle method") {
    def spreadOf(cycleMethod: CycleMethod) =
      gradientOf(LinearGradient(Pos(10, 0), Pos(30, 0), red, blue, cycleMethod))
        .getAttribute("spreadMethod")

    assertEquals(spreadOf(CycleMethod.NoCycle), null)
    assertEquals(spreadOf(CycleMethod.Reflect), "reflect")
    assertEquals(spreadOf(CycleMethod.Repeat), "repeat")
  }

  test("an SVG image says how it is to be resampled when the scaling method copies pixels") {
    // Scaled, so that the image is resampled.
    def picture                                = Bitmap(2, 2, Seq(red, blue, blue, red)).scaleBy(4)
    def pixelated(method: smile.ScalingMethod) =
      JSRenderer
        .createSvgFrom(picture, smile.RenderSettings(bufferScalingMethod = method))
        .outerHTML
        .contains("image-rendering: pixelated")

    // Nearest-neighbor, for Chromium, Firefox and WebKit in CSS and for the rest as SVG 1.1's
    // attribute.
    for method <- Seq(smile.ScalingMethod.NearestNeighbor, smile.ScalingMethod.Fast) do
      assert(pixelated(method), method.toString)
    // The smooth methods are every renderer's default and write nothing.
    for method <- Seq(smile.ScalingMethod.Smooth, smile.ScalingMethod.AreaAveraging) do
      assert(!pixelated(method), method.toString)
    assert(!JSRenderer.createSvgFrom(picture).outerHTML.contains("image-rendering: pixelated"))
  }

  test("an SVG embeds a bitmap's pixels computed with the settings the SVG is rendered with") {
    def hrefOf(picture: Element, settings: smile.RenderSettings): String =
      val image = JSRenderer.createSvgFrom(picture, settings).querySelector("image")
      Option(image.getAttributeNS("http://www.w3.org/1999/xlink", "href"))
        .getOrElse(image.getAttribute("xlink:href"))
    val scaled  = Bitmap(2, 1, Seq(red, blue)).scaleBy(4, 1).asBitmap
    val nearest = smile.RenderSettings(
      bufferScalingMethod = smile.ScalingMethod.NearestNeighbor,
      resamplingMode = smile.ResamplingMode.Stepwise
    )
    val smooth = smile.RenderSettings(resamplingMode = smile.ResamplingMode.Stepwise)
    def asPixels(settings: smile.RenderSettings) =
      Bitmap.fromARGB(8, 1, scaled.pixels(settings).argbData)

    // The nearest pixels are opaque, so copying them into a bitmap of their own keeps them exactly.
    assertEquals(hrefOf(scaled, nearest), hrefOf(asPixels(nearest), smooth))
    assertNotEquals(hrefOf(scaled, nearest), hrefOf(scaled, smooth))
  }
end SvgAgreementSuite
