package smile.pictures

import smile.RenderSettings
import smile.infrastructure.JVMRenderer
import smile.modeling.{Bounds, Pos}

import java.io.IOException
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}
import scala.concurrent.Await
import scala.concurrent.duration.DurationInt

class SvgPictureSuite extends munit.FunSuite:
  private val source = """<svg xmlns="http://www.w3.org/2000/svg" width="10" height="10"
    viewBox="0 0 10 10"><rect width="10" height="10" fill="white"/>
    <rect x="4.9" width="0.2" height="10" fill="black"/></svg>"""

  test("SVG repeat patterns keep their phase when loaded on the JVM") {
    val svg = SvgPicture.fromString(
      """<svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink"
      width="18" height="12" viewBox="0 0 18 12"><defs>
      <g id="tile"><rect width="18" height="12" fill="#46505a"/>
      <rect width="3" height="12" fill="red"/>
      <rect x="13" width="5" height="12" fill="blue"/>
      <rect width="18" height="2" fill="lime"/></g>
      <pattern id="repeat" patternUnits="userSpaceOnUse" width="18" height="12"
      patternTransform="matrix(1 0 0 1 5 3)"><use xlink:href="#tile"/></pattern>
      </defs><rect width="18" height="12" fill="url(#repeat)"/></svg>"""
    )
    val image = svg.toBitmap().toAWTImage()
    // The pattern's transform moves the tile by (5, 3), so each pixel shows the tile's pixel that
    // far back, around the tile's 18 by 12. A loader that drops the transform starts the tile at
    // the origin.
    for y <- 0 until 12; x <- 0 until 18 do
      val sourceX  = (x - 5 + 18) % 18
      val sourceY  = (y - 3 + 12) % 12
      val expected =
        if sourceY < 2 then 0xff00ff00
        else if sourceX < 3 then 0xffff0000
        else if sourceX >= 13 then 0xff0000ff
        else 0xff46505a
      assertEquals(image.getRGB(x, y), expected, s"at ($x,$y)")
  }

  test("scaling re-renders subpixel vector detail instead of enlarging the original pixels") {
    val svg   = SvgPicture.fromString(source)
    val large = svg.scaleBy(100, 100)
    val image = large.toBitmap().toAWTImage()
    assertEquals(image.getWidth, 1000)
    assertEquals(image.getRGB(500, 500), 0xff000000)
    assertEquals(image.getRGB(480, 500), 0xffffffff)
    assertEquals(image.getRGB(520, 500), 0xffffffff)
    assertEquals(svg.bounds, Some(Bounds(-5, -5, 5, 5)))
  }

  test("high DPI output renders SVG detail at the output density") {
    val svg   = SvgPicture.fromString(source)
    val image =
      JVMRenderer.createBitmapFrom(svg, 100, RenderSettings.Default).toAWTImage()
    assertEquals(image.getWidth, 1000)
    assertEquals(image.getRGB(500, 500), 0xff000000)
    assertEquals(image.getRGB(480, 500), 0xffffffff)
  }

  test("translation, rotation, nonuniform scaling and mirroring keep the document's box") {
    val svg = SvgPicture.fromString(
      """<svg xmlns="http://www.w3.org/2000/svg" width="20" height="10">
      <rect width="10" height="10" fill="red"/></svg>"""
    )
    val moved = svg.moveTo(Pos(30, 40))
    assertEquals(moved.bounds, Some(Bounds(20, 35, 40, 45)))
    val turned = moved.rotateBy(90)
    assertEqualsDouble(turned.bounds.get.width, 10, 1e-9)
    assertEqualsDouble(turned.bounds.get.height, 20, 1e-9)
    val rotatedImage = turned.toBitmap().toAWTImage()
    assertEquals(rotatedImage.getRGB(5, 15), 0xffff0000)
    assertEquals(rotatedImage.getRGB(5, 5), 0x00000000)
    val mirrored = svg.scaleBy(-2, 3).toBitmap().toAWTImage()
    assertEquals(mirrored.getWidth, 40)
    assertEquals(mirrored.getHeight, 30)
    assertEquals(mirrored.getRGB(30, 15), 0xffff0000)
    assertEquals(mirrored.getRGB(10, 15), 0x00000000)
  }

  test("viewBox, transparency, clipping and composition draw as for any element") {
    val svg = SvgPicture.fromString(
      """<svg xmlns="http://www.w3.org/2000/svg" width="20" height="10"
      viewBox="10 20 20 10"><circle cx="20" cy="25" r="4" fill="red"/></svg>"""
    )
    val shifted = svg.moveTo(Pos(30, 40))
    val picture = Picture(shifted, shifted.moveTo(Pos(60, 40))).crop(Bounds(20, 35, 70, 45))
    val image   = picture.toBitmap().toAWTImage()
    assertEquals(image.getRGB(10, 5), 0xffff0000)
    assertEquals(image.getRGB(40, 5), 0xffff0000)
    assertEquals(image.getRGB(25, 5), 0x00000000)
  }

  test("loads local files and rejects malformed documents and zero-sized viewports") {
    val path = Files.createTempFile("smile-svg-", ".svg")
    try
      val _ = Files.writeString(path, source)
      assertEquals(SvgPicture(path.toString).bounds, Some(Bounds(-5, -5, 5, 5)))
    finally Files.delete(path)
    val _ = intercept[IOException](SvgPicture.fromString("not an svg"))
    val _ = intercept[IOException](SvgPicture.fromString("<html/>"))
    val _ = intercept[IOException](
      SvgPicture.fromString(
        """<svg xmlns="http://www.w3.org/2000/svg" width="0" height="10"/>"""
      )
    )
  }

  test("SVG gradients and clip paths survive transformation") {
    val svg = SvgPicture
      .fromString(
        """<svg xmlns="http://www.w3.org/2000/svg" width="10" height="10">
      <defs><linearGradient id="g"><stop stop-color="red"/>
      <stop offset="1" stop-color="blue"/></linearGradient>
      <clipPath id="c"><rect width="10" height="5"/></clipPath></defs>
      <rect width="10" height="10" fill="url(#g)" clip-path="url(#c)"/></svg>"""
      )
      .scaleBy(10, 10)
    val image = svg.toBitmap().toAWTImage()
    val left  = image.getRGB(10, 20)
    val right = image.getRGB(90, 20)
    assert(((left >>> 16) & 255) > (left & 255))
    assert((right & 255) > ((right >>> 16) & 255))
    assertEquals(image.getRGB(50, 80), 0x00000000)
  }

  test("invalid transforms are rejected before they reach the renderer") {
    val svg = SvgPicture.fromString(source)
    val _   = intercept[IllegalArgumentException](svg.scaleBy(Double.NaN, 1))
    val _   = intercept[IllegalArgumentException](svg.scaleBy(Double.PositiveInfinity, 1))
    val _   = intercept[IllegalArgumentException](svg.moveTo(Pos(Double.PositiveInfinity, 0)))
  }

  private def pixelAt(svg: SvgPicture, x: Int, y: Int): Int =
    svg.toBitmap().toAWTImage().getRGB(x, y)

  private def withFile[A](text: String)(test: Path => A): A =
    val path = Files.createTempFile("smile-svg-", ".svg")
    try
      val _ = Files.writeString(path, text)
      test(path)
    finally Files.delete(path)

  test("an Inkscape file paints its rect") {
    val svg = SvgPicture.fromString(SvgSanitizingSuite.InkscapeFile)
    assertEquals(pixelAt(svg, 400, 300), 0xffff0000)
    assertEquals(pixelAt(svg, 400, 800), 0x00000000)
  }

  test("an Illustrator file paints the first child of its switch, with its styles") {
    val svg = SvgPicture.fromString(SvgSanitizingSuite.IllustratorFile)
    assertEquals(pixelAt(svg, 5, 5), 0xffff0000)
    assertEquals(pixelAt(svg, 15, 5), 0xff0000ff)
  }

  test("a switch is drawn as a group, since JSVG draws no switch") {
    val text = SvgDocument.normalized(
      """<svg xmlns="http://www.w3.org/2000/svg" width="10" height="10">
        <switch transform="scale(2)"><rect id="first"/><rect id="second"/></switch></svg>"""
    )
    assert(text.contains("""<g transform="scale(2)"><rect id="first"/></g>"""), text)
    assert(!text.contains("switch"), text)
  }

  test("stripped content does not paint, and the static picture does") {
    val svg = SvgPicture.fromString(
      """<svg xmlns="http://www.w3.org/2000/svg" xmlns:x="urn:x" width="20" height="10">
        <x:g><rect width="20" height="10" fill="blue"/></x:g>
        <rect width="10" height="10" fill="red"><set attributeName="fill" to="blue"/></rect>
      </svg>"""
    )
    assertEquals(pixelAt(svg, 5, 5), 0xffff0000)
    assertEquals(pixelAt(svg, 15, 5), 0x00000000)
  }

  test("an embedded data: raster is drawn") {
    val raster = new java.awt.image.BufferedImage(2, 2, java.awt.image.BufferedImage.TYPE_INT_ARGB)
    for x <- 0 until 2; y <- 0 until 2 do raster.setRGB(x, y, 0xff00ff00)
    val png = java.io.ByteArrayOutputStream()
    assert(javax.imageio.ImageIO.write(raster, "png", png))
    val data = java.util.Base64.getEncoder.encodeToString(png.toByteArray)
    val svg  = SvgPicture.fromString(
      s"""<svg xmlns="http://www.w3.org/2000/svg" width="10" height="10">
        <image href="data:image/png;base64,$data" width="10" height="10"/></svg>"""
    )
    assertEquals(pixelAt(svg, 5, 5), 0xff00ff00)
  }

  test("an embedded SVG document is not drawn") {
    val inner = java.util.Base64.getEncoder.encodeToString(
      """<svg xmlns="http://www.w3.org/2000/svg" width="10" height="10">
      <rect width="10" height="10" fill="red"/></svg>""".getBytes(UTF_8)
    )
    val text = s"""<svg xmlns="http://www.w3.org/2000/svg" width="10" height="10">
      <image href="data:image/svg+xml;base64,$inner" width="10" height="10"/></svg>"""
    assertEquals(pixelAt(SvgPicture.fromString(text), 5, 5), 0x00000000)
  }

  test("external entities, DTDs and CSS references are neither read nor fetched") {
    val requests = java.util.concurrent.atomic.AtomicInteger()
    val server   = com.sun.net.httpserver.HttpServer.create(
      java.net.InetSocketAddress("127.0.0.1", 0),
      0
    )
    server.createContext(
      "/",
      exchange =>
        val _    = requests.incrementAndGet()
        val body = """<!ENTITY red "<rect width='10' height='10' fill='red'/>">"""
          .getBytes(java.nio.charset.StandardCharsets.UTF_8)
        exchange.sendResponseHeaders(200, body.length.toLong)
        exchange.getResponseBody.write(body)
        exchange.close()
    )
    server.start()
    try
      withFile("<rect width='10' height='10' fill='red'/>"): fragment =>
        val base = s"http://127.0.0.1:${server.getAddress.getPort}"
        val url  = s"$base/svg.dtd"
        // JSVG logs the CSS it skips. That logging is not what this test checks, so the loading
        // runs quietly.
        val svg = quietly:
          SvgPicture.fromString(
            s"""<?xml version="1.0"?>
          <!DOCTYPE svg PUBLIC "-//W3C//DTD SVG 1.1//EN" "$url" [
            <!ENTITY file SYSTEM "${fragment.toUri}">
            <!ENTITY web SYSTEM "$url">
            <!ENTITY % remote SYSTEM "$url">
            %remote;
          ]>
          <svg xmlns="http://www.w3.org/2000/svg" width="10" height="10">&file;&web;
            <style>@import url($base/x.css); rect { fill: url($base/x.svg#q) }</style>
            <rect width="5" height="5" fill="url($base/x.svg#p)"/>
            <rect x="5" width="5" height="5" style="fill: url($base/x.svg#p)"/>
            <rect y="5" width="5" height="5" style="filter: url($base/x.svg#f)"/>
          </svg>"""
          )
        assertEquals(svg.bounds, Some(Bounds(-5, -5, 5, 5)))
        assertEquals(quietly(pixelAt(svg, 7, 7)), 0x00000000)
        assertEquals(requests.get, 0)
        // The server does answer what does ask it.
        val connection = java.net.URI(url).toURL.openConnection()
        connection.setConnectTimeout(2000)
        connection.setReadTimeout(2000)
        val _ = connection.getInputStream.readAllBytes()
        assertEquals(requests.get, 1)
    finally server.stop(0)
  }

  test("a document whose entities expand to billions of characters is refused") {
    // Ten levels of ten references each give 10 to the 10th copies of the innermost text.
    val levels = (1 to 9).map(n => s"""<!ENTITY e$n "${s"&e${n - 1};" * 10}">""").mkString
    val bomb   =
      s"""<?xml version="1.0"?>
      <!DOCTYPE svg [<!ENTITY e0 "lol">$levels]>
      <svg xmlns="http://www.w3.org/2000/svg" width="10" height="10"><desc>${"&e9;" * 10}</desc></svg>"""
    val message = intercept[IOException](SvgPicture.fromString(bomb)).getMessage
    assert(message.startsWith("SVG could not be read: it is not well-formed XML ("), message)
  }

  test("every load failure is an IOException naming the path") {
    def failure(path: String): String = intercept[IOException](SvgPicture(path)).getMessage
    assertEquals(failure(" "), "SVG path ' ' is empty")
    assertEquals(failure("no/such/picture.svg"), "SVG at path 'no/such/picture.svg' not found")
    withFile("not an svg"): path =>
      assert(
        failure(path.toString).startsWith(
          s"SVG at path '$path' could not be read: it is not well-formed XML ("
        )
      )
    withFile("<html/>"): path =>
      assertEquals(
        failure(path.toString),
        s"SVG at path '$path' could not be read: its root element is 'html', not an SVG 'svg'"
      )
    withFile("""<svg xmlns="http://www.w3.org/2000/svg" width="10" height="20000"/>"""): path =>
      assertEquals(
        failure(path.toString),
        s"SVG at path '$path' could not be read: it is 10 x 20000 pixels, and a side may be at most 16384"
      )
    val copies = """<use href="#p"/>""" * 2001
    withFile(
      s"""<svg xmlns="http://www.w3.org/2000/svg" width="10" height="10">
      <defs><path id="p" d="M0 0h1v1z"/></defs>$copies</svg>"""
    ): path =>
      assertEquals(
        quietly(failure(path.toString)),
        s"SVG at path '$path' could not be read: JSVG could not load it (it may be over the " +
          "limits of 2000 drawn elements, a nesting depth of 30 or a use depth of 15)"
      )
    withFile(SvgSanitizingSuite.nested(31)): path =>
      assertEquals(
        failure(path.toString),
        s"SVG at path '$path' could not be read: its elements nest more than 30 deep"
      )
    for badPath <- Seq("bad\u0000name.svg", "bad\u0000folder/name.svg") do
      assertEquals(failure(badPath), s"SVG at path '$badPath' not found")
  }

  test(
    "flips, turns and scalings keep an SVG picture, and a recolor that changes it gives pixels"
  ) {
    val svg = SvgPicture.fromString(source)
    for moved <- Seq(svg.flipHorizontally, svg.flipVertically, svg.rotateBy(30), svg.scaleBy(3, 2))
    do
      moved match
        case vector: SvgPicture => assert(vector.document eq svg.document)
        case other              => fail(s"$other is not an SVG picture")
    // The recolor is from white to red. The document's paints are not rewritten, so only the
    // pixels can show it.
    val toRed     = (argb: Int) => if argb == 0xffffffff then 0xffff0000 else argb
    val settings  = RenderSettings.Default
    val recolored = Recolor(svg, toRed, settings)
    recolored.source.source match
      case BitmapSource.Pixels(_) => ()
      case other                  => fail(s"$other is not pixels")
    assertEquals(recolored.toAWTImage().getRGB(1, 1), 0xffff0000)
    // A function that changes no color the document shows keeps the picture.
    Recolor(svg, identity, settings).source.source match
      case BitmapSource.Content(_) => ()
      case other                   => fail(s"$other does not keep the picture")
  }

  test("toBitmap, a crop and a recolor that keeps the picture draw an SVG's vectors at density 2") {
    // Red up to x = 4.5 and blue after it. At 1x the edge halves a pixel, and at 2x it lies
    // between two, where an enlarged 1x rendering would blend them.
    val svg = SvgPicture.fromString(
      """<svg xmlns="http://www.w3.org/2000/svg" width="10" height="10">
      <rect width="4.5" height="10" fill="#FF0000"/>
      <rect x="4.5" width="5.5" height="10" fill="#0000FF"/></svg>"""
    )
    val settings = RenderSettings.Default
    def assertSharp(bitmap: Bitmap, width: Int, edge: Int)(using munit.Location): Unit =
      val image = bitmap.toAWTImage()
      assertEquals((image.getWidth, image.getHeight), (width, 20))
      for x <- 0 until width do
        assertEquals(image.getRGB(x, 10), if x < edge then 0xffff0000 else 0xff0000ff, s"x = $x")
    assertSharp(svg.toBitmap(scale = 2), 20, 9)
    // From -3 to 3, with the edge at -0.5, 5 device pixels in.
    assertSharp(svg.crop(Bounds(-3, -5, 3, 5)).toBitmap(scale = 2), 12, 5)
    // Green is no color the document shows, so the recolor keeps the picture.
    val kept = Recolor(svg, argb => if argb == 0xff00ff00 then 0xffffff00 else argb, settings)
    assertSharp(kept.toBitmap(settings, 2), 20, 9)
    assertSharp(
      Recolor(svg.crop(Bounds(-3, -5, 3, 5)), identity, settings).toBitmap(scale = 2),
      12,
      5
    )
    // A recolor that changes red is the 1x pixels, which a denser output enlarges.
    val changed = Recolor(svg, argb => if argb == 0xffff0000 then 0xff00ff00 else argb, settings)
    val image   = changed.toBitmap(settings, 2).toAWTImage()
    assertEquals((image.getWidth, image.getHeight), (20, 20))
    assertEquals(image.getRGB(0, 10), 0xff00ff00)
    assertEquals(image.getRGB(19, 10), 0xff0000ff)
    assert(Seq(8, 9).exists(x => !Set(0xff00ff00, 0xff0000ff).contains(image.getRGB(x, 10))))
  }

  test("SvgPicture(path) loads a classpath resource, with or without a leading slash") {
    for path <- Seq("svg/sample.svg", "/svg/sample.svg") do
      val svg = SvgPicture(path)
      assertEquals(svg.bounds, Some(Bounds(-6, -4, 6, 4)), path)
      assertEquals(pixelAt(svg, 6, 4), 0xffff0000, path)
  }

  test("SvgPicture.load completes with the picture SvgPicture(path) gives") {
    withFile(source): path =>
      val loaded = Await.result(SvgPicture.load(path.toString), 10.seconds)
      assertEquals(loaded.bounds, SvgPicture(path.toString).bounds)
      assertEquals(pixelAt(loaded, 1, 1), 0xffffffff)
    val error =
      intercept[IOException](Await.result(SvgPicture.load("no/such/picture.svg"), 10.seconds))
    assertEquals(error.getMessage, "SVG at path 'no/such/picture.svg' not found")
  }

  test("elements nested 30 deep below the root are drawn, and 100000 deep do not overflow") {
    assertEquals(pixelAt(SvgPicture.fromString(SvgSanitizingSuite.nested(30)), 5, 5), 0xffff0000)
    // jsdom takes quadratic time to build such a tree, so this one runs on the JVM only.
    val _ = intercept[IOException](SvgPicture.fromString(SvgSanitizingSuite.nested(100000)))
  }

  /** `body`, run with JSVG's logger off. JSVG logs the failures it reports by returning `null` and
    * the CSS it skips.
    */
  private def quietly[A](body: => A): A =
    val logger = java.util.logging.Logger.getLogger("com.github.weisj.jsvg")
    val level  = logger.getLevel
    logger.setLevel(java.util.logging.Level.OFF)
    try body
    finally logger.setLevel(level)
