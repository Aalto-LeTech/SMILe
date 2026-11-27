package smile.infrastructure

import org.scalajs.dom
import smile.RenderSettings
import smile.colors.{Color, LinearGradient}
import smile.modeling.{Affine, Bounds, Pos}
import smile.pictures.*
import smile.pictures.{FillStyle, ResamplingHint}
import smile.testing.Rasterizer

import scala.concurrent.{ExecutionContext, Future, Promise}

/** The SVG renderer of the shared draw plan, by the structure of what it writes. jsdom cannot
  * rasterize SVG, so its pixels are compared with the canvas's in the browser tier. The pixels of a
  * wrap, which is written as a `<pattern>` fill, are also checked by [[SvgWrapSuite]] when the
  * suites run in a browser.
  */
class SvgPlanRendererSuite extends munit.FunSuite:

  import PlanRenderCases.*

  private given ExecutionContext = ExecutionContext.global

  private val settings = RenderSettings.Default

  private def render(e: Element, density: Double = 1.0): dom.Element =
    SvgPlanRenderer.render(e, settings, density)

  /** The size and the ARGB pixels of the image at `url`, as this environment decodes it. */
  private def decoded(url: String): Future[(Int, Int, Array[Int])] =
    val result = Promise[(Int, Int, Array[Int])]()
    val image  = dom.document.createElement("img").asInstanceOf[dom.html.Image]
    image.onload = _ =>
      val canvas = dom.document.createElement("canvas").asInstanceOf[dom.html.Canvas]
      canvas.width = image.naturalWidth
      canvas.height = image.naturalHeight
      val ctx = canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]
      ctx.drawImage(image, 0, 0)
      val rgba = ctx.getImageData(0, 0, canvas.width, canvas.height).data
      val argb = Array.tabulate(canvas.width * canvas.height): i =>
        (rgba(4 * i + 3) << 24) | (rgba(4 * i) << 16) | (rgba(4 * i + 1) << 8) | rgba(4 * i + 2)
      result.success((canvas.width, canvas.height, argb))
    image.addEventListener(
      "error",
      (_: dom.Event) =>
        val _ = result.tryFailure(new AssertionError("an embedded raster failed to decode"))
    )
    image.src = url
    result.future

  /** Whether two lists of embedded rasters hold the same pictures. Equal data is the same at once.
    * Otherwise each raster is paired with one of its size whose pixels are the same up to
    * [[smile.testing.Rasterizer.layerRoundingLevels]].
    */
  private def sameRasters(first: Vector[String], second: Vector[String]): Future[Boolean] =
    if first == second then Future.successful(true)
    else if first.length != second.length then Future.successful(false)
    else
      for
        as <- Future.sequence(first.map(decoded))
        bs <- Future.sequence(second.map(decoded))
      yield
        val left = scala.collection.mutable.ArrayBuffer.from(bs)
        as.forall: (width, height, pixels) =>
          val at = left.indexWhere: (w, h, others) =>
            w == width && h == height &&
              Rasterizer.sameUpTo(Rasterizer.layerRoundingLevels, pixels, others)
          if at >= 0 then
            val _ = left.remove(at)
          at >= 0

  private def all(root: dom.Element, selector: String): Vector[dom.Element] =
    val found = root.querySelectorAll(selector)
    (0 until found.length).map(i => found(i)).toVector

  /** The six numbers of a `matrix(a b c d e f)` transform. */
  private def matrix(transform: String): Vector[Double] =
    val inner = transform.stripPrefix("matrix(").stripSuffix(")")
    inner.split("[\\s,]+").toVector.map(_.toDouble)

  private def coefficients(m: Affine): Vector[Double] = Vector(m.a, m.b, m.c, m.d, m.e, m.f)

  /** What `href` refers to, with or without the `xlink` namespace. */
  private def href(e: dom.Element): String =
    Option(e.getAttribute("href"))
      .filter(_.nonEmpty)
      .getOrElse(e.getAttributeNS("http://www.w3.org/1999/xlink", "href"))

  /** Every tiled wrap in `ops`, nested ones included. */
  private def tiles(ops: Vector[DrawOp]): Vector[DrawOp.Tiles] = ops.flatMap:
    case t: DrawOp.Tiles        => Vector(t)
    case DrawOp.HardClip(_, in) => tiles(in)
    case l: DrawOp.Layer        => tiles(l.ops)
    case _                      => Vector()

  private val svgSource =
    """<svg xmlns="http://www.w3.org/2000/svg" width="24" height="12">""" +
      """<rect x="0" y="0" width="24" height="12" fill="#00ff00"/></svg>"""

  test("each distinct SVG document is defined once and every copy uses it") {
    val svg   = SvgPicture.fromString(svgSource)
    val other = SvgPicture.fromString(svgSource.replace("00ff00", "ff0000"))
    val scene = Picture(svg, svg.moveBy(30, 0), svg.rotateBy(30).moveBy(0, 40), other.moveBy(60, 0))
    val root  = render(scene)
    val images = all(root, "defs image")
    assertEquals(images.size, 2, "one definition for each document")
    val uses = all(root, "use")
    for image <- images do
      val id = image.getAttribute("id")
      assert(id.nonEmpty, "a definition has no id")
      val users = uses.filter(use => href(use) == s"#$id")
      assert(users.size == 3 || users.size == 1, s"#$id is used ${users.size} times")
    assertEquals(uses.size, 4)
    // The data URL is written only in the definitions.
    assertEquals(all(root, "image").size, 2)
  }

  test("an SVG of 50 copies of one document writes its data URL once") {
    val svg    = SvgPicture.fromString(svgSource)
    val url    = svg.document.dataUrl
    val one    = SvgPlanRenderer.string(svg, settings, 1.0)
    val copies =
      SvgPlanRenderer.string(Picture((0 until 50).map(i => svg.moveBy(30 * i, 0))*), settings, 1.0)
    def occurrences(text: String) =
      Iterator.iterate(text.indexOf(url))(i => text.indexOf(url, i + 1)).takeWhile(_ >= 0).size
    assertEquals(occurrences(one), 1)
    assertEquals(occurrences(copies), 1)
    // Each further copy adds only its use element.
    assert(copies.length - one.length < 49 * 200, s"${copies.length - one.length} more characters")
  }

  test("the ids of two renderings in one document are distinct and each resolves in its own") {
    val scene = Picture(
      MaskGroup(Circle(Pos.Origin, 8, Some(FillStyle(green)), None), sprite(30, 20)),
      MaskGroup(
        Circle(Pos.Origin, 8, Some(FillStyle(green)), None),
        Rectangle(30, 20, Pos.Origin, Some(FillStyle(gradient)), None),
        inverted = true
      ).moveBy(40, 0),
      Circle(Pos.Origin, 20, Some(FillStyle(red)), None).crop(Bounds(-10, -10, 10, 10)).rotateBy(30)
    )
    val host                   = dom.document.createElement("div")
    val (first, second)        = (render(scene), render(scene))
    val _                      = host.appendChild(first)
    val _                      = host.appendChild(second)
    def ids(root: dom.Element) = all(root, "[id]").map(_.getAttribute("id"))
    val (firstIds, secondIds)  = (ids(first), ids(second))
    assert(firstIds.size >= 6, s"only ${firstIds.size} definitions: $firstIds")
    assertEquals(firstIds.distinct.size, firstIds.size, "a rendering repeats an id")
    assert(firstIds.intersect(secondIds).isEmpty, "the two renderings share ids")
    for root <- Seq(first, second) do
      val own        = ids(root).toSet
      val referenced = all(root, "*").flatMap: e =>
        val attributes = Seq("mask", "clip-path", "filter", "fill", "stroke").flatMap: name =>
          Option(e.getAttribute(name)).collect:
            case s"url(#$id)" => id
        val link = Option(href(e)).filter(_.startsWith("#")).map(_.drop(1))
        attributes ++ link
      assert(referenced.nonEmpty, "nothing refers to a definition")
      for id <- referenced do assert(own.contains(id), s"#$id is not defined in its own rendering")
  }

  test("the ids of a rendering count up from one rendering's prefix") {
    val scene = Picture(
      Circle(Pos.Origin, 20, Some(FillStyle(red)), None)
        .crop(Bounds(-10, -10, 10, 10))
        .rotateBy(30),
      Rectangle(30, 20, Pos.Origin, Some(FillStyle(gradient)), None)
    )
    val ids      = all(render(scene), "[id]").map(_.getAttribute("id"))
    val prefixes = ids.map(_.reverse.dropWhile(_.isDigit).reverse)
    val numbers  = ids.map(_.reverse.takeWhile(_.isDigit).reverse.toInt)
    assertEquals(numbers.sorted, (1 to ids.size).toVector, s"$ids")
    assert(prefixes.map(_.takeWhile(_ != '-')).distinct.size == 1, s"$ids")
  }

  test("the ids carry a token of this session, so that SVGs saved from two pages never clash") {
    val token = SvgPlanRenderer.SessionToken
    assert(token.length >= 6 && token.forall(c => c >= 'a' && c <= 'z'), token)
    val ids = all(render(Circle(Pos.Origin, 20, Some(FillStyle(gradient)), None)), "[id]")
      .map(_.getAttribute("id"))
    assert(ids.nonEmpty)
    for id <- ids do assert(id.matches(s"smile$token\\d+-.*"), id)
  }

  test("each operation is drawn under its own affine as a matrix in logical units") {
    val scene = Picture(
      Rectangle(30, 20, Pos(3.25, 1.5), Some(FillStyle(red)), stroke(2)).rotateBy(30),
      Circle(Pos.Origin, 10, None, stroke(2)).scaleBy(3, 1).moveBy(-4.5, 7),
      Triangle(12, Some(FillStyle(gradient)), None).rotateBy(-37).scaleBy(1.5)
    )
    val (_, ops) = DrawPlan.of(scene, DeviceGrid(1), settings, 0L)
    val expected = ops.collect { case DrawOp.Path(m, _, _, _) => coefficients(m) }
    assertEquals(expected.size, 3)
    for density <- Seq(1.0, 2.0, 1.5) do
      val drawn = all(render(scene, density), "g[transform]")
        .map(g => matrix(g.getAttribute("transform")))
      assertEquals(drawn.size, expected.size)
      for (got, want) <- drawn.zip(expected); (g, w) <- got.zip(want) do
        assertEqualsDouble(g, w, 1e-12)
  }

  test("the view box is the logical frame, whatever the density") {
    val scene = Rectangle(10, 10, Pos(0.25, 0.25), Some(FillStyle(red)), None)
    for density <- Seq(1.0, 1.5, 2.0) do
      val root = render(scene, density)
      assertEquals(root.getAttribute("viewBox"), "-5 -5 11 11")
      assertEquals(root.getAttribute("width"), "11")
      assertEquals(root.getAttribute("height"), "11")
  }

  /** `svg` with the prefix of its rendering's ids taken out, and its embedded PNGs left out. */
  private def normalized(svg: String): String =
    "smile[a-z]+\\d+-".r
      .replaceAllIn(svg, "smile-")
      .replaceAll("data:image/png;base64,[^\"]*", "PNG")

  test("the output does not depend on the density but for the rasters it embeds") {
    val vectors = Picture(
      MaskGroup(Circle(Pos.Origin, 8, Some(FillStyle(green)), None), sprite(30, 20)),
      MaskGroup(
        Circle(Pos.Origin, 8, Some(FillStyle(green)), None),
        Rectangle(30, 20, Pos.Origin, Some(FillStyle(gradient)), None),
        inverted = true
      ).moveBy(40, 0),
      Circle(Pos.Origin, 20, Some(FillStyle(red)), None).crop(Bounds(-10.3, -10, 10, 10.6)),
      Point(Pos(4.3, 5.7), red),
      sprite(7, 5).moveTo(Pos(10.3, 4.6)).scaleBy(1.5)
    )
    val once = normalized(SvgPlanRenderer.string(vectors, settings, 1.0))
    for density <- Seq(1.5, 2.0, 3.0) do
      assertEquals(normalized(SvgPlanRenderer.string(vectors, settings, density)), once)
  }

  test("a bitmap of a picture is embedded at the density asked for") {
    val picture = Circle(Pos(0, 0), 6, Some(FillStyle(red)), None)
    val scene   = Picture(Bitmap.of(picture)).scaleBy(2)
    def embedded(density: Double): (Double, Vector[Double]) =
      val root  = render(scene, density)
      val image = all(root, "defs image").head
      val use   = all(root, "use").head
      (image.getAttribute("width").toDouble, matrix(use.getAttribute("transform")))
    val (once, onceMap)   = embedded(1.0)
    val (twice, twiceMap) = embedded(2.0)
    assertEqualsDouble(twice, 2 * once, 4.0)
    // Each embedded pixel is half as large.
    assertEqualsDouble(twiceMap(0), onceMap(0) / 2, 1e-9)
  }

  test("a resampled crop embeds only its own pixels with their edges repeated") {
    val crop   = fencedSprite.crop(fence).scaleBy(1.3)
    val root   = render(crop)
    val images = all(root, "defs image")
    assertEquals(images.size, 1)
    val image = images.head
    // Columns 3 to 9 and rows 2 to 6, and a border of one pixel around them.
    assertEquals(image.getAttribute("width"), "9")
    assertEquals(image.getAttribute("height"), "7")
    assertEquals(image.getAttribute("x"), "-1")
    assertEquals(image.getAttribute("y"), "-1")
    assert(all(root, "clipPath").nonEmpty, "the crop is not clipped")
  }

  test("a pixelated bitmap is embedded to be shown with its pixels' edges") {
    val pixelated = Picture(
      Bitmap(4, 4, pattern).withResampling(ResamplingHint.Pixelated)
    ).scaleBy(2.5)
    val root   = render(pixelated)
    val styled = all(root, "[style*='pixelated']")
    assert(styled.nonEmpty, "nothing asks for nearest-neighbor resampling")
    val smooth =
      Bitmap(4, 4, pattern).withResampling(ResamplingHint.Smooth)
    assert(all(render(Picture(smooth).scaleBy(2.5)), "[style*='pixelated']").isEmpty)
  }

  test("a mask is an alpha mask, and an inverted one cuts its shape out of a flood") {
    val disc   = Circle(Pos.Origin, 8, Some(FillStyle(green)), None)
    val square = Rectangle(30, 30, Pos.Origin, Some(FillStyle(red)), None)
    val kept   = render(MaskGroup(disc, square))
    val masks  = all(kept, "mask")
    assertEquals(masks.size, 1)
    assert(masks.head.getAttribute("style").contains("mask-type:alpha"))
    assert(all(kept, "filter").isEmpty)
    val cut = render(MaskGroup(disc, square, inverted = true))
    assertEquals(all(cut, "mask").size, 1)
    assertEquals(all(cut, "filter feFlood, filter feflood").size, 1)
    assertEquals(all(cut, "feComposite, fecomposite").head.getAttribute("operator"), "out")
  }

  test("a group's masks apply in order, each to a group around the ones before it") {
    val disc   = Circle(Pos.Origin, 8, Some(FillStyle(green)), None)
    val hole   = Circle(Pos.Origin, 3, Some(FillStyle(green)), None)
    val square = Rectangle(30, 30, Pos.Origin, Some(FillStyle(red)), None)
    val root   = render(MaskGroup(disc, MaskGroup(hole, square, inverted = true).moveBy(2, 0)))
    val masked = all(root, "g[mask]")
    assertEquals(masked.size, 2)
    val (outer, inner) = (masked(0), masked(1))
    assert(inner.parentNode eq outer)
    def maskOf(g: dom.Element): dom.Element =
      val id = g.getAttribute("mask").stripPrefix("url(#").stripSuffix(")")
      all(root, "mask").find(_.getAttribute("id") == id).getOrElse(fail(s"no mask $id"))
    // Only the inverted hole cuts its shape out of a flood.
    assertEquals(all(maskOf(inner), "filter, g[filter]").size, 1)
    assertEquals(all(maskOf(outer), "filter, g[filter]").size, 0)
    assertEquals(all(inner, "path, rect").size, 1)
  }

  test("a mask filled with a gradient has its gradient defined, and the mask refers to it") {
    // Issue #9: without the definition, the mask's fill refers to nothing and hides everything.
    val gradient = LinearGradient(Pos(-2, 0), Pos(2, 0), red, Color(0, 0, 255))
    val mask     = Rectangle(4, 4, Pos.Origin, Some(FillStyle(gradient)), None)
    val masked   = Rectangle(4, 4, Pos.Origin, Some(FillStyle(red)), None)
    val root     = render(MaskGroup(mask, masked))
    val defined  = all(root, "linearGradient, lineargradient").map(_.getAttribute("id")).toSet
    assertEquals(defined.size, 1)
    val masks = all(root, "mask")
    assertEquals(masks.size, 1)
    val fills = all(masks.head, "[fill]").map(_.getAttribute("fill")).filter(_.startsWith("url("))
    assertEquals(fills.map(_.stripPrefix("url(#").stripSuffix(")")).toSet, defined)
  }

  test("glyph strokes are round-joined whatever join the style asks for") {
    val miter = StrokeStyle(Color(0, 0, 255), width = 3, join = StrokeStyle.Join.Miter)
    val text  = smile.testing.Cases.boxedText(line = Some(miter))
    val drawn = all(render(text), "text")
    assertEquals(drawn.size, 1)
    assertEquals(drawn.head.getAttribute("stroke-linejoin"), "round")
    assertEquals(drawn.head.getAttribute("stroke-width"), "3")
  }

  test("a turned clip is a clip path of its rectangle under the clip's own affine") {
    val turned = Circle(Pos.Origin, 20, Some(FillStyle(red)), None)
      .crop(Bounds(-10.3, -12.7, 15.2, 8.1))
      .rotateBy(30)
    val (_, ops) = DrawPlan.of(turned, DeviceGrid(1), settings, 0L)
    val clip     = ops.collectFirst { case DrawOp.Layer(_, Some(c), _, _) => c }.get
    val rects    = all(render(turned), "clipPath rect[transform]")
    assertEquals(rects.size, 1)
    for (g, w) <- matrix(rects.head.getAttribute("transform")).zip(coefficients(clip.m)) do
      assertEqualsDouble(g, w, 1e-12)
    assertEquals(rects.head.getAttribute("x").toDouble, clip.rect.upperLeftCorner.x)
  }

  test("an axis-aligned clip is a clip path of the device pixels it covers") {
    val root = render(
      Circle(Pos.Origin, 40, Some(FillStyle(red)), None).crop(Bounds(-10.3, -12.7, 15.2, 8.1))
    )
    val rects = all(root, "clipPath rect")
    assertEquals(rects.size, 1)
    val r = rects.head
    assertEquals(
      Seq("x", "y", "width", "height").map(r.getAttribute),
      Seq("-11", "-13", "27", "22")
    )
  }

  test("a fractional wrap defines one raster tile and fills with its normalized pattern") {
    val filled   = Rectangle(20.5, 10.25, Pos.Origin, Some(FillStyle(red)), None).wrapShift(5.3, 2)
    val root     = render(filled)
    val patterns = all(root, "pattern")
    assertEquals(patterns.size, 1)
    val pattern = patterns.head
    assertEquals(pattern.getAttribute("patternUnits"), "userSpaceOnUse")
    assertEquals(pattern.getAttribute("width"), "21")
    assertEquals(pattern.getAttribute("height"), "10")
    assertEquals(all(pattern, "use").size, 1)
    assertEquals(all(root, "image").size, 1)
    assertEquals(all(root, s"rect[fill='url(#${pattern.getAttribute("id")})']").size, 1)
    // The pattern repeats from where the plan puts its copies, a whole number of tiles apart.
    val placed   = matrix(pattern.getAttribute("patternTransform"))
    val (_, ops) = DrawPlan.of(filled, DeviceGrid(1), settings, 0L, svg = true)
    val copies   = tiles(ops).head.copies
    assert(copies.size > 1, "the wrap is planned as a single copy")
    for copy <- copies do
      assertEquals(coefficients(copy).take(4), placed.take(4))
      val (dx, dy) = (copy.e - placed(4), copy.f - placed(5))
      val across   = (dx * placed(3) - dy * placed(2)) / 21
      val down     = (dy * placed(0) - dx * placed(1)) / 10
      assertEqualsDouble(across, math.rint(across), 1e-9)
      assertEqualsDouble(down, math.rint(down), 1e-9)
  }

  test("a whole-pixel wrap keeps its vectors and defines its tile only once") {
    val root =
      render(Rectangle(100, 40, Pos(50, 20), Some(FillStyle(gradient)), None).wrapShift(37, 23))
    assertEquals(all(root, "pattern").size, 1)
    assertEquals(all(root, "pattern use").size, 1)
    assertEquals(all(root, "image").size, 0)
    assertEquals(all(root, "linearGradient").size, 1)
    // The tile is the rectangle's own 100 by 40 pixels, repeated from the shift.
    val pattern = all(root, "pattern").head
    assertEquals(pattern.getAttribute("width"), "100")
    assertEquals(pattern.getAttribute("height"), "40")
    val placed = matrix(pattern.getAttribute("patternTransform"))
    assertEquals(placed.take(4), Vector(1.0, 0.0, 0.0, 1.0))
    assertEquals(math.floorMod(placed(4).toInt, 100), 37)
    assertEquals(math.floorMod(placed(5).toInt, 40), 23)
    assertEquals(placed(4), math.rint(placed(4)))
    assertEquals(placed(5), math.rint(placed(5)))
  }

  test("the empty picture is one transparent pixel") {
    val root = render(Picture())
    assertEquals(root.getAttribute("viewBox"), "0 0 1 1")
    assertEquals(all(root, "*").size, 0)
  }

  test("the string is the serialized element") {
    val text = SvgPlanRenderer.string(Point(Pos(0.5, 0.5), red), settings)
    assert(text.startsWith("<svg"), text)
    assert(text.contains("xmlns=\"http://www.w3.org/2000/svg\""), text)
  }

  for d <- DeepTrees.nested; density <- Seq(1.5, 2.0) do
    test(s"${d.name} embeds the same rasters at any nesting limit at density $density") {
      val tree                                  = d.build(30)
      def rasters(nesting: Int): Vector[String] =
        all(SvgPlanRenderer.render(tree, settings, density, 0L, nesting), "image").map(href).sorted
      val unbounded = rasters(Int.MaxValue)
      val checks    = Seq(1, 2, 16).map: nesting =>
        sameRasters(rasters(nesting), unbounded).map: same =>
          assert(same, s"the rasters differ at nesting $nesting")
      Future.sequence(checks)
    }

  // jsdom takes time cubic in the depth to nest elements. 900 levels, about where the turned crops
  // overflowed Node's stack before plans were bounded, took 450 s for these, and 10000 would take
  // days. 300 levels draw about 20 slots of each fold.
  for d <- DeepTrees.nested do
    test(s"${d.name} renders at 300 levels") {
      val root = render(d.build(300))
      assert(root.getAttribute("viewBox") != "0 0 1 1", "nothing is drawn")
    }
end SvgPlanRendererSuite
