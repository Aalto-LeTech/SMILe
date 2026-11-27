package smile.pictures

import smile.colors.Color
import smile.modeling.{Affine, Bounds, Pos}
import smile.pictures.FillStyle

import java.awt.RenderingHints
import java.awt.image.BufferedImage
import java.io.IOException
import java.nio.file.Files

/** SVG pictures as elements on the JVM. Their document is kept and drawn under their affine. */
class SvgElementSuite extends munit.FunSuite:

  // A red left half and a transparent right half, with a thin black line in the middle.
  private val source = """<svg xmlns="http://www.w3.org/2000/svg" width="20" height="10">
    <rect width="10" height="10" fill="red"/>
    <rect x="9.9" width="0.2" height="10" fill="black"/></svg>"""

  /** What `svg` draws, rendered at `scale` device pixels per logical pixel into its bounds. */
  private def render(svg: SvgPicture, scale: Double): BufferedImage =
    val box    = svg.bounds.getOrElse(fail(s"$svg has no bounds"))
    val corner = box.upperLeftCorner
    val image  = new BufferedImage(
      (box.width * scale).ceil.toInt,
      (box.height * scale).ceil.toInt,
      BufferedImage.TYPE_INT_ARGB
    )
    val graphics = image.createGraphics()
    try
      graphics.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
      val toDevice = svg.affine
        .andThen(Affine.translation(-corner.x, -corner.y))
        .andThen(Affine.scaling(scale, scale))
      svg.document.render(graphics, toDevice)
    finally graphics.dispose()
    image

  test("an SVG picture is centered at the origin with its anchor in its middle") {
    val svg = SvgPicture.fromString(source)
    assertEquals(svg.bounds, Some(Bounds(-10, -5, 10, 5)))
    assertEquals(svg.anchor, Pos.Origin)
  }

  test("an SVG picture scaled (2, 1) keeps its document and is its box under the affine") {
    val svg    = SvgPicture.fromString(source)
    val scaled = svg.scaleBy(2, 1) match
      case s: SvgPicture => s
      case other         => fail(s"$other is not an SVG picture")
    assert(scaled.document eq svg.document)
    assertEquals(scaled.bounds, Some(Bounds(-20, -5, 20, 5)))
    val turned = scaled.rotateBy(90)
    assertEquals(turned.bounds, Some(Bounds(-5, -20, 5, 20)))
  }

  test("an SVG picture is drawn as vectors under its affine, sharp at any density") {
    val scaled = SvgPicture.fromString(source).scaleBy(2, 1) match
      case s: SvgPicture => s
      case other         => fail(s"$other is not an SVG picture")
    val image = render(scaled, 10)
    assertEquals((image.getWidth, image.getHeight), (400, 100))
    assertEquals(image.getRGB(100, 50), 0xffff0000)
    assertEquals(image.getRGB(300, 50) >>> 24, 0)
    // The line, 0.2 pixels of the document, is 4 device pixels wide. It is drawn as a vector at
    // that size, so its middle is solid black.
    assertEquals(image.getRGB(200, 50), 0xff000000)
    assertEquals(image.getRGB(195, 50), 0xffff0000)
    assertEquals(image.getRGB(205, 50) >>> 24, 0)
  }

  test("an SVG picture masks content as any element does") {
    val svg     = SvgPicture.fromString(source)
    val content = Rectangle(40, 40, Pos.Origin, Some(FillStyle(Color(0, 0, 255))), None)
    MaskGroup(svg, content) match
      case mask: MaskGroup =>
        assert(mask.masks.head._1 eq svg)
        assertEquals(mask.bounds, svg.bounds)
      case other => fail(s"$other is not a mask")
  }

  test("SVG pictures are equal by their document, transform and anchor") {
    val svg = SvgPicture.fromString(source)
    assertEquals(svg.moveBy(3, 4).moveBy(-3, -4), svg)
    assertEquals(svg.rotateBy(90).rotateBy(-90), svg)
    assertEquals(svg.moveBy(1, 2).hashCode, svg.moveBy(1, 2).hashCode)
    assertNotEquals(svg, SvgPicture.fromString(source))
    assertNotEquals(svg, svg.withAnchor(Pos(1, 1)))
  }

  test("SvgPicture(path) loads a file synchronously and refuses what is not there") {
    val file = Files.createTempFile("smile-svg", ".svg")
    try
      Files.writeString(file, source)
      assertEquals(SvgPicture(file.toString).bounds, Some(Bounds(-10, -5, 10, 5)))
    finally Files.delete(file)
    val _ = intercept[IOException](SvgPicture("no/such/picture.svg"))
    intercept[IOException](SvgPicture.fromString("<not-svg/>"))
  }
end SvgElementSuite
