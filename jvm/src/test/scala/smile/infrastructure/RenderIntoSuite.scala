package smile.infrastructure

import smile.{RenderSettings, ResamplingMode}
import smile.modeling.Pos
import smile.pictures.*
import smile.pictures.FillStyle

import java.awt.{AlphaComposite, BasicStroke, Graphics2D, RenderingHints}
import java.awt.geom.Area
import java.awt.image.BufferedImage

/** `renderInto` on the JVM. A picture drawn onto a caller's graphics draws what its bitmap drawn at
  * the graphics' translation draws, stays inside its clip and leaves its state as it was.
  */
class RenderIntoSuite extends munit.FunSuite:

  import PlanRenderCases.*

  private val settings = RenderSettings.Default

  private def image(width: Int, height: Int): BufferedImage =
    new BufferedImage(width.max(1), height.max(1), BufferedImage.TYPE_INT_ARGB)

  private def argb(image: BufferedImage): Array[Int] =
    image.getRGB(0, 0, image.getWidth, image.getHeight, null, 0, image.getWidth)

  /** Runs `draw` on graphics of `image` and disposes of them. */
  private def drawOn(image: BufferedImage)(draw: Graphics2D => Unit): BufferedImage =
    val g = image.createGraphics()
    try draw(g)
    finally g.dispose()
    image

  private def square(size: Double, center: Pos): Element =
    Rectangle(size, size, center, Some(FillStyle(red)), None)

  for scale <- Seq(1.0, 1.5, 2.0); c <- cases do
    test(
      s"renderInto on a transparent image draws the bitmap createBitmapFrom gives at its corner: ${c.label} at $scale"
    ) {
      val bitmap   = Renderer().createBitmapFrom(c.element, scale, settings, 0L)
      val expected = bitmap.pixels(settings)
      val drawn    = drawOn(image(expected.width, expected.height)): g =>
        Renderer().renderInto(g, c.element, scale, settings, 0L)
      assertEquals((drawn.getWidth, drawn.getHeight), (expected.width, expected.height))
      assertEquals(argb(drawn).toSeq, expected.argbData.toSeq)
    }

  /** The pixels of `image` that are not transparent. */
  private def inked(image: BufferedImage): Seq[(Int, Int)] =
    for
      y <- 0 until image.getHeight
      x <- 0 until image.getWidth
      if image.getRGB(x, y) != 0
    yield (x, y)

  test("the corner of the picture's device box lands on the target's translation, rounded") {
    // The bounds run from (-5.5, -5.5) to (-4.5, -4.5), so the device box from (-6, -6) is 2 wide.
    val drawn = drawOn(image(40, 40)): g =>
      g.translate(10.4, 20.6)
      Renderer().renderInto(g, square(1, Pos(-5, -5)), 1.0, settings, 0L)
    assertEquals(inked(drawn), Vector((10, 21), (11, 21), (10, 22), (11, 22)))
  }

  test("translating further by the bounds' corner puts the origin on the translation") {
    // As on a high-DPI screen, the transform scales by the factor, so the corner is in logical
    // pixels. The corner (-1, -1) is on whole device pixels, so the origin lands exactly.
    val picture = square(2, Pos.Origin)
    val corner  = picture.bounds.get.upperLeftCorner
    val drawn   = drawOn(image(40, 40)): g =>
      g.scale(2, 2)
      g.translate(5, 10)
      g.translate(corner.x, corner.y)
      Renderer().renderInto(g, picture, 2.0, settings, 0L)
    assertEquals(inked(drawn), for y <- 18 until 22; x <- 8 until 12 yield (x, y))
  }

  test("renderInto draws only inside the target's clip") {
    for c <- Seq(
        square(20, Pos(10, 10)),
        MaskGroup(square(20, Pos(10, 10)), square(20, Pos(10, 10)))
      )
    do
      val drawn = drawOn(image(20, 20)): g =>
        g.clipRect(3, 4, 5, 6)
        Renderer().renderInto(g, c, 1.0, settings, 0L)
      for y <- 0 until 20; x <- 0 until 20 do
        val inside = x >= 3 && x < 8 && y >= 4 && y < 10
        assertEquals(drawn.getRGB(x, y) != 0, inside, s"pixel ($x, $y)")
  }

  test("a clip on a translated target limits the drawing where the clip is on the device") {
    val square20 = square(20, Pos.Origin)
    for c <- Seq(square20, MaskGroup(square20, square20)) do
      val drawn = drawOn(image(40, 40)): g =>
        g.translate(10, 5)
        g.clipRect(3, 4, 5, 6)
        Renderer().renderInto(g, c, 1.0, settings, 0L)
      for y <- 0 until 40; x <- 0 until 40 do
        val inside = x >= 13 && x < 18 && y >= 9 && y < 15
        assertEquals(drawn.getRGB(x, y) != 0, inside, s"pixel ($x, $y)")
  }

  test("of a target transform that also scales, only its translation places the pixels") {
    val mask     = cases.find(_.label == "a mask").get.element
    val expected = Renderer().createBitmapFrom(mask, 2.0, settings, 0L).pixels(settings)
    val (x, y)   = (7, 5)
    val drawn    = drawOn(image(expected.width + 20, expected.height + 20)): g =>
      g.translate(7.3, 4.6)
      g.scale(3, 1.5)
      g.rotate(0.2)
      Renderer().renderInto(g, mask, 2.0, settings, 0L)
    val placed = image(expected.width + 20, expected.height + 20)
    placed.setRGB(x, y, expected.width, expected.height, expected.argbData, 0, expected.width)
    assertEquals(argb(drawn).toSeq, argb(placed).toSeq)
  }

  test("renderInto leaves the target's transform, clip, composite and hints as they were") {
    drawOn(image(40, 40)): g =>
      g.translate(3.5, 4)
      g.scale(2, 2)
      g.clipRect(1, 1, 10, 10)
      g.setComposite(AlphaComposite.SrcOver.derive(0.5f))
      g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_OFF)
      g.setRenderingHint(
        RenderingHints.KEY_INTERPOLATION,
        RenderingHints.VALUE_INTERPOLATION_BICUBIC
      )
      g.setPaint(java.awt.Color.MAGENTA)
      g.setStroke(new BasicStroke(3))
      val (transform, clip, composite, hints, paint, stroke) =
        (g.getTransform, g.getClip, g.getComposite, g.getRenderingHints, g.getPaint, g.getStroke)
      Renderer().renderInto(g, cases.find(_.label == "a mask").get.element, 2.0, settings, 0L)
      assertEquals(g.getTransform, transform)
      assert(new Area(g.getClip).equals(new Area(clip)), s"${g.getClip} was $clip")
      assertEquals(g.getComposite, composite)
      assertEquals(g.getRenderingHints, hints)
      assertEquals(g.getPaint, paint)
      assertEquals(g.getStroke, stroke)
  }

  test("the target's own hints and composite do not change the pixels") {
    val mask     = cases.find(_.label == "a mask").get.element
    val expected = Renderer().createBitmapFrom(mask, 2.0, settings, 0L).pixels(settings)
    val drawn    = drawOn(image(expected.width, expected.height)): g =>
      g.setComposite(AlphaComposite.SrcOver.derive(0.5f))
      g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_OFF)
      g.setRenderingHint(RenderingHints.KEY_RENDERING, RenderingHints.VALUE_RENDER_SPEED)
      g.setRenderingHint(
        RenderingHints.KEY_ALPHA_INTERPOLATION,
        RenderingHints.VALUE_ALPHA_INTERPOLATION_SPEED
      )
      Renderer().renderInto(g, mask, 2.0, settings, 0L)
    assertEquals(argb(drawn).toSeq, expected.argbData.toSeq)
  }

  test("the empty picture draws nothing") {
    val drawn = drawOn(image(4, 4))(Renderer().renderInto(_, Empty))
    assert(argb(drawn).forall(_ == 0))
  }

  test("a Stepwise rendering at 2, and a factor of 0, are rejected") {
    val stepwise = settings.copy(resamplingMode = ResamplingMode.Stepwise)
    drawOn(image(4, 4)): g =>
      val _ = intercept[IllegalArgumentException](
        Renderer().renderInto(g, square(1, Pos.Origin), 2.0, stepwise)
      )
      val _ =
        intercept[IllegalArgumentException](Renderer().renderInto(g, square(1, Pos.Origin), 0))
  }

  /** An opaque white image of the device pixels `box`. */
  private def white(box: DeviceBox): BufferedImage =
    drawOn(image(box.width, box.height)): g =>
      g.setColor(java.awt.Color.WHITE)
      g.fillRect(0, 0, box.width.max(1), box.height.max(1))

  /** The largest difference in any channel between two packed ARGB pixels. */
  private def difference(a: Int, b: Int): Int =
    (0 until 32 by 8).map(shift => (((a >>> shift) & 0xff) - ((b >>> shift) & 0xff)).abs).max

  for antialiased <- Seq(false, true) do
    test(
      s"over an opaque background, renderInto equals drawing the bitmap over it, antialiased: $antialiased"
    ) {
      val drawing   = settings.copy(drawingIsAntiAliased = antialiased)
      val tolerance = if antialiased then 1 else 0
      val problems  =
        for
          scale <- Seq(1.0, 2.0)
          c     <- cases
          box     = DrawPlan.planned(c.element, DeviceGrid(scale), drawing, 0L).box
          direct  = drawOn(white(box))(Renderer().renderInto(_, c.element, scale, drawing, 0L))
          bitmap  = Renderer().createBitmapFrom(c.element, scale, drawing, 0L).toAWTImage(drawing)
          blitted = drawOn(white(box))(g => assert(g.drawImage(bitmap, 0, 0, null)))
          worst   = argb(direct).zip(argb(blitted)).map(difference).maxOption.getOrElse(0)
          if worst > tolerance
        yield s"${c.label} at $scale differs by $worst"
      assert(problems.isEmpty, problems.mkString("\n"))
    }
end RenderIntoSuite
