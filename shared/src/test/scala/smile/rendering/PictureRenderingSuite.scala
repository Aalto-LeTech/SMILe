package smile.rendering

import smile.colors.Color
import smile.infrastructure.Renderer
import smile.modeling.{Bounds, Pos}
import smile.pictures.*

/** Rendering that runs on both platforms, the JVM against AWT and Scala.js against a jsdom canvas:
  * stacking order and viewports, mask groups, strokes beneath and around fills, and display scale
  * factors (issue #6). Pixel assertions use opaque colors because canvas stores premultiplied
  * alpha, which makes low-alpha channel values approximate on JS. The JVM-only `JVMRenderingSuite`
  * covers translucent channel fidelity.
  */
class PictureRenderingSuite extends munit.FunSuite:

  private val red   = Color(255, 0, 0)
  private val green = Color(0, 255, 0)
  private val blue  = Color(0, 0, 255)

  test("the first Picture element is rendered in front") {
    val front   = Rectangle(2, 2, Pos(1, 1), fillWith(red), line = None)
    val back    = Rectangle(2, 2, Pos(1, 1), fillWith(blue), line = None)
    val picture = Picture(front, back)
      .crop(Bounds(Pos(0, 0), Pos(2, 2)))

    assertEquals(picture.toBitmap().getColor(1, 1), red)
  }

  test("a crop's toBitmap covers exactly the crop") {
    val rectangle = Rectangle(4, 4, Pos(2, 2), fillWith(red), line = None)
    val picture   = rectangle
      .crop(Bounds(Pos(0, 0), Pos(10, 8)))

    val bitmap = picture.toBitmap()

    assertEquals(bitmap.pixels().width, 10)
    assertEquals(bitmap.pixels().height, 8)
    assertEquals(bitmap.bounds.get, Bounds(Pos(0, 0), Pos(10, 8)))
  }

  test("blank space renders a transparent bitmap of its size") {
    val picture = blank(10, 6).moveBy(5, 3)

    val bitmap = picture.toBitmap()

    assertEquals(bitmap.pixels().width, 10)
    assertEquals(bitmap.pixels().height, 6)
    assert(bitmap.pixels().colors.forall(_.opacity == 0))
  }

  test("crop accepts world coordinates inside an element's bounds") {
    val circle  = Circle(Pos(300, 300), 50, fillWith(red), line = None)
    val cropped = circle.crop(Bounds(260, 260, 340, 340)).toBitmap()

    assertEquals(cropped.pixels().width, 80)
    assertEquals(cropped.pixels().height, 80)
  }

  test("a negative arc angle renders the mirrored sweep on both platforms") {
    val negativeQuarter = Arc(
      pos = Pos.Origin,
      width = 100,
      height = 80,
      startAngle = 0,
      arcAngle = -90,
      fill = fillWith(red),
      line = None
    )

    val bitmap = negativeQuarter.toBitmap()

    // The bounds are tight over the mirrored quarter, which lies below the center.
    assertEquals(bitmap.pixels().width, 50)
    assertEquals(bitmap.pixels().height, 40)
    // This sample lies between the arc and its closing chord.
    assertEquals(bitmap.getColor(35, 15), red)
  }

  test("a Point renders as its color instead of an empty bitmap") {
    // A point is a 1x1 square around its position, here on one whole pixel.
    val bitmap = Point(Pos(0.5, 0.5), red).toBitmap()

    assertEquals(bitmap.pixels().width, 1)
    assertEquals(bitmap.pixels().height, 1)
    assertEquals(bitmap.getColor(0, 0), red)
  }

  test("MaskGroup composites at the mask's world position") {
    val mask   = Rectangle(2, 2, Pos(10, 10), fillWith(green), line = None)
    val masked = Rectangle(6, 6, Pos(10, 10), fillWith(red), line = None)
    val group  = MaskGroup(mask, masked)

    val picture = Picture(group).crop(Bounds(Pos(7, 7), Pos(13, 13)))
    val bitmap  = Renderer().createBitmapFrom(picture)

    // Inside the mask's box the red content shows. Outside it, the content is clipped
    // away, even though the mask sits far from the world origin.
    assert(bitmap.getColor(3, 3).opacity > 0, "center pixel should be masked in")
    assertEquals(bitmap.getColor(0, 0).opacity, 0, "pixel outside the mask should be clipped")
  }

  test("a mask half past its content renders the part of the mask over the content") {
    // The content covers 70..130 x 35..65 and the mask 118..142 x 38..62, so the group covers
    // 118..130 x 38..62.
    val content = Rectangle(60, 30, Pos(100, 50), fillWith(blue), line = None)
    val mask    = Circle(Pos(130, 50), 12, fillWith(green), line = None)

    val bitmap = MaskGroup(mask, content).toBitmap()

    assertEquals(bitmap.pixels().width, 12)
    assertEquals(bitmap.pixels().height, 24)
    assertEquals(bitmap.getColor(6, 12), blue)
  }

  test("a masked circle at scale 2 has the edge of an unmasked one, not an enlarged 1x edge") {
    // A mask composited at 1x and enlarged has an edge twice as wide as one drawn at 2x. The edge
    // has as many different alphas, but each on more pixels.
    def edge(picture: Element): (Int, Int) =
      val partial = Renderer()
        .createBitmapFrom(picture, 2.0)
        .pixels()
        .argbData
        .map(_ >>> 24)
        .filter(alpha => alpha > 0 && alpha < 255)
      (partial.length, partial.toSet.size)

    val frame      = Bounds(Pos(0, 0), Pos(40, 40))
    val circle     = Circle(Pos(20, 20), 15, fillWith(red), line = None)
    val everything = Rectangle(40, 40, Pos(20, 20), fillWith(green), line = None)
    val unmasked   = Picture(circle).crop(frame)
    val masked     = Picture(MaskGroup(circle, everything)).crop(frame)

    val (unmaskedPixels, unmaskedAlphas) = edge(unmasked)
    val (maskedPixels, maskedAlphas)     = edge(masked)
    assert(unmaskedAlphas > 8, s"the unmasked circle has $unmaskedAlphas edge alphas")
    assert(
      maskedAlphas >= unmaskedAlphas * 3 / 4,
      s"alphas, masked: $maskedAlphas, unmasked: $unmaskedAlphas"
    )
    assert(
      maskedPixels <= unmaskedPixels * 5 / 4,
      s"edge pixels, masked: $maskedPixels, unmasked: $unmaskedPixels"
    )
  }

  test("MaskGroup masks by opacity, not by the mask's colors") {
    // The mask is fully black and fully opaque. Luminance masking would hide everything, and
    // alpha masking keeps everything under it.
    val blackMask = Rectangle(2, 2, Pos(1, 1), fillWith(Color(0, 0, 0)), line = None)
    val masked    = Rectangle(2, 2, Pos(1, 1), fillWith(red), line = None)

    val bitmap = Renderer().createBitmapFrom(MaskGroup(blackMask, masked))

    assertEquals(bitmap.getColor(1, 1), red)
  }

  test("MaskGroup keeps every masked element, not only the frontmost one") {
    // Two disjoint content rectangles lie under one covering mask. Compositing the mask once per
    // element erases the earlier element outside the later one's area.
    val mask   = Rectangle(8, 4, Pos(4, 2), fillWith(green), line = None)
    val masked = Picture(
      Rectangle(2, 2, Pos(2, 2), fillWith(red), line = None),
      Rectangle(2, 2, Pos(6, 2), fillWith(blue), line = None)
    )

    val bitmap = Renderer().createBitmapFrom(MaskGroup(mask, masked))

    // The group covers the content, 1..7 x 1..3, so world (2, 2) is pixel (1, 1).
    assertEquals(bitmap.getColor(1, 1), red)
    assertEquals(bitmap.getColor(5, 1), blue)
  }

  test("an inverted MaskGroup keeps the content outside the mask and cuts a hole under it") {
    val mask   = Rectangle(2, 2, Pos(5, 5), fillWith(green), line = None)
    val masked = Rectangle(10, 10, Pos(5, 5), fillWith(red), line = None)
    val group  = MaskGroup(mask, masked, inverted = true)

    // The group reaches as far as the content, since the content shows wherever the mask is not.
    assertEquals(group.bounds.get, masked.bounds.get)
    val bitmap = Renderer().createBitmapFrom(Picture(group))
    assertEquals((bitmap.pixels().width, bitmap.pixels().height), (10, 10))
    assertEquals(bitmap.getColor(1, 1), red)
    assertEquals(bitmap.getColor(8, 8), red)
    assertEquals(bitmap.getColor(4, 4).opacity, 0)
    assertEquals(bitmap.getColor(5, 5).opacity, 0)
  }

  test("an inverted MaskGroup is the content where the mask it inverts is not") {
    // At every pixel, the two opacities add up to the content's, within rounding. The mask is a
    // circle, so its edge has every partial opacity.
    val frame  = Bounds(Pos(0, 0), Pos(30, 30))
    val circle = Circle(Pos(15, 15), 9, fillWith(green), line = None)
    val masked = Rectangle(30, 30, Pos(15, 15), fillWith(red), line = None)
    for scale <- Seq(1.0, 2.0) do
      def alphas(inverted: Boolean) =
        Renderer()
          .createBitmapFrom(
            Picture(MaskGroup(circle, masked, inverted)).crop(frame),
            scale
          )
          .pixels()
          .argbData
          .map(_ >>> 24)
      val sums = alphas(false).zip(alphas(true)).map(_ + _)
      assert(
        sums.forall(sum => sum >= 254 && sum <= 256),
        s"scale $scale: ${sums.distinct.mkString(", ")}"
      )
  }

  test("a MaskGroup masks with a whole picture") {
    // Two separate squares mask together, which no single element could do.
    val mask = Picture(
      Rectangle(2, 2, Pos(1, 1), fillWith(green), line = None),
      Rectangle(2, 2, Pos(7, 1), fillWith(green), line = None)
    )
    val masked = Rectangle(8, 2, Pos(4, 1), fillWith(red), line = None)
    val frame  = Bounds(Pos(0, 0), Pos(8, 2))

    val kept =
      Renderer().createBitmapFrom(Picture(MaskGroup(mask, masked)).crop(frame))
    assertEquals(kept.getColor(1, 1), red)
    assertEquals(kept.getColor(7, 1), red)
    assertEquals(kept.getColor(4, 1).opacity, 0)

    val cut = Renderer().createBitmapFrom(
      Picture(MaskGroup(mask, masked, inverted = true)).crop(frame)
    )
    assertEquals(cut.getColor(1, 1).opacity, 0)
    assertEquals(cut.getColor(7, 1).opacity, 0)
    assertEquals(cut.getColor(4, 1), red)

  }

  test("moving a MaskGroup moves the mask and the masked content together") {
    val mask   = Rectangle(2, 2, Pos(10, 10), fillWith(green), line = None)
    val masked = Rectangle(6, 6, Pos(11, 11), fillWith(red), line = None)
    val group  = MaskGroup(mask, masked)

    val moved = group.moveTo(Pos(50, 50))

    // The whole group lands at the new position, by the masked content's anchor.
    assertEquals(moved.anchor, Pos(50, 50))
    assertEquals(moved.bounds, group.bounds.map(_.moveBy(39, 39)))
    // It draws there as it did before the move.
    assertEquals(
      moved.toBitmap().pixels().colors,
      group.toBitmap().pixels().colors
    )
  }

  test("a Line's square end caps paint on both platforms") {
    val line   = Line(Pos(2, 2), Pos(8, 2), StrokeStyle(blue, width = 4))
    val bitmap = Renderer().createBitmapFrom(line)

    // The boundary reserves the stroke radius (2px) around the segment, and the square caps put
    // ink past both endpoints. A backend that closes the two-point path suppresses the caps and
    // leaves the cap pixels fully empty. The cap pixels are asserted present, and may be partly
    // transparent, because AWT's stroke normalization may shift the outline by half a pixel.
    assertEquals(bitmap.pixels().width, 10)
    assertEquals(bitmap.pixels().height, 4)
    assert(bitmap.getColor(0, 1).opacity > 0, "the start cap should paint past the endpoint")
    assert(bitmap.getColor(9, 2).opacity > 0, "the end cap should paint past the endpoint")
    assertEquals(bitmap.getColor(5, 1), blue)
  }

  test("a stroked Circle's bounds hold its outline instead of clipping it") {
    val circle = Circle(Pos(10, 10), 8, fill = None, line = Some(StrokeStyle(blue, width = 4)))
    val bitmap = Renderer().createBitmapFrom(circle)

    // The ellipse box is 16x16 and the stroke paints 2px past it on every side. A boundary that
    // stops at the box cuts the outline's outer half off.
    assertEquals(bitmap.pixels().width, 20)
    assertEquals(bitmap.pixels().height, 20)
    // The outline's topmost run is inside the bitmap, above the ellipse box.
    assert(bitmap.getColor(10, 1).opacity > 0, "the outline should paint above the ellipse box")
    assert(bitmap.getColor(1, 10).opacity > 0, "the outline should paint left of the ellipse box")
  }

  test("a stroke with onTop = false paints beneath the fill on both platforms") {
    def bitmap(onTop: Boolean) =
      val rectangle = Rectangle(
        6,
        6,
        Pos(4, 4),
        fillWith(red),
        Some(StrokeStyle(blue, width = 4, onTop = onTop))
      )
      Renderer().createBitmapFrom(rectangle)

    val strokeBelow = bitmap(onTop = false)
    val strokeAbove = bitmap(onTop = true)

    // World pixel [1,2]x[4,5] lies where the fill and the stroke band overlap. The
    // rectangle's edge runs at x=1 with a 2px stroke radius on each side. Whichever
    // paints last owns it.
    assertEquals(strokeBelow.getColor(2, 5), red)
    assertEquals(strokeAbove.getColor(2, 5), blue)
    // The rectangle's middle is beyond the stroke band either way.
    assertEquals(strokeBelow.getColor(5, 5), red)
    assertEquals(strokeAbove.getColor(5, 5), red)
  }

  test("a stroked Text with onTop = false paints beneath the glyphs on both platforms") {
    def rendered(onTop: Boolean) =
      // Custom bounds keep the element renderable on both platforms, as jsdom cannot
      // measure glyphs, so text without them does not paint there. The viewport
      // leaves room around the layout box for the outer half of the stroke.
      val text = Text(
        pos = Pos(20, 15),
        customBounds = Some(Bounds(Pos(4, 3), Pos(36, 27))),
        content = "H",
        font = "sans-serif",
        size = 20,
        weight = 700,
        fill = fillWith(red),
        line = Some(StrokeStyle(blue, width = 4, onTop = onTop))
      )
      val picture = text.crop(Bounds(Pos(0, 0), Pos(40, 30)))
      Renderer().createBitmapFrom(picture)

    // Glyph outlines land on different pixels per platform and font, so the test
    // counts pixels. Whichever paint goes last owns the band where the outline and
    // the fill overlap.
    def fillPixels(bitmap: Bitmap)   = bitmap.pixels().colors.count(c => c.red > c.blue)
    def strokePixels(bitmap: Bitmap) = bitmap.pixels().colors.count(c => c.blue > c.red)

    val strokeBelow = rendered(onTop = false)
    val strokeAbove = rendered(onTop = true)

    assert(fillPixels(strokeBelow) > 0, "the glyph should be filled at all")
    assert(strokePixels(strokeBelow) > 0, "the outline should be stroked at all")
    assert(
      strokePixels(strokeAbove) > strokePixels(strokeBelow),
      s"a stroke on top should cover more of the glyph: " +
        s"${strokePixels(strokeAbove)} vs ${strokePixels(strokeBelow)}"
    )
    assert(
      fillPixels(strokeBelow) > fillPixels(strokeAbove),
      s"a stroke beneath should leave more fill showing: " +
        s"${fillPixels(strokeBelow)} vs ${fillPixels(strokeAbove)}"
    )
  }

  test("a display scale factor densifies the buffer and keeps the logical bounds (#6)") {
    val rectangle = Rectangle(4, 4, Pos(2, 2), fillWith(red), line = None)
    val picture   = rectangle

    val unscaled = Renderer().createBitmapFrom(picture)
    val scaled   = Renderer().createBitmapFrom(picture, 2.0)

    assertEquals(unscaled.pixels().width, 4)
    assertEquals(unscaled.pixels().height, 4)
    assertEquals(scaled.pixels().width, 8)
    assertEquals(scaled.pixels().height, 8)

    // (4, 4) is past the unscaled buffer entirely, but is inside the denser one.
    // Pixel coordinates follow the buffer.
    assertEquals(unscaled.colorAt(4, 4), None)
    assertEquals(scaled.getColor(4, 4), red)
    assertEquals(scaled.getColor(7, 7), red)

    // The logical model is untouched, with the same world bounds and world size.
    assertEquals(scaled.bounds.get, unscaled.bounds.get)
    assertEquals(scaled.bounds.get.width, 4.0)
    assertEquals(scaled.bounds.get.height, 4.0)
  }

  test("scale factor 1 renders exactly what the unscaled path renders (#6)") {
    val picture = Picture(
      Circle(Pos(5, 5), 4, fillWith(blue), line = None),
      Rectangle(3, 2, Pos(2, 8), fillWith(green), line = None)
    ).crop(Bounds(Pos(0, 0), Pos(10, 10)))

    val unscaled = Renderer().createBitmapFrom(picture)
    val atOne    = Renderer().createBitmapFrom(picture, 1.0)

    assertEquals(atOne.pixels().width, unscaled.pixels().width)
    assertEquals(atOne.pixels().height, unscaled.pixels().height)
    assertEquals(atOne.bounds.get, unscaled.bounds.get)
    assertEquals(atOne.pixels().colors, unscaled.pixels().colors)
  }

  test("a fractional scale factor covers the bounds with whole device pixels (#6)") {
    val picture = Rectangle(5, 5, Pos(2.5, 2.5), fillWith(red), line = None)

    val scaled = Renderer().createBitmapFrom(picture, 1.5)

    // 5 logical pixels * 1.5 = 7.5, covered by 8 device pixels. Nothing is floored away.
    assertEquals(scaled.pixels().width, 8)
    assertEquals(scaled.pixels().height, 8)
    // The bounds are those device pixels in logical units.
    assertEquals(scaled.bounds.get.width, 8 / 1.5)
    // The last device pixel holds the edge of the rectangle, half covered.
    val edge = scaled.getColor(7, 3).opacity
    assert(edge > 96 && edge < 160, s"half-covered edge pixel: $edge")
  }

  test("a display-scaled bitmap keeps its logical size when composed into another picture") {
    val rectangle = Rectangle(4, 4, Pos(2, 2), fillWith(red), line = None)
    val dense     = Renderer().createBitmapFrom(rectangle, 2.0)

    // The recomposed dense bitmap must follow the 4x4 logical bounds of its 8x8
    // buffer, and the content must fill them.
    val recomposed = Renderer().createBitmapFrom(dense)

    assertEquals(recomposed.pixels().width, 4)
    assertEquals(recomposed.pixels().height, 4)
    assertEquals(recomposed.getColor(0, 0), red)
    assertEquals(recomposed.getColor(3, 3), red)
  }

  test("a non-positive scale factor is rejected (#6)") {
    val picture = Rectangle(4, 4, Pos(2, 2), fillWith(red), line = None)

    val _ = intercept[IllegalArgumentException]:
      Renderer().createBitmapFrom(picture, 0.0)

    intercept[IllegalArgumentException]:
      Renderer().createBitmapFrom(picture, -2.0)
  }

  test("blank space paints nothing but still takes part in layout") {
    val square = Rectangle(4, 4, Pos(2, 2), fillWith(red), line = None)

    val plain  = Picture(square).toBitmap()
    val marked = Picture(blank(1, 1).moveTo(Pos(2, 2)), square).toBitmap()

    // The blank is inside the square, so the bounds and every pixel are unchanged even
    // though the blank is in front.
    assertEquals(marked.bounds.get, plain.bounds.get)
    assertEquals(marked.pixels().colors, plain.pixels().colors)

    // Outside the square it does extend the picture's bounds. The square spans x 0..4 and the
    // blank 9.5..10.5, which the whole pixels up to 11 cover.
    val faraway = Picture(blank(1, 1).moveTo(Pos(10, 2)), square).toBitmap()
    assertEquals(faraway.pixels().width, 11)
  }

  test("#9 a gradient across a partial arc paints both of its end colors") {
    // The clockwise quarter arc occupies the lower right 50x40 of the 100x80 ellipse. Using the
    // ellipse's corner as the gradient origin puts both stops left of the painted pixels and makes
    // the arc uniformly blue.
    val gradient = smile.colors.LinearGradient(Pos(0, 0), Pos(50, 0), red, blue)
    val arc      = Arc(
      pos = Pos(50, 40),
      width = 100,
      height = 80,
      startAngle = 0,
      arcAngle = -90,
      fill = fillWith(gradient),
      line = None
    )

    val bitmap = Renderer().createBitmapFrom(arc)

    assertEquals(bitmap.pixels().width, 50)
    assertEquals(bitmap.pixels().height, 40)

    // Both samples lie well inside the area the arc and its closing chord enclose. The
    // backends interpolate gradients slightly differently, so only dominant channels count.
    val nearStart = bitmap.getColor(10, 35)
    val nearEnd   = bitmap.getColor(40, 15)

    assert(nearStart.red > nearStart.blue, s"the arc's left end should be red, was $nearStart")
    assert(nearEnd.blue > nearEnd.red, s"the arc's right end should be blue, was $nearEnd")
  }
