package smile.rendering

import smile.RenderSettings
import smile.colors.Color
import smile.modeling.{Bounds, Pos}
import smile.pictures.*
import smile.testing.{Cases, InkAssertions}

/** A picture transformed as vectors and then rasterized should look like the picture rasterized and
  * then transformed as pixels. The two take entirely separate routes through the library. One uses
  * the elements' own geometry and the other a bitmap's resampling. So each checks the other, which
  * comparing a picture's transformation with its elements' cannot do, because the picture's is
  * implemented that way.
  */
class PictureRasterSuite extends munit.FunSuite with InkAssertions:

  private val red   = Cases.red
  private val blue  = Cases.blue
  private val green = Cases.green

  /** Whole-pixel blocks in three colors, placed lopsidedly. A mirrored or reversed transformation
    * gives a different picture, and a right-angle turn has no antialiased edge.
    */
  private val blocks = Picture(
    Rectangle(8, 4, Pos(-12, -8), fillWith(red), None),
    Rectangle(4, 10, Pos(6, 1), fillWith(blue), None),
    Rectangle(6, 2, Pos(-9, 7), fillWith(green), None)
  )

  private def dominant(color: Color): String =
    if color.opacity < 128 then "none"
    else if color.red > 128 then "red"
    else if color.blue > 128 then "blue"
    else if color.green > 128 then "green"
    else "none"

  /** The share of pixels on which two bitmaps of one size show different things. */
  private def disagreement(one: Bitmap, other: Bitmap): Double =
    assertEquals(
      (one.pixels().width, one.pixels().height),
      (other.pixels().width, other.pixels().height)
    )
    val pairs = one.pixels().colors.zip(other.pixels().colors)
    pairs.count((a, b) => dominant(a) != dominant(b)).toDouble / pairs.size

  for angle <- Seq(90.0, 180.0, 270.0, -90.0) do
    test(
      s"a picture turned by $angle degrees is its bitmap turned by $angle degrees, pixel for pixel"
    ) {
      val pivot  = blocks.anchor
      val vector = blocks.rotateAround(angle, pivot).toBitmap()
      val raster = blocks.toBitmap().rotateAround(angle, pivot)

      assertClose(raster.bounds.get, vector.bounds.get, 1e-9, "the same place")
      assertEquals(disagreement(vector, raster), 0.0)
    }

  test("a picture turned by an oblique angle covers what its turned bitmap covers") {
    for angle <- Seq(30.0, -55.0, 200.0) do
      val pivot  = blocks.anchor
      val canvas = Bounds(Pos(-40, -40), Pos(40, 40))
      val vector = blocks.rotateAround(angle, pivot).crop(canvas).toBitmap()
      val raster =
        blocks.toBitmap().rotateAround(angle, pivot).crop(canvas).toBitmap()

      // Resampling blurs the edges, and the areas agree but for a fringe.
      assert(disagreement(vector, raster) < 0.03, s"at $angle: ${disagreement(vector, raster)}")
  }

  test("a picture scaled by whole factors is its bitmap scaled, pixel for pixel") {
    for (h, v) <- Seq((2.0, 2.0), (3.0, 1.0), (1.0, 2.0)) do
      val vector = blocks.scaleBy(h, v).toBitmap()
      val raster = blocks.toBitmap().scaleBy(h, v).asBitmap

      assertClose(raster.bounds.get, vector.bounds.get, 1e-9, s"by ($h, $v)")
      assert(disagreement(vector, raster) < 0.02, s"by ($h, $v): ${disagreement(vector, raster)}")
  }

  test("a mirrored picture is its mirrored bitmap, and its flipped one") {
    val leftToRight = blocks.scaleBy(-1, 1).toBitmap()
    assertEquals(disagreement(leftToRight, blocks.toBitmap().scaleBy(-1, 1).asBitmap), 0.0)
    assertEquals(disagreement(leftToRight, blocks.toBitmap().flipHorizontally), 0.0)

    val topToBottom = blocks.scaleBy(1, -1).toBitmap()
    assertEquals(disagreement(topToBottom, blocks.toBitmap().scaleBy(1, -1).asBitmap), 0.0)
    assertEquals(disagreement(topToBottom, blocks.toBitmap().flipVertically), 0.0)

    val both = blocks.scaleBy(-1, -1).toBitmap()
    assertEquals(disagreement(both, blocks.toBitmap().scaleBy(-1, -1).asBitmap), 0.0)
    assertEquals(disagreement(both, blocks.toBitmap().flipDiagonally), 0.0)
    assertEquals(disagreement(both, blocks.rotateAround(180, blocks.anchor).toBitmap()), 0.0)

    // The lopsided layout makes each of them a different picture from the original.
    assert(disagreement(leftToRight, blocks.toBitmap()) > 0.1)
    assert(disagreement(topToBottom, blocks.toBitmap()) > 0.1)
  }

  test("a moved picture is the same pixels somewhere else") {
    val moved = blocks.moveBy(37, -12)
    assertEquals(moved.toBitmap().pixels().colors, blocks.toBitmap().pixels().colors)
    assertEquals(moved.bounds.get, blocks.bounds.get.moveBy(37, -12))
  }

  test("a picture's elements paint in the order of the list, front first, whatever is done to it") {
    val front    = Rectangle(10, 10, Pos(0, 0), fillWith(red), None)
    val back     = Rectangle(10, 10, Pos(4, 0), fillWith(blue), None)
    val stacked  = Picture(front, back)
    val pictures = Seq(stacked, stacked.rotateAround(180, stacked.anchor), stacked.scaleBy(-1, 1))

    for picture <- pictures do
      val bitmap  = picture.toBitmap()
      val overlap = picture.elements(0).bounds.get.center
      val spot    = overlap - picture.bounds.get.upperLeftCorner
      assertEquals(dominant(bitmap.getColor(spot.x.toInt, spot.y.toInt)), "red")
  }

  test("a crop moves with its picture: transforming it transforms its window too") {
    // A crop is a clip, which transforms as one thing with what it shows, so nothing it hides
    // reappears and what it shows stays in it.
    val window  = Bounds(Pos(-20, -15), Pos(15, 13))
    val picture = blocks.crop(window)

    assertEquals(picture.bounds, Some(window))
    assertEquals(picture.moveBy(100, 0).bounds, Some(window.moveBy(100, 0)))
    assertClose(
      picture.scaleAround(2, 2, Pos.Origin).bounds.get,
      Bounds(-40, -30, 30, 26),
      1e-9,
      ""
    )
    assertEquals(picture.rotateAround(90, Pos.Origin).width, window.height)

    // The moved window still shows the blocks it showed, at its new place.
    val moved = picture.moveBy(100, 0).toBitmap()
    assertEquals((moved.pixels().width, moved.pixels().height), (35, 28))
    assertEquals(moved.pixels().colors, picture.toBitmap().pixels().colors)
  }

  test("merging two pictures' bitmaps merges their pixels") {
    val one    = Rectangle(4, 4, Pos.Origin, fillWith(red), None)
    val other  = Rectangle(4, 4, Pos.Origin, fillWith(blue), None)
    val merged = one
      .toBitmap()
      .mergeWith(other.toBitmap(), (a, b) => Color(a.red, 0, b.blue), RenderSettings.Default)

    assertEquals((merged.pixels().width, merged.pixels().height), (4, 4))
    assert(merged.pixels().colors.forall(_ == Color(255, 0, 255)), merged.pixels().colors)
  }
