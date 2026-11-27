package smile.pictures

import smile.RenderSettings
import smile.colors.{Color, PresetColor}
import smile.infrastructure.BufferAdapter
import smile.modeling.{Bounds, Pos}

/** A bitmap's pixels and the buffer behind them, on both platforms: row-major access, independent
  * copies, cropping, the Color- and ARGB-based pixel operations, and merging.
  */
class BitmapPixelsSuite extends munit.FunSuite:

  private val red    = Color(255, 0, 0)
  private val green  = Color(0, 255, 0)
  private val blue   = Color(0, 0, 255)
  private val walnut = Color(12, 34, 56)

  test("Bitmap applies colors in row-major order and checks coordinates") {
    val bitmap = Bitmap(2, 2, Seq(red, green, blue, walnut))

    assertEquals(bitmap.getColor(0, 0), red)
    assertEquals(bitmap.getColor(1, 0), green)
    assertEquals(bitmap.getColor(0, 1), blue)
    assertEquals(bitmap.getColor(1, 1), walnut)
    assertEquals(bitmap.colorAt(-1, 0), None)
    assertEquals(bitmap.colorAt(2, 0), None)
    assertEquals(bitmap.colorAt(0, 2), None)
  }

  test("Bitmap rejects a color count that does not match its dimensions") {
    intercept[IllegalArgumentException]:
      Bitmap(2, 2, Seq(red, green, blue))
  }

  test("setColorsByLocation receives row-major coordinates") {
    val bitmap = Bitmap(3, 2, (x, y) => Color(x * 40, y * 80, 0))

    assertEquals(
      bitmap.pixels().colors,
      Seq(
        Color(0, 0, 0),
        Color(40, 0, 0),
        Color(80, 0, 0),
        Color(0, 80, 0),
        Color(40, 80, 0),
        Color(80, 80, 0)
      )
    )
  }

  test("Bitmap.crop accepts corners in either order") {
    val bitmap = Bitmap(
      3,
      2,
      Seq(red, green, blue, walnut, PresetColor.Black, PresetColor.White)
    )

    // In world coordinates the bitmap's bounds are (-1.5,-1)-(1.5,1).
    val cropped = bitmap.crop(Bounds(1.5, 1, -0.5, -1)).asBitmap

    assertEquals(cropped.pixels().width, 2)
    assertEquals(cropped.pixels().height, 2)
    assertEquals(
      cropped.pixels().colors,
      Seq(green, blue, PresetColor.Black, PresetColor.White)
    )
  }

  test("cropping copies pixel storage instead of aliasing the source") {
    val bitmap = Bitmap(2, 1, Seq(red, green))

    // In world coordinates the bitmap's bounds are (-1,-0.5)-(1,0.5).
    val cropped = bitmap.crop(Bounds(-1, -0.5, 0, 0.5)).asBitmap

    cropped.nativeBuffer().setRGBA(0, 0, blue)

    assertEquals(bitmap.getColor(0, 0), red)
    assertEquals(cropped.getColor(0, 0), blue)
  }

  test("Bitmap.crop past the source's pixels has the requested bounds and shows only its pixels") {
    val bitmap = Bitmap(2, 2, Seq.fill(4)(red))

    // In world coordinates the bitmap's bounds are (-1,-1)-(1,1).
    assertEquals(bitmap.crop(Bounds(-2, 0, 1, 1)).bounds, Some(Bounds(-2, 0, 1, 1)))
    assertEquals(bitmap.crop(Bounds(0, 0, 3, 1)).bounds, Some(Bounds(0, 0, 3, 1)))
    assertEquals(
      bitmap.crop(Bounds(-2, 0, 1, 1)).toBitmap().pixels().colors.map(_.opacity),
      Seq(0, 255, 255)
    )
    // One that holds none of its pixels is transparent space of the size asked for.
    assertEquals(bitmap.crop(Bounds(3, 0, 5, 1)).bounds, Some(Bounds(3, 0, 5, 1)))
    assertEquals(
      bitmap.crop(Bounds(3, 0, 5, 1)).toBitmap().pixels().colors.map(_.opacity).toSet,
      Set(0)
    )
  }

  test("transformColorToColor maps every pixel") {
    val bitmap = Bitmap(2, 1, Seq(red, walnut))

    val inverted =
      bitmap.transformColorToColor(
        c => Color(255 - c.red, 255 - c.green, 255 - c.blue, c.opacity),
        RenderSettings.Default
      )

    assertEquals(
      inverted.pixels().colors,
      Seq(Color(0, 255, 255), Color(243, 221, 199))
    )
  }

  test("ImageOperations.copyPortionXYXY accepts reversed corners") {
    val buffer = BufferAdapter(3, 1)
    buffer.setColorsFromSeq(Seq(red, green, blue))

    val copied = buffer.copyPortionXYXY(3, 1, 1, 0)

    assertEquals(copied.imageData, Seq(green, blue))
  }

  test("mergeWith uses each source buffer's row stride") {
    val wider  = Bitmap(3, 2, Seq(red, green, blue, walnut, PresetColor.Black, red))
    val narrow = Bitmap(2, 2, Seq.fill(4)(PresetColor.White))

    val merged = wider.mergeWith(narrow, (left, _) => left, RenderSettings.Default)

    assertEquals(merged.pixels().colors, Seq(red, green, walnut, PresetColor.Black))
  }

  test("mergeWith preserves the bitmap's position") {
    val a = Bitmap(2, 2, Seq.fill(4)(red)).moveBy(500, 500)
    val b = Bitmap(2, 2, Seq.fill(4)(blue)).moveBy(500, 500)

    assertEquals(a.mergeWith(b, (left, _) => left, RenderSettings.Default).bounds.get, a.bounds.get)

    // A smaller other bitmap truncates the merge, and the result stays anchored at
    // this bitmap's upper left corner.
    val smaller = Bitmap(1, 1, Seq(blue)).moveBy(500, 500)
    assertEquals(
      a.mergeWith(smaller, (left, _) => left, RenderSettings.Default).bounds.get,
      Bounds(Pos(499, 499), Pos(500, 500))
    )
  }

  test("transformARGB maps every pixel without boxing a Color") {
    val bitmap = Bitmap(2, 1, Seq(red, walnut))

    // Invert the color channels, leave the alpha channel untouched.
    val inverted = bitmap.transformARGB(_ ^ 0x00ffffff, RenderSettings.Default)

    assertEquals(
      inverted.pixels().colors,
      Seq(Color(0, 255, 255), Color(243, 221, 199))
    )
  }

  test("mergeARGBWith merges packed pixels and uses each source's row stride") {
    val wider  = Bitmap(3, 2, Seq(red, green, blue, walnut, PresetColor.Black, PresetColor.White))
    val narrow = Bitmap(2, 2, Seq(blue, green, red, walnut))

    // Alpha and red from the left pixel, green and blue from the right one.
    val merged = wider.mergeARGBWith(
      narrow,
      (l, r) => (l & 0xffff0000) | (r & 0x0000ffff),
      RenderSettings.Default
    )

    assertEquals(merged.pixels().width, 2)
    assertEquals(merged.pixels().height, 2)
    assertEquals(
      merged.pixels().colors,
      Seq(Color(255, 0, 255), Color(0, 255, 0), Color(12, 0, 0), Color(0, 34, 56))
    )
  }

  test("mergeARGBWith anchors the result like mergeWith") {
    val a       = Bitmap(2, 2, Seq.fill(4)(red)).moveBy(500, 500)
    val smaller = Bitmap(1, 1, Seq(blue)).moveBy(500, 500)

    assertEquals(
      a.mergeARGBWith(smaller, (left, _) => left, RenderSettings.Default).bounds.get,
      Bounds(Pos(499, 499), Pos(500, 500))
    )
  }

  test("the Color-based and ARGB-based pixel operations agree") {
    val a = Bitmap(3, 2, Seq(red, green, blue, walnut, PresetColor.Black, PresetColor.White))
    val b = Bitmap(2, 2, Seq(blue, walnut, PresetColor.White, green))

    def average(x: Int, y: Int): Int = (x + y) / 2

    val mergedAsColors = a.mergeWith(
      b,
      (l, r) =>
        Color(
          average(l.red, r.red),
          average(l.green, r.green),
          average(l.blue, r.blue),
          average(l.opacity, r.opacity)
        ),
      RenderSettings.Default
    )
    val mergedAsARGB = a.mergeARGBWith(
      b,
      (l, r) =>
        val channel = (shift: Int) => average((l >> shift) & 0xff, (r >> shift) & 0xff) << shift
        channel(24) | channel(16) | channel(8) | channel(0)
      ,
      RenderSettings.Default
    )

    assertEquals(mergedAsARGB.pixels().colors, mergedAsColors.pixels().colors)
    assertEquals(mergedAsARGB.bounds.get, mergedAsColors.bounds.get)

    val transformedAsColors =
      a.transformColorToColor(c => Color(c.blue, c.red, c.green, c.opacity), RenderSettings.Default)
    val transformedAsARGB =
      a.transformARGB(
        argb =>
          (argb & 0xff000000) | ((argb & 0x000000ff) << 16) | ((argb & 0x00ff0000) >> 8) |
            ((argb & 0x0000ff00) >> 8),
        RenderSettings.Default
      )

    assertEquals(transformedAsARGB.pixels().colors, transformedAsColors.pixels().colors)
  }

  test("Bitmap.moveTo puts the anchor at an absolute position") {
    val moved  = Bitmap(2, 2, Seq.fill(4)(red)).moveBy(5, 7)
    val copied = moved.moveTo(Pos(20, 30))

    assertEquals(copied.anchor, Pos(20, 30))
  }

  test("copyPortionXYWH treats out-of-bounds regions the same on both platforms") {
    val buffer = smile.infrastructure.BufferAdapter(3, 2)

    val error = intercept[IllegalArgumentException]:
      buffer.copyPortionXYWH(2, 0, 5, 2)

    assert(
      error.getMessage.contains("outside the 3x2 buffer"),
      s"unexpected message: ${error.getMessage}"
    )
  }
