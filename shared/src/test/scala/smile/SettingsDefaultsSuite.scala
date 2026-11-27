package smile

import smile.modeling.*
import smile.pictures.*
import smile.testing.Cases

/** Construction defaults are plain default arguments. Render settings belong to outputs, are passed
  * to them explicitly, and are never found implicitly.
  */
class SettingsDefaultsSuite extends munit.FunSuite:

  private val fill = fillWith(Cases.red)

  test("shapes with no position are centered at the origin") {
    assertEquals(Rectangle(10, 5, fill, None).anchor, Pos.Origin)
    assertEquals(Triangle(10, fill, None).anchor, Pos.Origin)
    assertEquals(Arc(width = 10, height = 6, fill = fill, line = None).anchor, Pos.Origin)
    assertEquals(
      Arc(width = 10, height = 6, fill = fill, line = None).bounds,
      Some(Bounds(-5, -3, 5, 3))
    )
  }

  test("the library's own defaults are the documented ones") {
    assertEquals(
      RenderSettings.Default,
      RenderSettings(
        drawingIsAntiAliased = true,
        normalizesStrokes = false,
        bufferScalingMethod = ScalingMethod.Smooth,
        bufferTransformMethod = TransformMethod.Bilinear,
        resamplingMode = ResamplingMode.Direct
      )
    )
    assertEquals(RenderSettings.Default.resamplingMode, ResamplingMode.Direct)
  }

  test("RenderSettings is never found implicitly") {
    assert(compileErrors("summon[smile.RenderSettings]").nonEmpty)
  }

  test("the default settings antialias a circle's edge") {
    // Turning antialiasing off is pinned in the JVM suite, because only the JVM renderer reads it.
    val circle = Circle(Pos.Origin, 10, fill, None)
    val edge   = circle.toBitmap(RenderSettings.Default).pixels().colors
    assert(edge.exists(c => c.opacity > 0 && c.opacity < 255))
  }

  test("bufferScalingMethod chooses between resampling and repeating a bitmap's pixels") {
    val nearest = RenderSettings(
      bufferScalingMethod = ScalingMethod.NearestNeighbor,
      resamplingMode = ResamplingMode.Stepwise
    )
    val bitmap   = Cases.twoColorBitmap
    val enlarged = bitmap.scaleTo(20, 10) match
      case scaled: Bitmap => scaled.pixels(nearest)
      case other          => fail(s"a bitmap scales into a bitmap, not $other")
    assertEquals((enlarged.width, enlarged.height), (20, 10))
    // Nearest neighbor invents no colors.
    assertEquals(enlarged.colors.toSet, Set(Cases.red, Cases.blue))
    assertEquals(enlarged.colors.count(_ == Cases.red), 100)
  }

  test("an output hands its settings on to the bitmaps it scales and rotates") {
    val exact = RenderSettings(
      bufferScalingMethod = ScalingMethod.NearestNeighbor,
      bufferTransformMethod = TransformMethod.Nearest,
      resamplingMode = ResamplingMode.Stepwise
    )
    val smooth = RenderSettings(resamplingMode = ResamplingMode.Stepwise)
    def colorsOf(picture: Element, settings: RenderSettings): Set[smile.colors.Color] =
      smile.infrastructure
        .Renderer()
        .createBitmapFrom(picture, 1.0, settings)
        .pixels()
        .colors
        .toSet
    val picture = Picture(Cases.twoColorBitmap)

    // Only opaque pixels are compared. A canvas antialiases a turned image's edges either way.
    def opaqueColorsOf(picture: Element, settings: RenderSettings) =
      colorsOf(picture, settings).filter(_.opacity == 255)

    // Smooth resampling, the default, blends red into blue. Copying pixels never does.
    val scaled = picture.scaleBy(2.5)
    assert(opaqueColorsOf(scaled, smooth).size > 2)
    assertEquals(opaqueColorsOf(scaled, exact), Set(Cases.red, Cases.blue))
    val turned = picture.rotateAround(30, Pos.Origin)
    assert(opaqueColorsOf(turned, smooth).size > 2)
    assertEquals(opaqueColorsOf(turned, exact), Set(Cases.red, Cases.blue))
  }
