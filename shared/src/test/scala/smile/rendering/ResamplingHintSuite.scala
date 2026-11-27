package smile.rendering

import smile.{RenderSettings, ResamplingMode, ScalingMethod}
import smile.colors.Color
import smile.infrastructure.Renderer
import smile.pictures.*

/** A bitmap's resampling hint decides how a `Direct` output resamples it, and a `Stepwise` output
  * ignores it.
  */
class ResamplingHintSuite extends munit.FunSuite:

  private val direct   = RenderSettings()
  private val stepwise = RenderSettings(resamplingMode = ResamplingMode.Stepwise)

  private val red    = Color(255, 0, 0).toARGBInt
  private val green  = Color(0, 255, 0).toARGBInt
  private val blue   = Color(0, 0, 255).toARGBInt
  private val yellow = Color(255, 255, 0).toARGBInt

  private val twoByTwo: Bitmap =
    Bitmap.generateARGB(
      2,
      2,
      (x, y) =>
        (x, y) match
          case (0, 0) => red
          case (1, 0) => green
          case (0, 1) => blue
          case _      => yellow
    )

  private def colorCount(bitmap: Bitmap, settings: RenderSettings): Int =
    bitmap.pixels(settings).argbData.toSet.size

  test("a bitmap is Auto until it is given a hint, and operations keep the hint") {
    assertEquals(twoByTwo.resampling, ResamplingHint.Auto)
    val hinted = twoByTwo.withResampling(ResamplingHint.Pixelated)
    assertEquals(hinted.resampling, ResamplingHint.Pixelated)
    assertEquals(
      hinted.scaleBy(3, 3).asBitmap.rotateAround(10, hinted.anchor).resampling,
      ResamplingHint.Pixelated
    )
  }

  test("Pixelated scales a bitmap to exactly its own colors in Direct mode") {
    val scaled = twoByTwo.scaleBy(4, 4).asBitmap.withResampling(ResamplingHint.Pixelated)
    assertEquals(scaled.pixels(direct).width, 8)
    assertEquals(colorCount(scaled, direct), 4)
  }

  test("Pixelated wins over a smooth scaling method, and Smooth over a nearest one") {
    val smoothSettings  = direct.copy(bufferScalingMethod = ScalingMethod.Smooth)
    val nearestSettings = direct.copy(bufferScalingMethod = ScalingMethod.NearestNeighbor)
    val pixelated       = twoByTwo.scaleBy(4, 4).asBitmap.withResampling(ResamplingHint.Pixelated)
    val smooth          = twoByTwo.scaleBy(4, 4).asBitmap.withResampling(ResamplingHint.Smooth)

    assertEquals(colorCount(pixelated, smoothSettings), 4)
    assert(colorCount(smooth, nearestSettings) > 4)
    assert(colorCount(smooth, direct) > 4)
  }

  test("Auto follows the scaling method of the settings") {
    val auto = twoByTwo.scaleBy(4, 4).asBitmap
    assertEquals(
      colorCount(auto, direct.copy(bufferScalingMethod = ScalingMethod.NearestNeighbor)),
      4
    )
    assert(colorCount(auto, direct.copy(bufferScalingMethod = ScalingMethod.Smooth)) > 4)
  }

  test("a Pixelated bitmap rendered at a larger scale stays crisp") {
    val scaled  = twoByTwo.scaleBy(4, 4).asBitmap.withResampling(ResamplingHint.Pixelated)
    val picture = scaled.crop(scaled.bounds.get)
    val drawn   = Renderer().createBitmapFrom(picture, 2.0, direct)
    assertEquals(drawn.pixels(direct).width, 16)
    assertEquals(colorCount(drawn, direct), 4)
  }

  test("Stepwise pixels are the same for every hint") {
    for method <- Seq(ScalingMethod.NearestNeighbor, ScalingMethod.Smooth) do
      val settings  = stepwise.copy(bufferScalingMethod = method)
      val reference =
        twoByTwo.scaleBy(4, 4).asBitmap.rotateAround(20, twoByTwo.anchor).pixels(settings)
      for hint <- ResamplingHint.values do
        val hinted =
          twoByTwo
            .scaleBy(4, 4)
            .asBitmap
            .rotateAround(20, twoByTwo.anchor)
            .withResampling(hint)
            .pixels(settings)
        assertEquals(hinted.argbData.toSeq, reference.argbData.toSeq, s"$method $hint")
  }
end ResamplingHintSuite
