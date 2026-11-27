package smile.infrastructure

import smile.colors.Color
import smile.pictures.*
import smile.testing.BrowserCapabilities

import scala.concurrent.ExecutionContext

/** Loading images where a browser engine decodes them. Covers `Bitmap.load`, its cache, failures,
  * and GIF frames through WebCodecs.
  *
  * Each test starts from a [[smile.testing.BrowserCapabilities]] probe and `assume`s the capability
  * it needs, so the default jsdom run reports the cases it cannot support as skipped. Run them for
  * real with `sbt -Dsmile.browser=true smileJS/test`.
  */
class BrowserImageLoadingSuite extends munit.FunSuite:

  private given ExecutionContext = ExecutionContext.global

  private val red   = Color(255, 0, 0)
  private val green = Color(0, 255, 0)
  private val blue  = Color(0, 0, 255)
  private val white = Color(255, 255, 255)

  /** A 2x2 opaque PNG with red and green on the top row and blue and white on the bottom one.
    * Inlined as a data URL because a browser loads those through `HTMLImageElement` with no server,
    * no filesystem access and no cross-origin rules involved.
    */
  private val CheckerboardPNG =
    "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAIAAAACCAIAAAD91JpzAAAAEklEQVR42mP4z8DAAMIM/4EA" +
      "AB/uBfvxq7p3AAAAAElFTkSuQmCC"

  test("Bitmap.load decodes an image the browser fetches") {
    BrowserCapabilities.decodesImages.flatMap: canDecode =>
      assume(canDecode, "this JS environment does not decode images")

      Bitmap
        .load(CheckerboardPNG)
        .map: bitmap =>
          assertEquals(bitmap.pixels().width, 2)
          assertEquals(bitmap.pixels().height, 2)
          assertEquals(bitmap.bounds.get.width, 2.0)
          assertEquals(bitmap.bounds.get.height, 2.0)

          assertEquals(bitmap.getColor(0, 0), red)
          assertEquals(bitmap.getColor(1, 0), green)
          assertEquals(bitmap.getColor(0, 1), blue)
          assertEquals(bitmap.getColor(1, 1), white)
  }

  test("a loaded image is cached, so the synchronous API serves it afterwards") {
    BrowserCapabilities.decodesImages.flatMap: canDecode =>
      assume(canDecode, "this JS environment does not decode images")

      Bitmap
        .load(CheckerboardPNG)
        .map: loaded =>
          val fromCache = Bitmap(CheckerboardPNG)
          assertEquals(fromCache.pixels().width, loaded.pixels().width)
          assertEquals(fromCache.getColor(1, 1), white)
  }

  /** A 2x2 GIF whose three frames are all red, all green and all blue, with stored delays of 10, 20
    * and 10 centiseconds. Hand-built at 107 bytes so that it can be inlined as a data URL, which a
    * browser fetches without a server or any file access.
    */
  private val ThreeFrameGIF =
    "data:image/gif;base64,R0lGODlhAgACAJEAAP8AAAD/AAAA/////yH5BAAKAAAALAAAAAACAAIAAAcGgAAAAACBACH5" +
      "BAAUAAAALAAAAAACAAIAAAcGgAEBAQGBACH5BAAKAAAALAAAAAACAAIAAAcGgAICAgKBADs="

  test("AnimatedPicture.load extracts every GIF frame where WebCodecs is available") {
    assume(BrowserCapabilities.extractsImageFrames, "this JS environment has no ImageDecoder")

    AnimatedPicture
      .load(ThreeFrameGIF)
      .map: animation =>
        assertEquals(animation.frames.size, 3)

        val frameColors = animation.frames.map(_.asInstanceOf[Bitmap].getColor(1, 1))
        assertEquals(frameColors, Seq(red, green, blue))

        // Delays come from the gaps between the frames' presentation timestamps, so the first two
        // are the GIF's own 100 ms and 200 ms. The last frame has no successor to measure against
        // and repeats the previous delay.
        assertEquals(animation.frameDelays, Seq(100, 200, 200))

        // The delays drive the clock-based frame selection, so the second frame is on display
        // 150 ms in and the third one 350 ms in.
        assert(animation.frameAt(animation.startTime + 150) eq animation.frames(1))
        assert(animation.frameAt(animation.startTime + 350) eq animation.frames(2))

        assertEquals(animation.bounds.get.width, 2.0)
  }

  test("a still image loads as a one-frame animation on any JS environment") {
    BrowserCapabilities.decodesImages.flatMap: canDecode =>
      assume(canDecode, "this JS environment does not decode images")

      // Whether this goes through ImageDecoder or the single-frame fallback, a PNG has exactly one
      // frame, and a lone frame gets the default delay because nothing times it.
      AnimatedPicture
        .load(CheckerboardPNG)
        .map: animation =>
          assertEquals(animation.frames.size, 1)
          assertEquals(animation.frameDelays, Seq(100))
          assertEquals(animation.frames.head.asInstanceOf[Bitmap].getColor(0, 0), red)
  }

  test("Bitmap.load fails for data a browser cannot decode") {
    // Runs only in a browser engine. jsdom reports a failed image load as a jsdomError, and the
    // jsdom JS environment kills the whole test process on those.
    assume(BrowserCapabilities.laysOutSVGText, "this JS environment is not a full browser engine")

    Bitmap
      .load("data:image/png;base64,bm90LWEtcG5n")
      .failed
      .map: error =>
        assert(error.isInstanceOf[java.io.IOException], s"expected an IOException, got $error")
  }
