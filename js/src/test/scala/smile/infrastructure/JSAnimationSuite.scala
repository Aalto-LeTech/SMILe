package smile.infrastructure

import scala.scalajs.js
import smile.colors.Color
import smile.modeling.Pos
import smile.pictures.{AnimatedPicture, FillStyle, Rectangle}

/** The parts of the JS animation loading path that need no image decoding at all. They turn a
  * decoder's frame timestamps into per-frame delays and decide which MIME type the decoder is told
  * to expect. Both run under jsdom, where WebCodecs itself does not exist.
  *
  * The decoding itself is exercised in `BrowserImageLoadingSuite`.
  */
class JSAnimationSuite extends munit.FunSuite:

  test("frame delays are the gaps between successive frame timestamps") {
    // Frames starting at 0 ms, 100 ms and 300 ms, timestamped in microseconds, leave 100 ms and
    // 200 ms gaps. The last frame has no successor to measure against, so it repeats the previous
    // delay.
    assertEquals(
      JSResourceFactory.frameDelaysFrom(Seq(0.0, 100000.0, 300000.0)),
      Seq(100, 200, 200)
    )
  }

  test("a single frame falls back to the default delay") {
    assertEquals(JSResourceFactory.frameDelaysFrom(Seq(0.0)), Seq(100))
    assertEquals(JSResourceFactory.frameDelaysFrom(Seq.empty[Double]), Seq.empty[Int])
  }

  test("frames without usable timestamps fall back instead of yielding a zero-length cycle") {
    // A decoder that leaves every timestamp at zero would otherwise produce delays of 0 ms, and an
    // AnimatedPicture with a zero cycle length freezes on its first frame.
    assertEquals(JSResourceFactory.frameDelaysFrom(Seq(0.0, 0.0, 0.0)), Seq(100, 100, 100))

    // A timestamp that goes backwards keeps the previous frame's delay too.
    assertEquals(JSResourceFactory.frameDelaysFrom(Seq(0.0, 50000.0, 10000.0)), Seq(50, 50, 50))
  }

  test("the decoder is told the MIME type the response declares") {
    assertEquals(JSResourceFactory.mimeTypeFrom("image/gif", "unknown"), "image/gif")
    assertEquals(JSResourceFactory.mimeTypeFrom("image/PNG; charset=binary", "x"), "image/png")
  }

  test("a MIME type that names no image type falls back to the path's extension") {
    assertEquals(JSResourceFactory.mimeTypeFrom("application/octet-stream", "a.png"), "image/png")
    assertEquals(JSResourceFactory.mimeTypeFrom(null, "photos/Bird.GIF"), "image/gif")
    assertEquals(JSResourceFactory.mimeTypeForPath("a.jpeg"), "image/jpeg")
    assertEquals(JSResourceFactory.mimeTypeForPath("a.webp?v=2"), "image/webp")

    // Animations are what this path exists for, so an unrecognizable path is decoded as a GIF.
    assertEquals(JSResourceFactory.mimeTypeForPath("no-extension"), "image/gif")
  }

  test("WebCodecs support is feature-detected without throwing") {
    // The probe has to ask with the typeof operator, because reading an undeclared global any
    // other way throws a ReferenceError. jsdom, where this assertion runs, declares no
    // ImageDecoder at all.
    val detected = WebCodecs.hasImageDecoder
    assertEquals(detected, js.typeOf(js.Dynamic.global.ImageDecoder) != "undefined")
  }

  test("an SVG draws the frame at the given instant") {
    val red   = Rectangle(10, 10, Pos(5, 5), Some(FillStyle(Color(255, 0, 0))), None)
    val blue  = Rectangle(10, 10, Pos(5, 5), Some(FillStyle(Color(0, 0, 255))), None)
    val anim  = AnimatedPicture(Seq(red, blue), Seq(10, 20), startTime = 0)
    val early = JSRenderer.svgString(anim, timeMillis = 5)

    assertNotEquals(early, JSRenderer.svgString(anim, timeMillis = 15))
    // Shapes take no ids, so the markup of a frame is that of the frame on its own.
    assertEquals(early, JSRenderer.svgString(red))
    assertEquals(JSRenderer.svgString(anim, timeMillis = 15), JSRenderer.svgString(blue))
  }
