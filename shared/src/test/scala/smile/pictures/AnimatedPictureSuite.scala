package smile.pictures

import smile.RenderSettings
import smile.colors.Color
import smile.infrastructure.{RasterCache, Renderer}
import smile.modeling.Pos

/** `AnimatedPicture` on both platforms: the frame a moment selects (issue #8), bounds that do not
  * jitter between frames, the arguments it refuses, and what equality and transformations keep.
  */
class AnimatedPictureSuite extends munit.FunSuite:

  private val red  = Color(255, 0, 0)
  private val blue = Color(0, 0, 255)

  test("an AnimatedPicture renders the frame the system clock selects (#8)") {
    val oneHour = 60 * 60 * 1000
    // On the whole pixel from the origin, so that the bitmap of the animation is that pixel.
    val frames = Seq(Bitmap(1, 1, Seq(red)), Bitmap(1, 1, Seq(blue))).map(_.moveBy(0.5, 0.5))
    val now    = System.currentTimeMillis()

    val justStarted = AnimatedPicture(frames, Seq(oneHour, oneHour), startTime = now)
    assertEquals(justStarted.toBitmap().getColor(0, 0), red)

    val oneDelayIn = AnimatedPicture(frames, Seq(oneHour, oneHour), startTime = now - oneHour)
    assertEquals(oneDelayIn.toBitmap().getColor(0, 0), blue)
  }

  test("frameAt walks the elapsed time through the delays and wraps around the cycle") {
    val first  = Bitmap(1, 1, Seq(red))
    val second = Bitmap(1, 1, Seq(blue))
    // Cycle: frame 0 for [0, 10), frame 1 for [10, 30), then repeat.
    val animation = AnimatedPicture.ofFrames(Seq(first, second), Seq(10, 20), startTime = 0)

    assertEquals(animation.cycleLength, 30L)
    assertEquals(animation.frameAt(0), first)
    assertEquals(animation.frameAt(5), first)
    assertEquals(animation.frameAt(9), first)
    assertEquals(animation.frameAt(10), second)
    assertEquals(animation.frameAt(15), second)
    assertEquals(animation.frameAt(29), second)
    assertEquals(animation.frameAt(30), first)
    assertEquals(animation.frameAt(35), first)
    assertEquals(animation.frameAt(45), second)
    // Instants before the start time wrap backwards into the cycle.
    assertEquals(animation.frameAt(-1), second)
    assertEquals(animation.frameAt(-30), first)
  }

  test("frameAt shows the first frame when the cycle has no length") {
    val first     = Bitmap(1, 1, Seq(red))
    val second    = Bitmap(1, 1, Seq(blue))
    val animation = AnimatedPicture.ofFrames(Seq(first, second), Seq(0, 0), startTime = 0)

    assertEquals(animation.cycleLength, 0L)
    assertEquals(animation.frameAt(0), first)
    assertEquals(animation.frameAt(1_000_000), first)
  }

  test("AnimatedPicture bounds are the union of all frames, so layout does not jitter") {
    val small     = Bitmap(1, 1, Seq(red))
    val wide      = Bitmap(20, 4, Seq.fill(80)(blue))
    val animation =
      AnimatedPicture.ofFrames(Seq(small, wide), Seq(10, 10), startTime = 0)

    assertEquals(animation.width, wide.width)
    assertEquals(animation.height, wide.height)
    // The same bounds regardless of which frame is on display.
    assertEquals(animation.frameAt(0), small)
    assertEquals(animation.frameAt(15), wide)
  }

  test("moving an AnimatedPicture preserves its layout and moves its anchor") {
    val base      = Bitmap(1, 1, Seq(red)).moveBy(20, 0)
    val offset    = Bitmap(1, 1, Seq(blue)).moveBy(30, 0)
    val animation = AnimatedPicture.ofFrames(Seq(base, offset), Seq(100, 100))

    val copied = animation.moveTo(Pos(100, 200))

    // The anchor is the first frame's.
    assertEquals(copied.anchor, Pos(100, 200))
    assertEquals(
      copied.frameAt(animation.startTime + 150).anchor - copied.frameAt(animation.startTime).anchor,
      offset.anchor - base.anchor
    )
  }

  test("AnimatedPicture rejects an empty frame sequence") {
    intercept[IllegalArgumentException]:
      AnimatedPicture.ofFrames(Seq.empty, Seq.empty)
  }

  test("AnimatedPicture requires one delay per frame") {
    val frame = Bitmap(1, 1, Seq(red))

    intercept[IllegalArgumentException]:
      AnimatedPicture.ofFrames(Seq(frame, frame), Seq(100))
  }

  test("AnimatedPicture rejects negative frame delays") {
    val frame = Bitmap(1, 1, Seq(red))

    intercept[IllegalArgumentException]:
      AnimatedPicture.ofFrames(Seq(frame, frame), Seq(100, -1))
  }

  test("AnimatedPicture equality includes the start time") {
    val frames = Seq(Bitmap(1, 1, Seq(red)), Bitmap(1, 1, Seq(blue)))
    val delays = Seq(10, 20)

    val early = AnimatedPicture(frames, delays, startTime = 0)
    val late  = AnimatedPicture(frames, delays, startTime = 1_000)

    // Animations that start at other instants draw other frames at the same instant.
    assertNotEquals(early, late)
    assertEquals(early, AnimatedPicture(frames, delays, startTime = 0))
    assertEquals(early.hashCode, AnimatedPicture(frames, delays, startTime = 0).hashCode)
    assertNotEquals(early, AnimatedPicture(frames, Seq(10, 30), startTime = 0))
  }

  /** Red for 10 ms and then blue for 20 ms from the instant 0, on the whole pixel from the origin,
    * so that the bitmap of the animation is that pixel.
    */
  private val redThenBlue = AnimatedPicture(
    Seq(Bitmap(1, 1, Seq(red)), Bitmap(1, 1, Seq(blue))).map(_.moveBy(0.5, 0.5)),
    Seq(10, 20),
    startTime = 0
  )

  test("rendering draws the frame at the given instant") {
    def colorAt(timeMillis: Long): Color =
      Renderer()
        .createBitmapFrom(redThenBlue, 1.0, RenderSettings.Default, timeMillis)
        .getColor(0, 0)

    assertEquals(colorAt(timeMillis = 5), red)
    assertEquals(colorAt(timeMillis = 15), blue)
    assertEquals(colorAt(timeMillis = 35), red)
  }

  test("a bitmap of a picture drawn in a frame shows the animations in it at the frame's instant") {
    // A picture source rendered as part of a frame is rendered at the frame's instant, however
    // deep it lies.
    def nested(timeMillis: Long): Color =
      val outer = Rasters.of(Picture(Rasters.of(Picture(redThenBlue))))
      Renderer().createBitmapFrom(outer, 1.0, RenderSettings.Default, timeMillis).getColor(0, 0)

    assertEquals(nested(timeMillis = 5), red)
    assertEquals(nested(timeMillis = 15), blue)
  }

  test("a bitmap of a picture shows the animations in it at the instant of its first rendering") {
    // Every rendering of one source, at any density and also after the cache let it go, is
    // rendered at that instant, so the frame never depends on what was kept.
    val bitmap                                 = Rasters.of(Picture(redThenBlue))
    def drawn(timeMillis: Long, scale: Double) =
      Renderer().createBitmapFrom(bitmap, scale, RenderSettings.Default, timeMillis).getColor(0, 0)

    assertEquals(drawn(timeMillis = 5, scale = 1), red)
    assertEquals(drawn(timeMillis = 15, scale = 2), red, "another density shows the same frame")
    RasterCache.shared.withBudget(0)(())
    assertEquals(drawn(timeMillis = 15, scale = 1), red, "the frame after the cache was emptied")
    val uncached = RasterCache.shared.withBudget(0)(drawn(timeMillis = 15, scale = 3))
    assertEquals(uncached, red, "the frame with nothing kept")
  }

  test("AnimatedPicture transformations keep the start time and the frame timeline") {
    val first     = Bitmap(1, 1, Seq(red))
    val second    = Bitmap(1, 1, Seq(blue))
    val animation =
      AnimatedPicture
        .ofFrames(Seq(first, second), Seq(10, 20), startTime = 0)
        .scaleBy(2.0, 2.0) match
        case scaled: AnimatedPicture => scaled
        case other                   => fail(s"an animation scales into one, not $other")

    assertEquals(animation.startTime, 0L)
    assertEquals(animation.frameAt(5).bounds, first.scaleAround(2, 2, first.anchor).bounds)
    assertEquals(animation.frameAt(15).bounds, second.scaleAround(2, 2, first.anchor).bounds)
  }

  test(
    "animated pictures can follow a caller's elapsed time independent of their wall-clock start"
  ) {
    val a         = Circle(Pos(0, 0), 1, fillWith(Color(0, 0, 0)), None)
    val b         = Circle(Pos(10, 0), 1, fillWith(Color(0, 0, 0)), None)
    val animation = AnimatedPicture.ofFrames(Seq(a, b), Seq(100, 200), startTime = 123456L)
    assertEquals(animation.frameAtElapsed(0), a)
    assertEquals(animation.frameAtElapsed(0.0999), a)
    assertEquals(animation.frameAtElapsed(0.15), b)
    assertEquals(animation.frameAtElapsed(0.3), a)
    assertEquals(animation.frameAtElapsed(-0.05), b)
  }
