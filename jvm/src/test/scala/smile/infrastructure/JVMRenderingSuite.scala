package smile.infrastructure

import smile.RenderSettings
import smile.colors.{Color, PresetColor}
import smile.modeling.Pos
import smile.pictures.*
import smile.testing.{DeterministicRendering, NearestNeighborTransforms}

import scala.jdk.CollectionConverters.SetHasAsScala

/** Antialiasing settings, exact translucent channel values and asynchronous image loading, which
  * need the JVM renderer. A canvas stores premultiplied alpha, so low-alpha channel values are
  * approximate on JS. Cross-platform rendering lives in the shared `PictureRenderingSuite`.
  */
class JVMRenderingSuite extends munit.FunSuite with smile.testing.InkAssertions:

  private val red         = Color(255, 0, 0)
  private val green       = Color(0, 255, 0)
  private val blue        = Color(0, 0, 255)
  private val translucent = Color(12, 34, 56, 78)

  /** The name `JVMResourceFactory` gives its loader threads. */
  private val LoaderThreadName = "smile-image-loader"

  test("an explicit RenderSettings switches the renderer's antialiasing") {
    val circle = Circle(Pos.Origin, 20, fillWith(red), line = None)

    def hasSoftEdges(bitmap: Bitmap): Boolean =
      bitmap.pixels().colors.exists(color => color.opacity > 0 && color.opacity < 255)

    // The settings travel from here through the renderer and the drawing surface into the
    // graphics context's rendering hints.
    val smoothed = circle.toBitmap(RenderSettings(drawingIsAntiAliased = true))
    val aliased  = circle.toBitmap(RenderSettings(drawingIsAntiAliased = false))

    assert(hasSoftEdges(smoothed), "an antialiased circle should have partly transparent edges")
    assert(!hasSoftEdges(aliased), "an aliased circle should have hard edges only")
  }

  test("translucent channel values round-trip exactly through the JVM buffer") {
    val bitmap = Bitmap(2, 1, Seq(red, translucent))

    assertEquals(bitmap.getColor(1, 0), translucent)
    assertEquals(bitmap.pixels().colors, Seq(red, translucent))
    // The bulk ARGB path preserves them too.
    assertEquals(
      bitmap.transformColorToColor(identity, RenderSettings.Default).pixels().colors,
      Seq(red, translucent)
    )
  }

  test("bitmap flips preserve dimensions and exact pixels, whatever the transform method") {
    val bitmap = Bitmap(2, 2, Seq(red, green, blue, translucent))

    for settings <- Seq(RenderSettings.Default, NearestNeighborTransforms) do
      assertEquals(
        bitmap.flipHorizontally.pixels(settings).colors,
        Seq(green, red, translucent, blue)
      )
      assertEquals(
        bitmap.flipVertically.pixels(settings).colors,
        Seq(blue, translucent, red, green)
      )
      assertEquals(
        bitmap.flipDiagonally.pixels(settings).colors,
        Seq(translucent, blue, green, red)
      )
  }

  test("rotating a Bitmap zero degrees is a geometry and pixel identity") {
    val bitmap  = Bitmap(2, 2, Seq(red, green, blue, translucent)).moveBy(5, 7)
    val rotated = bitmap.rotateAround(0, Pos.Origin)

    assertEquals(rotated, bitmap)
    assertEquals(rotated.pixels(NearestNeighborTransforms).colors, bitmap.pixels().colors)
  }

  test("a partial arc renders in a tight canvas (#13)") {
    val quarterArc = Arc(
      pos = Pos.Origin,
      width = 100,
      height = 80,
      startAngle = 0,
      arcAngle = 90,
      fill = fillWith(red),
      line = None
    )

    val bitmap = Bitmap(quarterArc, DeterministicRendering)

    // Tight bounds. The full ellipse would be 100x80.
    assertEquals(bitmap.pixels().width, 50)
    assertEquals(bitmap.pixels().height, 40)
    // A point inside the chord-closed fill region is painted.
    assertEquals(bitmap.getColor(35, 15), red)
  }

  test("uniform scaling scales the stroke width (#10)") {
    val original = Rectangle(
      10,
      4,
      Pos.Origin,
      fill = None,
      line = lineWith(PresetColor.Black, width = 3, round = false)
    )
    val expected = Rectangle(
      20,
      8,
      Pos.Origin,
      fill = None,
      line = lineWith(PresetColor.Black, width = 6, round = false)
    )

    assertEquals(
      Bitmap(original.scaleBy(2, 2), DeterministicRendering).pixels().colors,
      Bitmap(expected, DeterministicRendering).pixels().colors
    )
  }

  test("Bitmap.load loads an image asynchronously") {
    val path = java.nio.file.Files.createTempFile("smile-async-", ".png")
    assert(Bitmap(2, 1, Seq(red, blue)).saveAsPngTo(path.toString))

    given scala.concurrent.ExecutionContext = scala.concurrent.ExecutionContext.global

    Bitmap
      .load(path.toString)
      .map: loaded =>
        assertEquals(loaded.pixels().width, 2)
        assertEquals(loaded.getColor(0, 0), red)
        assertEquals(loaded.getColor(1, 0), blue)
      .andThen(_ => java.nio.file.Files.deleteIfExists(path))
  }

  test("the asynchronous loaders run on a bounded pool of daemon threads") {
    // Running the loaders on the global pool, each wrapped in scala.concurrent.blocking, lets that
    // pool grow past its core size to availableProcessors + 256 threads. Decoding is CPU work, so
    // a burst of loads would oversubscribe the CPU and hold that many partly decoded images at
    // once. The loaders' own pool must keep its threads daemons, or a program that loaded a single
    // image would never exit.
    val path = java.nio.file.Files.createTempFile("smile-pool-", ".png")
    assert(Bitmap(2, 1, Seq(red, blue)).saveAsPngTo(path.toString))

    given scala.concurrent.ExecutionContext = scala.concurrent.ExecutionContext.global

    val bound = math.max(2, Runtime.getRuntime.availableProcessors)

    scala.concurrent.Future
      .sequence(Seq.fill(8 * bound)(Bitmap.load(path.toString)))
      .map: loaded =>
        assertEquals(loaded.size, 8 * bound)

        val loaderThreads = Thread.getAllStackTraces.keySet.asScala
          .filter(_.getName == LoaderThreadName)
          .toSeq

        assert(loaderThreads.nonEmpty, s"no $LoaderThreadName thread ran the loads")
        assert(
          loaderThreads.sizeIs <= bound,
          s"the loader pool grew to ${loaderThreads.size} threads, past its bound of $bound"
        )
        assert(
          loaderThreads.forall(_.isDaemon),
          "loader threads must be daemons so a program that loaded an image still exits"
        )
      .andThen(_ => java.nio.file.Files.deleteIfExists(path))
  }
