package smile.issues

import smile.{RenderSettings, ResamplingMode}
import smile.colors.{Color, PresetColor}
import smile.infrastructure.{JVMRenderer, JVMResourceFactory}
import smile.modeling.{Bounds, Pos}
import smile.pictures.*
import smile.testing.*

import java.awt.image.BufferedImage
import java.io.IOException
import java.nio.file.{Files, Path}
import javax.imageio.ImageIO
import scala.jdk.CollectionConverters.IteratorHasAsScala
import scala.util.Using

class OpenIssueRegressionSuite extends munit.FunSuite:

  private val red   = Color(255, 0, 0)
  private val green = Color(0, 255, 0)
  private val blue  = Color(0, 0, 255)
  private val white = Color(255, 255, 255)

  private def withTemporaryFile[A](suffix: String)(test: Path => A): A =
    val path = Files.createTempFile("smile-test-", suffix)
    try test(path)
    finally
      val _ = Files.deleteIfExists(path)

  private def deleteRecursively(directory: Path): Unit =
    if Files.exists(directory) then
      val entries = Using.resource(Files.list(directory))(_.iterator.asScala.toVector)
      entries.foreach: entry =>
        if Files.isDirectory(entry) then deleteRecursively(entry)
        else
          val _ = Files.deleteIfExists(entry)
      val _ = Files.deleteIfExists(directory)

  private def withTemporaryDirectory[A](test: Path => A): A =
    val directory = Files.createTempDirectory("smile-test-")
    try test(directory)
    finally deleteRecursively(directory)

  /** Probes the filesystem hosting temporary files. Creates `a.tmp` and checks whether `A.tmp`
    * resolves to it. False on macOS and Windows, true on a typical Linux CI runner.
    */
  private lazy val temporaryFilesystemIsCaseSensitive: Boolean =
    withTemporaryDirectory: directory =>
      val _ = Files.createFile(directory.resolve("a.tmp"))
      !Files.exists(directory.resolve("A.tmp"))

  /** `b` with its corners rounded to a millionth of a pixel. */
  private def rounded(b: Option[Bounds]): Option[Bounds] =
    def r(v: Double) = math.rint(v * 1e6) / 1e6 + 0.0
    b.map(box =>
      Bounds(
        r(box.upperLeftCorner.x),
        r(box.upperLeftCorner.y),
        r(box.lowerRightCorner.x),
        r(box.lowerRightCorner.y)
      )
    )

  /** `e`, which an operation of a bitmap's own gives, as the bitmap it is. */
  private def asBitmap(e: Element): Bitmap = e match
    case bitmap: Bitmap => bitmap
    case other          => fail(s"Expected a bitmap, was $other")

  private def writePng(path: Path): Unit =
    val _ =
      ImageIO.write(new BufferedImage(1, 1, BufferedImage.TYPE_INT_ARGB), "png", path.toFile)

  test("#20 Bitmap.apply reports an IOException for a file it cannot decode") {
    withTemporaryFile(".not-an-image"): path =>
      Files.writeString(path, "This is not an image")

      val error = intercept[IOException]:
        Bitmap(path.toString)

      assert(error.getMessage.contains(path.toString))
  }

  test("#19 a rendered line starts at the requested coordinate") {
    val line = Line(
      Pos(1, 1),
      Pos(6, 1),
      StrokeStyle(PresetColor.Black, width = 1, cap = StrokeStyle.Cap.Butt)
    )
    val picture = line.crop(Bounds(Pos(0, 0), Pos(8, 3)))
    val bitmap  = Bitmap(picture, DeterministicRendering)

    assertEquals(bitmap.getColor(1, 1), PresetColor.Black)
    assertEquals(bitmap.getColor(0, 1).opacity, 0)
  }

  test("#17 moving a picture preserves its layout and moves its anchor") {
    val left     = Rectangle(4, 2, Pos(10, 20), fillWith(red), line = None)
    val right    = Rectangle(4, 2, Pos(30, 40), fillWith(blue), line = None)
    val original = Picture(left, right)

    val copied = original.moveTo(Pos(100, 200))

    assertEquals(copied.anchor, Pos(100, 200))
    assertEquals(copied.bounds, original.bounds.map(_.moveBy(80, 170)))
  }

  test("#16 Bitmap.crop returns the requested pixels and dimensions") {
    val bitmap = Bitmap(
      3,
      2,
      Seq(red, green, blue, white, PresetColor.Black, PresetColor.Transparent)
    )

    // In world coordinates the bitmap's boundary is (-1.5,-1)-(1.5,1).
    val cropped = asBitmap(bitmap.crop(Bounds(-0.5, -1, 1.5, 1)))

    assertEquals(cropped.pixels().width, 2)
    assertEquals(cropped.pixels().height, 2)
    assertEquals(
      cropped.pixels().colors,
      Seq(green, blue, PresetColor.Black, PresetColor.Transparent)
    )
    assertEquals(cropped.bounds, Some(Bounds(Pos(-0.5, -1), Pos(1.5, 1))))
  }

  test("#15 Bitmap.saveAsPngTo writes a readable lossless PNG") {
    withTemporaryFile(".png"): path =>
      val bitmap = Bitmap(2, 1, Seq(red, blue))

      assert(bitmap.saveAsPngTo(path.toString))

      val saved = ImageIO.read(path.toFile)
      assert(saved != null)
      assertEquals(saved.getWidth, 2)
      assertEquals(saved.getHeight, 1)
      assertEquals(saved.getRGB(0, 0), red.toARGBInt)
      assertEquals(saved.getRGB(1, 0), blue.toARGBInt)
  }

  test("#14 scaleBy uses relativityPoint as a fixed point") {
    val rectangle = Rectangle(10, 4, Pos(10, 20), fillWith(red), line = None)

    val scaled = rectangle.scaleAround(2, 2, Pos.Origin)

    assertEquals(scaled.anchor, Pos(20, 40))
    assertEquals(scaled.width, 20.0)
    assertEquals(scaled.height, 8.0)
  }

  test("#11 rotating text stays a vector element with rotated bounds") {
    val text = Text(
      pos = Pos(30, 10),
      customBounds = Some(Bounds(Pos(0, 0), Pos(60, 20))),
      content = "oblong text",
      font = "Dialog",
      size = 12,
      weight = 400,
      fill = fillWith(PresetColor.Black),
      line = None
    )

    val rotated = text.rotateAround(90, Pos(30, 10))

    // Rotating around the middle of its box swaps the bounding box dimensions.
    assertEquals(rounded(rotated.bounds), Some(Bounds(Pos(20, -20), Pos(40, 40))))
  }

  // GradientPlacementSuite's "#9 ..." tests cover #9 (point-defined gradients) on both platforms.

  // LinearGradient documents that "a rotated element rotates its gradient with it". These
  // tests cover two element kinds that once painted the gradient left to right however far
  // they were turned. GradientPlacementSuite tests the full behavior on both platforms.
  private def paintsBlueEndAtTheTop(element: Element): Boolean =
    val bitmap = element.toBitmap()
    val top    = bitmap.getColor(bitmap.pixels().width / 2, 3)
    val bottom = bitmap.getColor(bitmap.pixels().width / 2, bitmap.pixels().height - 4)
    top.blue > top.red && bottom.red > bottom.blue

  private val redToBlueAcross40Pixels =
    smile.colors.LinearGradient(Pos(0, 0), Pos(40, 0), red, blue)

  test("a rotated circle rotates its gradient with it") {
    // Turning a circle does not change its outline, but it does change a gradient painted
    // on it, which lives in the circle's own frame.
    val circle = Circle(Pos.Origin, 20, fillWith(redToBlueAcross40Pixels), line = None)

    // +90 degrees is a counterclockwise turn, which takes the blue right end to the top.
    assert(paintsBlueEndAtTheTop(circle.rotateAround(90, Pos.Origin)))
  }

  test("a rotated polygon rotates its gradient with it") {
    // The gradient lives in the square's own frame, which the turn carries.
    val square = Rectangle(40, 40, Pos.Origin, fillWith(redToBlueAcross40Pixels), line = None)

    assert(paintsBlueEndAtTheTop(square.rotateAround(90, Pos.Origin)))
  }

  test("#21 oblong rotations preserve their pivot") {
    val text = Text(
      pos = Pos(30, 10),
      customBounds = Some(Bounds(Pos(0, 0), Pos(60, 20))),
      content = "oblong text",
      font = "Dialog",
      size = 12,
      weight = 400,
      fill = fillWith(PresetColor.Black),
      line = None
    )

    val rotated = text.rotateAround(90, Pos.Origin)

    // The anchor rotates rigidly around the pivot and the box follows.
    val expected = text.anchor.rotateAround(90, Pos.Origin)
    assertEqualsDouble(rotated.anchor.x, expected.x, 1e-6)
    assertEqualsDouble(rotated.anchor.y, expected.y, 1e-6)
    assertEquals(rounded(rotated.bounds), Some(Bounds(Pos(0, -60), Pos(20, 0))))
  }

  test("#21 a turned bitmap keeps its middle on the pivot") {
    // Settings under which a frozen picture showed off its pivot.
    val normalized = RenderSettings(
      drawingIsAntiAliased = false,
      normalizesStrokes = true,
      resamplingMode = ResamplingMode.Stepwise
    )
    val pivot      = Pos(150, 150)
    val frozen     = Rectangle(200, 50, pivot, fillWith(blue), line = None).toBitmap(normalized)
    val background = Rectangle(300, 300, pivot, fillWith(white), line = None)

    // A turned buffer an odd number of pixels wide starts half a pixel off the grid, and is drawn
    // from the next whole pixel, half a pixel right of its place. The same holds down the rows for
    // an odd height.
    def offFromTheGrid(size: Double): Double = if size.toInt % 2 == 1 then 0.5 else 0.0

    for angle <- 0 until 360 do
      val turned = frozen.rotateAround(angle, pivot)
      val pixels =
        JVMRenderer
          .createBitmapFrom(background.addToFront(turned), 1, normalized)
          .nativeBuffer(normalized)
      // Each pixel weighs how far it is from white towards blue.
      val (x, y) = weightedMiddle(pixels, argb => 1 - ((argb >> 16) & 255) / 255.0)
      val box    = turned.bounds.get
      assertEqualsDouble(x - pivot.x, offFromTheGrid(box.width), 1e-3, s"$angle")
      assertEqualsDouble(y - pivot.y, offFromTheGrid(box.height), 1e-3, s"$angle")
  }

  test("#12 rotating around a named-anchor pivot keeps the pivot fixed") {
    // An edge anchor follows the bounds, and a picture turns around it. A TopLeft anchor is
    // bounds.upperLeftCorner, which this pivot emulates.
    val rectangle    = Rectangle(8, 4, Pos(10, 20), fillWith(red), line = None)
    val topLeftPivot = rectangle.bounds.get.upperLeftCorner

    val rotated = rectangle.rotateAround(90, topLeftPivot)

    // A rigid 90-degree rotation around the top left corner keeps that corner
    // on the bounds and swaps the dimensions.
    assertEquals(rotated.bounds, Some(Bounds(Pos(6, 10), Pos(10, 18))))
  }

  test("#7 suggestCaseInsensitiveMatch reports the name as it appears on disk") {
    withTemporaryDirectory: directory =>
      writePng(directory.resolve("Bird.png"))

      assertEquals(
        JVMResourceFactory.suggestCaseInsensitiveMatch(directory, "bird.png"),
        Some("Bird.png")
      )
      assertEquals(
        JVMResourceFactory.suggestCaseInsensitiveMatch(directory, "BIRD.PNG"),
        Some("Bird.png")
      )
  }

  test("#7 suggestCaseInsensitiveMatch stays quiet without an actual case mismatch") {
    withTemporaryDirectory: directory =>
      writePng(directory.resolve("Bird.png"))

      // An exact hit is not a mismatch, and an unrelated name has no suggestion.
      assertEquals(JVMResourceFactory.suggestCaseInsensitiveMatch(directory, "Bird.png"), None)
      assertEquals(JVMResourceFactory.suggestCaseInsensitiveMatch(directory, "cat.png"), None)
      assertEquals(JVMResourceFactory.suggestCaseInsensitiveMatch(directory, ""), None)

      // A directory that does not exist gives no suggestion and throws nothing.
      assertEquals(
        JVMResourceFactory.suggestCaseInsensitiveMatch(directory.resolve("missing"), "bird.png"),
        None
      )
  }

  test("#7 loading a case-mismatched path suggests the correctly-cased file") {
    assume(
      temporaryFilesystemIsCaseSensitive,
      "case-sensitive filesystem needed (a mis-cased path just loads elsewhere)"
    )

    withTemporaryDirectory: directory =>
      writePng(directory.resolve("Bird.png"))

      val error = intercept[IOException]:
        Bitmap(directory.resolve("bird.png").toString)

      assert(
        error.getMessage.contains("did you mean 'Bird.png'?"),
        s"missing suggestion in: ${error.getMessage}"
      )
  }

  test("#6 a display scale factor rasterizes at the denser resolution") {
    // One logical pixel becomes a 2x2 block of device pixels, because the point is drawn in a
    // scaled coordinate system.
    val point       = Point(Pos(0, 0), red)
    val scaledPoint = JVMRenderer.createBitmapFrom(point, 2.0)

    assertEquals(scaledPoint.pixels().width, 2)
    assertEquals(scaledPoint.pixels().height, 2)
    assertEquals(scaledPoint.pixels().colors, Seq.fill(4)(red))

    // The extra pixels carry new detail. At least one 2x2 block of the dense render is not
    // uniform, which a nearest-neighbor enlargement of the 1x render could never produce.
    // The circle's antialiased edge is where that shows.
    val circle = Circle(Pos(0, 0), 20, fillWith(blue), line = None)
    val atOne  = JVMRenderer.createBitmapFrom(circle)
    val atTwo  = JVMRenderer.createBitmapFrom(circle, 2.0)

    assertEquals(atOne.pixels().width, 40)
    assertEquals(atTwo.pixels().width, 80)
    assertEquals(atTwo.bounds, atOne.bounds)

    val blocksWithNewDetail =
      for
        y <- 0 until atOne.pixels().height
        x <- 0 until atOne.pixels().width
        block = Set(
          atTwo.getColor(2 * x, 2 * y),
          atTwo.getColor(2 * x + 1, 2 * y),
          atTwo.getColor(2 * x, 2 * y + 1),
          atTwo.getColor(2 * x + 1, 2 * y + 1)
        )
        if block.size > 1
      yield (x, y)

    assert(blocksWithNewDetail.nonEmpty, "the dense render carries no detail beyond the 1x one")
  }
