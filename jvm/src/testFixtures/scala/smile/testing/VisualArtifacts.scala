package smile.testing

import smile.colors.Color
import smile.pictures.{Bitmap, toBitmap}

import java.awt.image.BufferedImage
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}
import javax.imageio.ImageIO
import scala.jdk.CollectionConverters.*

/** Compares rendered bitmaps pixel by pixel and, on mismatch, writes expected/actual/diff PNGs plus
  * an HTML report under [[smile.testing.VisualArtifacts.outputRoot]].
  */
object VisualArtifacts:
  private val OutputDirectoryName = "visual-test-results"
  private val CaptionsFileName    = "captions.txt"

  // Diff-image colors. The HTML legend uses these same constants.
  private val MissingExpected      = 0xffff3b30
  private val UnexpectedActual     = 0xff00d7ff
  private val ChangedPixel         = 0xffff00ff
  private val MatchedPixelDimAlpha = 64 // matched pixels stay visible as ~25%-opacity context

  val outputRoot: Path = Paths
    .get(sys.props.getOrElse("smile.visual.output", s"target/$OutputDirectoryName"))
    .toAbsolutePath
    .normalize()

  def reset(): Unit =
    require(
      outputRoot.iterator().asScala.exists(_.toString == OutputDirectoryName),
      s"Refusing to clear a directory outside $OutputDirectoryName: $outputRoot"
    )
    if Files.exists(outputRoot) then
      val paths = Files.walk(outputRoot)
      try
        paths.iterator().asScala.toSeq.sortBy(_.getNameCount).reverse.foreach(Files.deleteIfExists)
      finally paths.close()
    val _ = Files.createDirectories(outputRoot)

  def compare(name: String, expected: Bitmap, actual: Bitmap): VisualDiffResult =
    val expectedImage = imageFrom(expected)
    val actualImage   = imageFrom(actual)
    val diff          = diffImages(expectedImage, actualImage)

    val result = VisualDiffResult(
      name,
      expectedImage.getWidth,
      expectedImage.getHeight,
      actualImage.getWidth,
      actualImage.getHeight,
      diff.mismatchedPixels,
      diff.mismatchBounds,
      artifactDirectory = None
    )

    if result.matches then result
    else
      val directory = persistArtifacts(name, expectedImage, actualImage, diff.image, result)
      result.copy(artifactDirectory = Some(directory))

  private final case class Diff(
      image: BufferedImage,
      mismatchedPixels: Long,
      mismatchBounds: Option[PixelBounds]
  )

  private def diffImages(expected: BufferedImage, actual: BufferedImage): Diff =
    val width  = expected.getWidth.max(actual.getWidth)
    val height = expected.getHeight.max(actual.getHeight)
    val image  = BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB)

    var mismatchCount = 0L
    var bounds        = Option.empty[PixelBounds]

    for
      y <- 0 until height
      x <- 0 until width
    do
      val (pixel, isMismatch) = classifyPixel(pixelAt(expected, x, y), pixelAt(actual, x, y))
      if isMismatch then
        mismatchCount += 1
        bounds = Some(bounds.fold(PixelBounds(x, y, x, y))(_.including(x, y)))
      image.setRGB(x, y, pixel)

    Diff(image, mismatchCount, bounds)

  /** Returns the diff-image color for one pixel position and whether the position counts as a
    * mismatch.
    */
  private def classifyPixel(expected: Option[Int], actual: Option[Int]): (Int, Boolean) =
    (expected, actual) match
      case (Some(expectedArgb), Some(actualArgb)) if expectedArgb == actualArgb =>
        (dim(expectedArgb), false)
      case (None, Some(_))                        => (UnexpectedActual, true)
      case (Some(_), None)                        => (MissingExpected, true)
      case (Some(expectedArgb), Some(actualArgb)) =>
        val expectedAlpha = expectedArgb >>> 24
        val actualAlpha   = actualArgb >>> 24
        val color         =
          if expectedAlpha == 0 && actualAlpha > 0 then UnexpectedActual
          else if expectedAlpha > 0 && actualAlpha == 0 then MissingExpected
          else ChangedPixel
        (color, true)
      case (None, None) => (0, false)

  private def persistArtifacts(
      name: String,
      expected: BufferedImage,
      actual: BufferedImage,
      diff: BufferedImage,
      result: VisualDiffResult
  ): Path =
    val artifactName = sanitize(name)
    require(artifactName.nonEmpty, "A visual comparison name must contain a letter or digit")
    val directory = outputRoot.resolve(artifactName)
    val _         = Files.createDirectories(directory)
    writePng(directory.resolve("expected.png"), expected)
    writePng(directory.resolve("actual.png"), actual)
    writePng(directory.resolve("diff.png"), diff)
    writeText(directory.resolve("report.txt"), report(result))
    val _ = writeIndex()
    directory

  /** Writes a comparison that was made elsewhere, such as one with a tolerant metric or between two
    * renderings neither of which is "the expected one", in the layout
    * [[smile.testing.VisualArtifacts.writeIndex]] shows.
    *
    * @param first
    *   The caption and picture shown where a visual test shows the expected rendering.
    * @param second
    *   The caption and picture shown where a visual test shows the actual rendering.
    * @param extras
    *   Further text files to keep next to the pictures, as name and contents.
    * @return
    *   The directory the artifacts were written to.
    */
  def persistComparison(
      name: String,
      first: (String, BufferedImage),
      second: (String, BufferedImage),
      diff: BufferedImage,
      report: String,
      extras: Seq[(String, String)] = Seq.empty
  ): Path =
    val artifactName = sanitize(name)
    require(artifactName.nonEmpty, "A visual comparison name must contain a letter or digit")
    val directory = outputRoot.resolve(artifactName)
    val _         = Files.createDirectories(directory)
    writePng(directory.resolve("expected.png"), first._2)
    writePng(directory.resolve("actual.png"), second._2)
    writePng(directory.resolve("diff.png"), diff)
    writeText(directory.resolve("report.txt"), report)
    writeText(directory.resolve(CaptionsFileName), s"${first._1}\n${second._1}\n")
    for (fileName, contents) <- extras do writeText(directory.resolve(fileName), contents)
    val _ = writeIndex()
    directory

  def writeIndex(): Path =
    Files.createDirectories(outputRoot)
    val stream  = Files.list(outputRoot)
    val entries =
      try
        stream.iterator().asScala.filter(Files.isDirectory(_)).toSeq.sortBy(_.getFileName.toString)
      finally stream.close()

    val cards =
      if entries.isEmpty then "<p>No visual mismatches were produced.</p>"
      else entries.map(renderCard).mkString("\n")

    val html = s"""<!doctype html>
      |<html lang="en">
      |<head>
      |  <meta charset="utf-8">
      |  <meta name="viewport" content="width=device-width, initial-scale=1">
      |  <title>SMILe visual test results</title>
      |  <style>
      |    :root { color-scheme: light dark; font-family: system-ui, sans-serif; }
      |    body { margin: 2rem; max-width: 90rem; }
      |    .card { border: 1px solid #8888; border-radius: .75rem; padding: 1rem; margin: 1rem 0; }
      |    .images { display: grid; grid-template-columns: repeat(3, minmax(0, 1fr)); gap: 1rem; }
      |    figure { margin: 0; }
      |    img { width: 100%; min-height: 8rem; object-fit: contain; image-rendering: pixelated;
      |          background: repeating-conic-gradient(#bbb 0 25%, #eee 0 50%) 50% / 16px 16px; }
      |    figcaption { font-weight: 650; margin-bottom: .35rem; }
      |    pre { white-space: pre-wrap; }
      |    .legend span { display: inline-block; width: .8rem; height: .8rem; margin: 0 .3rem 0 1rem; }
      |    @media (max-width: 50rem) { .images { grid-template-columns: 1fr; } }
      |  </style>
      |</head>
      |<body>
      |  <h1>SMILe visual test results</h1>
      |  <p class="legend">Diff colors:
      |    <span style="background:${cssHex(MissingExpected)}"></span>missing expected pixels
      |    <span style="background:${cssHex(UnexpectedActual)}"></span>unexpected actual pixels
      |    <span style="background:${cssHex(ChangedPixel)}"></span>changed pixels
      |  </p>
      |  $cards
      |</body>
      |</html>
      |""".stripMargin

    val index = outputRoot.resolve("index.html")
    writeText(index, html)
    index

  def trimTransparent(bitmap: Bitmap): Bitmap =
    val image        = imageFrom(bitmap)
    val opaquePixels = for
      y <- 0 until image.getHeight
      x <- 0 until image.getWidth
      if (image.getRGB(x, y) >>> 24) != 0
    yield (x, y)

    if opaquePixels.isEmpty then smile.pictures.Empty.toBitmap()
    else
      val xs     = opaquePixels.map(_._1)
      val ys     = opaquePixels.map(_._2)
      val left   = xs.min
      val top    = ys.min
      val right  = xs.max
      val bottom = ys.max
      Bitmap(
        right - left + 1,
        bottom - top + 1,
        for
          y <- top to bottom
          x <- left to right
        yield new Color(image.getRGB(x, y))
      )

  private def renderCard(directory: Path): String =
    val name       = escapeHtml(directory.getFileName.toString)
    val reportPath = directory.resolve("report.txt")
    val reportText =
      if Files.exists(reportPath) then escapeHtml(Files.readString(reportPath)) else ""
    // A comparison between two renderings writes captions naming them. A visual test writes
    // none and shows "Expected" and "Actual".
    val captionsPath                  = directory.resolve(CaptionsFileName)
    val (firstCaption, secondCaption) =
      if Files.exists(captionsPath) then
        Files.readString(captionsPath).linesIterator.map(escapeHtml).toSeq match
          case Seq(first, second, _*) => (first, second)
          case _                      => ("Expected", "Actual")
      else ("Expected", "Actual")
    s"""<section class="card">
      |  <h2>$name</h2>
      |  <div class="images">
      |    <figure><figcaption>$firstCaption</figcaption><img src="$name/expected.png" alt="$firstCaption"></figure>
      |    <figure><figcaption>$secondCaption</figcaption><img src="$name/actual.png" alt="$secondCaption"></figure>
      |    <figure><figcaption>Difference</figcaption><img src="$name/diff.png" alt="Difference"></figure>
      |  </div>
      |  <pre>$reportText</pre>
      |</section>""".stripMargin

  private def report(result: VisualDiffResult): String =
    s"""name: ${result.name}
       |expected: ${result.expectedWidth}x${result.expectedHeight}
       |actual: ${result.actualWidth}x${result.actualHeight}
       |mismatched pixels: ${result.mismatchedPixels}
       |mismatch bounds: ${result.mismatchBounds.map(_.formatted).getOrElse("none")}
       |""".stripMargin

  private def imageFrom(bitmap: Bitmap): BufferedImage =
    bitmap.nativeBuffer().get

  private def pixelAt(image: BufferedImage, x: Int, y: Int): Option[Int] =
    if x < image.getWidth && y < image.getHeight then Some(image.getRGB(x, y)) else None

  private def dim(argb: Int): Int =
    val alpha       = argb >>> 24
    val dimmedAlpha = if alpha == 0 then 0 else alpha.min(MatchedPixelDimAlpha)
    (dimmedAlpha << 24) | (argb & 0x00ffffff)

  private def cssHex(argb: Int): String = f"#${argb & 0xffffff}%06x"

  private def writePng(path: Path, image: BufferedImage): Unit =
    require(ImageIO.write(image, "png", path.toFile), s"No PNG writer available for $path")

  private def writeText(path: Path, contents: String): Unit =
    val _ = Files.writeString(path, contents, StandardCharsets.UTF_8)

  private def sanitize(name: String): String =
    name.toLowerCase
      .replaceAll("[^a-z0-9]+", "-")
      .stripPrefix("-")
      .stripSuffix("-")

  private def escapeHtml(value: String): String =
    value
      .replace("&", "&amp;")
      .replace("<", "&lt;")
      .replace(">", "&gt;")
      .replace("\"", "&quot;")
