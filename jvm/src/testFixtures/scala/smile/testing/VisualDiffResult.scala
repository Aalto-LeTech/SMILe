package smile.testing

import java.nio.file.Path

/** Bounding box of the mismatched pixels, in image coordinates. */
final case class PixelBounds(left: Int, top: Int, right: Int, bottom: Int):
  def formatted: String = s"($left,$top)-($right,$bottom)"

  def including(x: Int, y: Int): PixelBounds =
    PixelBounds(left.min(x), top.min(y), right.max(x), bottom.max(y))

final case class VisualDiffResult(
    name: String,
    expectedWidth: Int,
    expectedHeight: Int,
    actualWidth: Int,
    actualHeight: Int,
    mismatchedPixels: Long,
    mismatchBounds: Option[PixelBounds],
    artifactDirectory: Option[Path]
):
  def matches: Boolean =
    expectedWidth == actualWidth &&
      expectedHeight == actualHeight &&
      mismatchedPixels == 0

  def message: String =
    val dimensions =
      s"expected ${expectedWidth}x$expectedHeight, actual ${actualWidth}x$actualHeight"
    val bounds = mismatchBounds
      .map(b => s"; mismatch bounds ${b.formatted}")
      .getOrElse("")
    val artifacts = artifactDirectory
      .map(path => s"; visual report: ${path.getParent.resolve("index.html").toAbsolutePath}")
      .getOrElse("")
    s"Visual mismatch for '$name': $dimensions; $mismatchedPixels differing pixels$bounds$artifacts"
