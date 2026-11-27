package smile.testing

import smile.colors.Color
import smile.modeling.{Bounds, Pos}

import scala.collection.mutable

/** How much two renderings of a picture may differ and still count as the same picture. */
enum Tolerance derives CanEqual:

  /** No pixel may differ by more than a rounding step. The same renderer drew the same picture. */
  case Strict

  /** Antialiased edges may differ, as long as they lie in the same place. Away from the edges the
    * colors must match. These are the rules SMILe's parity harness compares its renderers with.
    */
  case Antialiased

  /** As [[smile.testing.Tolerance.Antialiased]], but colors away from the edges may differ by up to
    * `colorStep` per channel, and up to `edgeBudget` of the edge pixels may lack a counterpart.
    */
  case Loose(colorStep: Int, edgeBudget: Double)

/** A place where two pictures differ, as a connected group of pixels that broke the comparison.
  *
  * @param bounds
  *   The box around the pixels, in pixel coordinates.
  * @param expected
  *   The most common color of the expected picture in those pixels.
  * @param found
  *   The most common color of the actual picture in those pixels.
  * @param pixels
  *   How many pixels the group has.
  */
final case class DiffRegion(bounds: Bounds, expected: Color, found: Color, pixels: Int)

/** A color whose area differs between the pictures. It covers a different number of pixels, or its
  * pixels sit elsewhere on average. This catches a shape that is a pixel off in position or in
  * size, which the per-pixel rules forgive as an antialiasing difference.
  *
  * @param expectedPixels
  *   How many pixels have the color in the expected picture.
  * @param foundPixels
  *   How many have it in the actual one.
  * @param centroidShift
  *   How far the mean position of those pixels moved, in pixels. Right and down are positive.
  */
final case class ColorMismatch(
    color: Color,
    expectedPixels: Int,
    foundPixels: Int,
    centroidShift: (Double, Double)
)

/** The outcome of comparing a picture with the one it was expected to be.
  *
  * @param sizeMismatch
  *   The actual and the expected size, when they differ. No pixels are compared then.
  * @param regions
  *   Where the pictures differ, largest first. Empty when they pass, and when they fail only on the
  *   amount or the position of the ink in the whole picture.
  * @param colorMismatches
  *   The colors whose area differs in size or position. Empty under
  *   [[smile.testing.Tolerance.Strict]], which compares every pixel exactly anyway.
  * @param measurements
  *   What the comparison measured, for a detailed report.
  * @param diffImage
  *   The actual picture dimmed, with tolerated differences in amber and failures in magenta.
  */
final case class PictureDiff(
    passed: Boolean,
    sizeMismatch: Option[((Int, Int), (Int, Int))],
    regions: Seq[DiffRegion],
    colorMismatches: Seq[ColorMismatch],
    measurements: Seq[(String, String)],
    diffImage: Raster
)

/** Compares a picture with the picture it was expected to be, and says where they differ. */
object PictureComparison:

  def compare(
      actual: Raster,
      expected: Raster,
      tolerance: Tolerance = Tolerance.Antialiased
  ): PictureDiff =
    if actual.width != expected.width || actual.height != expected.height then
      PictureDiff(
        passed = false,
        sizeMismatch = Some(((actual.width, actual.height), (expected.width, expected.height))),
        regions = Seq.empty,
        colorMismatches = Seq.empty,
        measurements = Seq.empty,
        diffImage = actual
      )
    else
      val (expectation, limits) = tolerance match
        case Tolerance.Strict                   => (Expectation.Exact, ParityMetric.Limits())
        case Tolerance.Antialiased              => (Expectation.AA, ParityMetric.Limits())
        case Tolerance.Loose(colorStep, budget) =>
          (Expectation.AA, ParityMetric.Limits(Some(colorStep), budget))
      val diff    = ParityMetric.compare(actual, expected, expectation, translucent = false, limits)
      val regions =
        if diff.passed then Seq.empty else regionsOf(diff.diff, actual, expected)
      val mismatches = tolerance match
        case Tolerance.Strict              => Seq.empty
        case Tolerance.Antialiased         => colorMismatchesOf(actual, expected, 0)
        case Tolerance.Loose(colorStep, _) => colorMismatchesOf(actual, expected, colorStep)
      PictureDiff(
        diff.passed && mismatches.isEmpty,
        None,
        regions,
        mismatches,
        diff.measurements,
        diff.diff
      )

  /** Colors covering fewer pixels than this in the expected picture count as antialiasing shades
    * and are excluded from the area check.
    */
  private val MinColorPixels = 16

  /** Maximum shift in a color's centroid, in pixels. Moving a shape by one pixel shifts the
    * centroid a full pixel. Antialiasing shifts it by a fraction.
    */
  private val CentroidSlack = 0.5

  private def colorMismatchesOf(actual: Raster, expected: Raster, step: Int): Seq[ColorMismatch] =
    val width  = expected.width
    val counts = expected.argb.groupMapReduce(identity)(_ => 1)(_ + _)
    val colors = counts.filter(_._2 >= MinColorPixels).keys.toSeq.sortBy(argb => -counts(argb))
    colors.flatMap: argb =>
      val target              = ParityMetric.premultiplied(argb)
      def matches(pixel: Int) =
        ParityMetric.delta(ParityMetric.premultiplied(pixel), target) <= step
      def stats(raster: Raster): (Int, Double, Double) =
        var (count, sumX, sumY) = (0, 0.0, 0.0)
        for index <- raster.argb.indices if matches(raster.argb(index)) do
          count += 1
          sumX += index % width + 0.5
          sumY += index / width + 0.5
        (count, sumX, sumY)
      val (expectedCount, expectedX, expectedY) = stats(expected)
      val (foundCount, foundX, foundY)          = stats(actual)
      // Each edge pixel may turn into a shade or back, so half of them may change sides.
      val edgePixels = expected.argb.indices.count: index =>
        matches(expected.argb(index)) && {
          val (x, y) = (index % width, index / width)
          Seq((x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)).exists: (nx, ny) =>
            nx >= 0 && ny >= 0 && nx < width && ny < expected.height &&
              !matches(expected.argb(ny * width + nx))
        }
      val shift =
        if foundCount == 0 then (Double.NaN, Double.NaN)
        else
          (
            foundX / foundCount - expectedX / expectedCount,
            foundY / foundCount - expectedY / expectedCount
          )
      val countOff = (foundCount - expectedCount).abs > edgePixels / 2 + 2
      val moved    = foundCount == 0 || shift._1.abs > CentroidSlack || shift._2.abs > CentroidSlack
      Option.when(countOff || moved)(
        ColorMismatch(new Color(argb), expectedCount, foundCount, shift)
      )

  /** The 8-connected groups of failed pixels in a comparison's picture, largest first. */
  private def regionsOf(diff: Raster, actual: Raster, expected: Raster): Seq[DiffRegion] =
    val width  = diff.width
    val failed = diff.argb.map(_ == ParityMetric.Violation)
    val seen   = new Array[Boolean](failed.length)
    val groups = Seq.newBuilder[DiffRegion]
    for start <- failed.indices if failed(start) && !seen(start) do
      val members = mutable.ArrayBuffer(start)
      seen(start) = true
      var next = 0
      while next < members.length do
        val index  = members(next)
        val (x, y) = (index % width, index / width)
        for
          dy <- -1 to 1
          dx <- -1 to 1
          (nx, ny) = (x + dx, y + dy)
          if nx >= 0 && ny >= 0 && nx < width && ny < diff.height
          neighbor = ny * width + nx
          if failed(neighbor) && !seen(neighbor)
        do
          seen(neighbor) = true
          members += neighbor
        next += 1
      val xs = members.map(_ % width)
      val ys = members.map(_ / width)
      groups += DiffRegion(
        Bounds(Pos(xs.min, ys.min), Pos(xs.max + 1, ys.max + 1)),
        mostCommon(members, expected),
        mostCommon(members, actual),
        members.length
      )
    groups.result().sortBy(-_.pixels)

  private def mostCommon(indices: Iterable[Int], raster: Raster): Color =
    val argb = indices.groupMapReduce(raster.argb(_))(_ => 1)(_ + _).maxBy(_._2)._1
    new Color(argb)
