package smile.testing

/** The outcome of comparing two renderings of one picture.
  *
  * @param failures
  *   One line per check that did not hold, naming the check, what was measured, and the limit.
  *   Empty when the renderings agree.
  * @param measurements
  *   Everything that was measured, whether it failed or not, for the report.
  * @param mismatchBounds
  *   The box `(left, top, right, bottom)` around the pixels that broke a per-pixel check.
  * @param diff
  *   A picture of the comparison, with agreeing pixels dimmed, tolerated differences in amber and
  *   pixels that broke a check in magenta.
  */
final case class ParityDiff(
    failures: Seq[String],
    measurements: Seq[(String, String)],
    mismatchBounds: Option[(Int, Int, Int, Int)],
    diff: Raster
):
  def passed: Boolean = failures.isEmpty

  def report: String =
    val verdict = if passed then "agree" else failures.map("FAILED " + _).mkString("\n")
    val bounds  = mismatchBounds
      .map((left, top, right, bottom) => s"mismatch bounds: ($left,$top)-($right,$bottom)\n")
      .getOrElse("")
    s"$verdict\n$bounds" + measurements.map((name, value) => s"$name: $value").mkString("\n")

/** Compares two renderings of one picture.
  *
  * Pixels are compared premultiplied by their alpha, so a fully transparent pixel is the same pixel
  * whatever color it nominally has, and the loss a canvas incurs by storing premultiplied values
  * shrinks with the alpha it happens at. The difference between two pixels is their largest
  * difference on any channel, out of 255.
  *
  * What is checked depends on the [[smile.testing.Expectation]]:
  *
  *   - **Dimensions** are equal. Always, except for [[smile.testing.Expectation.Layout]], where
  *     each platform measures the text itself and the renderings are laid over each other by their
  *     centers.
  *   - [[smile.testing.Expectation.Exact]]: no pixel differs by more than
  *     [[smile.testing.ParityMetric.RoundingStep]].
  *   - [[smile.testing.Expectation.AA]] and [[smile.testing.Expectation.Gradient]], pixel by pixel:
  *     - A **flat** pixel, one whose 3x3 neighborhood is one color in both renderings, is away from
  *       every edge, so antialiasing has no say in it. It must be equal, or within
  *       [[smile.testing.ParityMetric.RoundingStep]] where translucent paint is blended. Under
  *       `Gradient` the pixels whose neighborhood has one alpha and holds no sharp change of color
  *       take this role and get [[smile.testing.ParityMetric.GradientStep]].
  *       [[smile.testing.ParityMetric.JumpBudget]] of the picture's pixels may find their color
  *       next door, which is what happens along the jumps of a repeating gradient.
  *     - Any other pixel is at an **edge**. It must find a pixel within
  *       [[smile.testing.ParityMetric.EdgeStep]] of itself in the other rendering's 3x3
  *       neighborhood, and the other way round, because two rasterizers shade an edge differently
  *       but put it in the same place. [[smile.testing.ParityMetric.EdgeBudget]] of the edge
  *       pixels, and never fewer than two, may fail to.
  *   - The neighborhood rule forgives a picture shifted by a pixel, so three measurements are also
  *     taken over the whole picture. The **alpha mass** is the sum of all alphas, which is the
  *     painted area. It agrees within [[smile.testing.ParityMetric.MassTolerance]] of a pixel for
  *     each edge pixel, since it is at the edges that rasterizers differ. Its **centroid** lies
  *     within [[smile.testing.ParityMetric.CentroidTolerance]] pixels on each axis, which a shift
  *     of half a pixel breaks. The **ink box** around the pixels at least half covered agrees
  *     within a pixel per edge.
  *   - [[smile.testing.Expectation.Layout]] checks only those three, with its own slack.
  *
  * The numbers were calibrated on Java2D with pure stroke control against Chromium's canvas. The
  * centroid is a mean over the picture, so the catalog keeps its pictures small and to one feature
  * each.
  */
object ParityMetric:
  val RoundingStep: Int         = 2
  val GradientStep: Int         = 12
  val EdgeStep: Int             = 64
  val EdgeBudget: Double        = 0.001
  val JumpBudget: Double        = 0.04
  val MassTolerance: Double     = 0.05
  val CentroidTolerance: Double = 0.25
  val InkBoxTolerance: Double   = 1.0

  /** The share of its ink a text may gain or lose between two font engines. */
  val LayoutMassTolerance: Double = 0.25

  /** Two pixels of mass, which two rasterizers may always differ by whatever the picture's size. */
  private val MassFloor = 2.0 * 255

  private val BarelyCovered = 64
  private val HalfCovered   = 128
  private val MostlyCovered = 192

  private[testing] val Violation = 0xffff00ff
  private val Tolerated          = 0xffffb000
  private val DimAlpha           = 64

  /** Compares two renderings under `expectation`. A [[smile.testing.Expectation.KnownDiffers]] is
    * compared under the profile that would hold without its difference.
    * [[smile.testing.ParityMetric.judge]] weighs its bound too.
    */
  def compare(a: Raster, b: Raster, expectation: Expectation, translucent: Boolean): ParityDiff =
    compare(a, b, expectation, translucent, Limits())

  /** Limits that replace the calibrated ones, for comparisons that tolerate more than two renderers
    * do, and for the bounds of known differences.
    *
    * @param flatStep
    *   Maximum difference per channel for flat pixels, or for all pixels under
    *   [[smile.testing.Expectation.Exact]]. Overrides the profile's step.
    * @param edgeBudget
    *   Maximum fraction of unmatched edge pixels. Overrides
    *   [[smile.testing.ParityMetric.EdgeBudget]].
    * @param massTolerance
    *   Maximum alpha-mass difference. The default is [[smile.testing.ParityMetric.MassTolerance]]
    *   pixels per edge pixel. Under [[smile.testing.Expectation.Layout]] it is
    *   [[smile.testing.ParityMetric.LayoutMassTolerance]] times the larger mass.
    */
  final case class Limits(
      flatStep: Option[Int] = None,
      edgeBudget: Double = EdgeBudget,
      massTolerance: Option[Double] = None
  )

  /** What a comparison under an expectation came to. `diff` is the comparison under the expectation
    * itself, or for a [[smile.testing.Expectation.KnownDiffers]] under the profile that would hold
    * without the difference. `bound` is the comparison under a known difference's bound.
    */
  final case class Verdict(diff: ParityDiff, bound: Option[ParityDiff]):
    /** Whether the renderings agree, or differ no further than a known difference's bound. */
    def passed: Boolean = diff.passed || bound.exists(_.passed)

    /** In a few words, whether the renderings agree and whether a known difference showed. */
    def outcome: String =
      if diff.passed then "agree"
      else if bound.exists(_.passed) then "differ as known, within the bound"
      else "disagree"

    def report: String =
      bound match
        case None        => diff.report
        case Some(known) =>
          s"outcome: $outcome\n${diff.report}\nagainst the known difference's bound:\n" +
            known.report

  /** Compares two renderings under `expectation`. A known difference passes where the renderings
    * agree under the profile that would hold without it, and also where they differ within its
    * bound.
    */
  def judge(a: Raster, b: Raster, expectation: Expectation, translucent: Boolean): Verdict =
    expectation match
      case Expectation.KnownDiffers(_, otherwise, bound, limits) =>
        Verdict(
          compare(a, b, otherwise, translucent),
          Some(compare(a, b, bound, translucent, limits))
        )
      case other => Verdict(compare(a, b, other, translucent), None)

  private[testing] def compare(
      a: Raster,
      b: Raster,
      expectation: Expectation,
      translucent: Boolean,
      limits: Limits
  ): ParityDiff =
    expectation match
      case Expectation.KnownDiffers(_, otherwise, _, _) =>
        compare(a, b, otherwise, translucent, limits)
      case _ => compareUnder(a, b, expectation, translucent, limits)

  private[testing] def premultiplied(argb: Int): Int =
    val alpha               = argb >>> 24
    def channel(shift: Int) = (((argb >> shift) & 0xff) * alpha + 127) / 255
    (alpha << 24) | (channel(16) << 16) | (channel(8) << 8) | channel(0)

  private[testing] def delta(p: Int, q: Int): Int =
    val alpha = ((p >>> 24) - (q >>> 24)).abs
    val red   = (((p >> 16) & 0xff) - ((q >> 16) & 0xff)).abs
    val green = (((p >> 8) & 0xff) - ((q >> 8) & 0xff)).abs
    val blue  = ((p & 0xff) - (q & 0xff)).abs
    alpha.max(red).max(green.max(blue))

  /** A rendering laid onto a common frame of `width` x `height`, premultiplied, and transparent
    * where the rendering does not reach.
    */
  private final class Layer(source: Raster, val width: Int, val height: Int, centered: Boolean):
    private val offsetX = if centered then (width - source.width) / 2 else 0
    private val offsetY = if centered then (height - source.height) / 2 else 0

    val pixels: Array[Int] =
      val result = new Array[Int](width * height)
      for
        y <- 0 until source.height
        x <- 0 until source.width
      do result((y + offsetY) * width + x + offsetX) = premultiplied(source(x, y))
      result

    def apply(x: Int, y: Int): Int =
      pixels(y.max(0).min(height - 1) * width + x.max(0).min(width - 1))

    /** How much the 3x3 neighborhood varies on any channel, and how much on alpha alone. */
    def variation(x: Int, y: Int): (Int, Int) =
      val low  = Array(255, 255, 255, 255)
      val high = Array(0, 0, 0, 0)
      for
        dy <- -1 to 1
        dx <- -1 to 1
      do
        val pixel = this(x + dx, y + dy)
        var index = 0
        while index < 4 do
          val channel = (pixel >>> (index * 8)) & 0xff
          if channel < low(index) then low(index) = channel
          if channel > high(index) then high(index) = channel
          index += 1
      ((0 until 4).map(index => high(index) - low(index)).max, high(3) - low(3))

    lazy val mass: Double = pixels.iterator.map(pixel => (pixel >>> 24).toDouble).sum

    lazy val centroid: Option[(Double, Double)] =
      if mass == 0 then None
      else
        var (sumX, sumY) = (0.0, 0.0)
        for index <- pixels.indices do
          val alpha = pixels(index) >>> 24
          sumX += alpha * (index % width + 0.5)
          sumY += alpha * (index / width + 0.5)
        Some((sumX / mass - width / 2.0, sumY / mass - height / 2.0))

    /** The box around the pixels covered at least `threshold` of 255, relative to the frame's
      * center.
      */
    def inkBox(threshold: Int): Option[(Double, Double, Double, Double)] =
      val covered = pixels.indices.filter(index => (pixels(index) >>> 24) >= threshold)
      Option.when(covered.nonEmpty):
        val xs = covered.map(_ % width)
        val ys = covered.map(_ / width)
        (
          xs.min - width / 2.0,
          ys.min - height / 2.0,
          xs.max + 1 - width / 2.0,
          ys.max + 1 - height / 2.0
        )
  end Layer

  private def compareUnder(
      a: Raster,
      b: Raster,
      expectation: Expectation,
      translucent: Boolean,
      limits: Limits
  ): ParityDiff =
    val isLayout = expectation match
      case Expectation.Layout(_) => true
      case _                     => false
    val width  = a.width.max(b.width)
    val height = a.height.max(b.height)
    val first  = Layer(a, width, height, centered = isLayout)
    val second = Layer(b, width, height, centered = isLayout)

    val failures     = Seq.newBuilder[String]
    val measurements = Seq.newBuilder[(String, String)]
    measurements += "dimensions" -> s"${a.width}x${a.height} and ${b.width}x${b.height}"
    measurements += "profile"    -> expectation.toString

    if !isLayout && (a.width != b.width || a.height != b.height) then
      failures += s"dimensions: ${a.width}x${a.height} against ${b.width}x${b.height}"

    val diff                    = new Array[Int](width * height)
    var (differing, largest)    = (0, 0)
    var (flatBroken, flatWorst) = (0, 0)
    var (edges, edgesBroken)    = (0, 0)
    var jumps                   = 0
    var bounds                  = Option.empty[(Int, Int, Int, Int)]
    val flatStep                = limits.flatStep.getOrElse:
      expectation match
        case Expectation.Gradient => GradientStep
        case _                    => if translucent then RoundingStep else 0

    def nearest(pixel: Int, in: Layer, x: Int, y: Int): Int =
      var best = 255
      for
        dy <- -1 to 1
        dx <- -1 to 1
      do best = best.min(delta(pixel, in(x + dx, y + dy)))
      best

    for
      y <- 0 until height
      x <- 0 until width
    do
      val (p, q)   = (first(x, y), second(x, y))
      val distance = delta(p, q)
      if distance > 0 then differing += 1
      largest = largest.max(distance)

      val broken = expectation match
        case Expectation.Exact     => distance > limits.flatStep.getOrElse(RoundingStep)
        case Expectation.Layout(_) => false
        case _                     =>
          val (variationA, alphaVariationA) = first.variation(x, y)
          val (variationB, alphaVariationB) = second.variation(x, y)
          val isFlat                        = expectation match
            // Evenly covered, and no boundary between two paints running through.
            case Expectation.Gradient =>
              alphaVariationA == 0 && alphaVariationB == 0 &&
              variationA <= EdgeStep && variationB <= EdgeStep
            case _ => variationA <= RoundingStep && variationB <= RoundingStep
          if isFlat then
            // Where a gradient jumps, as a repeating one does from its last color back to its
            // first, the pixel the jump runs through takes either side's color. Such a pixel is
            // let off if the other rendering has its color next door.
            val isAtJump = distance > flatStep && expectation == Expectation.Gradient &&
              nearest(p, second, x, y).max(nearest(q, first, x, y)) <= flatStep
            if isAtJump then jumps += 1
            if distance > flatStep && !isAtJump then
              flatBroken += 1
              flatWorst = flatWorst.max(distance)
              true
            else false
          else
            edges += 1
            val unmatched = nearest(p, second, x, y).max(nearest(q, first, x, y)) > EdgeStep
            if unmatched then edgesBroken += 1
            unmatched

      if broken then
        bounds = Some(
          bounds.fold((x, y, x, y))((left, top, right, bottom) =>
            (left.min(x), top.min(y), right.max(x), bottom.max(y))
          )
        )
      diff(y * width + x) =
        if broken then Violation
        else if distance > 0 then Tolerated
        else
          val original = a.at(x, y)
          ((original >>> 24).min(DimAlpha) << 24) | (original & 0xffffff)

    measurements += "differing pixels"   -> s"$differing of ${width * height}"
    measurements += "largest difference" -> s"$largest of 255"

    expectation match
      case Expectation.Exact =>
        val broken = diff.count(_ == Violation)
        if broken > 0 then
          failures += s"pixels: $broken differ by more than " +
            s"${limits.flatStep.getOrElse(RoundingStep)} (largest: $largest)"
      case Expectation.Layout(_) =>
      case _                     =>
        measurements += "flat pixels off" -> s"$flatBroken (limit $flatStep per channel)"
        measurements += "edge pixels"     -> s"$edges, unmatched $edgesBroken"
        val jumpBudget = (width * height * JumpBudget).ceil.toInt
        if jumps > jumpBudget then
          failures += s"gradient: $jumps pixels have their color only next door, more than " +
            s"jumps in a gradient account for (allowed: $jumpBudget): it is shifted"
        if flatBroken > 0 then
          failures += s"flat areas: $flatBroken pixels away from every edge differ by more " +
            s"than $flatStep (largest: $flatWorst)"
        val budget = (edges * limits.edgeBudget).ceil.toInt.max(2)
        if edgesBroken > budget then
          failures += s"edges: $edgesBroken of $edges edge pixels have no counterpart within " +
            s"a pixel (allowed: $budget)"

    expectation match
      case Expectation.Exact =>
      case _                 =>
        val (massSlack, centroidSlack, boxSlack) = expectation match
          case Expectation.Layout(slack) =>
            (limits.massTolerance.getOrElse(LayoutMassTolerance), slack, slack)
          case _ =>
            (limits.massTolerance.getOrElse(MassTolerance), CentroidTolerance, InkBoxTolerance)

        val massDifference = (first.mass - second.mass).abs
        val massAllowed    = expectation match
          case Expectation.Layout(_) => first.mass.max(second.mass) * massSlack + MassFloor
          case _                     => edges * massSlack * 255 + MassFloor
        measurements += "alpha mass" ->
          f"${first.mass / 255}%.1f and ${second.mass / 255}%.1f pixels"
        if massDifference > massAllowed then
          failures += f"alpha mass: ${first.mass / 255}%.1f against ${second.mass / 255}%.1f " +
            f"pixels of ink (allowed difference: ${massAllowed / 255}%.1f)"

        (first.centroid, second.centroid) match
          case (Some((ax, ay)), Some((bx, by))) =>
            measurements += "centroid shift" -> f"(${bx - ax}%.3f, ${by - ay}%.3f) px"
            if (bx - ax).abs > centroidSlack || (by - ay).abs > centroidSlack then
              failures += f"centroid: the ink sits (${bx - ax}%.3f, ${by - ay}%.3f) px apart " +
                s"(allowed: $centroidSlack per axis)"
          case (None, None) =>
          case _            => failures += "ink: one rendering paints nothing at all"

        def shown(box: Option[(Double, Double, Double, Double)]) =
          box.fold("none")((left, top, right, bottom) => s"($left,$top)-($right,$bottom)")
        measurements += "ink boxes (from the center)" ->
          s"${shown(first.inkBox(HalfCovered))} and ${shown(second.inkBox(HalfCovered))}"

        // Each rendering's half-covered box has to lie between the other's boxes of the barely
        // and of the mostly covered pixels. Comparing the two half-covered boxes directly would
        // hang on which side of one half a long thin tip's coverage falls.
        def bracketed(of: Layer, by: Layer): Boolean =
          (of.inkBox(HalfCovered), by.inkBox(BarelyCovered), by.inkBox(MostlyCovered)) match
            case (Some((left, top, right, bottom)), Some(outer), inner) =>
              val within = left >= outer._1 - boxSlack && top >= outer._2 - boxSlack &&
                right <= outer._3 + boxSlack && bottom <= outer._4 + boxSlack
              val around = inner.forall: (innerLeft, innerTop, innerRight, innerBottom) =>
                left <= innerLeft + boxSlack && top <= innerTop + boxSlack &&
                  right >= innerRight - boxSlack && bottom >= innerBottom - boxSlack
              within && around
            case (None, _, inner) => inner.isEmpty
            case _                => false
        if !bracketed(first, second) || !bracketed(second, first) then
          failures += s"ink box: ${shown(first.inkBox(HalfCovered))} against " +
            s"${shown(second.inkBox(HalfCovered))} (allowed: $boxSlack px per edge)"

    ParityDiff(failures.result(), measurements.result(), bounds, Raster(width, height, diff))
  end compareUnder
