package smile.infrastructure

import org.scalajs.dom
import org.scalajs.dom.html

import scala.scalajs.js
import scala.util.control.NonFatal

/** Text measurement shared by the canvas and SVG renderers: the CSS font and the hidden measurement
  * context. `smile.modeling.JSBoundaryCalculator.textMetrics` measures each text here once, and
  * both renderers take its baseline from that measurement, so that their text cannot drift apart.
  */
private[smile] object JSTextMetrics:
  /** The CSS `font` shorthand for a font name, size and weight, shared by the canvas painter and
    * the hidden measurement context.
    */
  def cssFont(font: String, size: Double, weight: Int): String =
    s"$weight ${size}px ${FontFamily.css(font)}"

  /** A hidden canvas context kept solely for measuring text. SVG generation always happens with a
    * `document` in reach, in a browser or in jsdom in the tests. The context is still created
    * lazily and inside a `try` so that a headless environment falls back to unscaled glyphs.
    */
  private lazy val measurementContext: Option[dom.CanvasRenderingContext2D] =
    try
      val measurementCanvas = dom.document.createElement("canvas").asInstanceOf[html.Canvas]
      Option(measurementCanvas.getContext("2d"))
        .map(_.asInstanceOf[dom.CanvasRenderingContext2D])
    catch case NonFatal(_) => None

  /** A line of text as the hidden measurement context measures it.
    *
    * @param advance
    *   How far the glyphs advance, which is the line's width.
    * @param line
    *   The font's ascent and descent, the line's height above and below the baseline. `None` where
    *   the environment does not tell.
    * @param ink
    *   How far the ink itself reaches from the middle of the advance to the left and to the right,
    *   and from the baseline up and down. `None` where the environment does not tell.
    */
  private[smile] final case class TextMeasurement(
      advance: Double,
      line: Option[(Double, Double)],
      ink: Option[(Double, Double, Double, Double)]
  ):
    /** How far below the middle of its line the alphabetic baseline lies, which is half of what the
      * font's ascent exceeds its descent by.
      *
      * Both backends place the baseline by this one number, which is also the JVM's rule. Left to
      * center a line themselves they would disagree. A canvas's "middle" baseline and SVG's
      * "central" one are a pixel apart at 20 pixels. `None` where the font's metrics cannot be had,
      * and the backends then fall back on those two baselines.
      */
    def baselineDrop: Option[Double] = line.map((ascent, descent) => (ascent - descent) / 2)

    /** The box around the ink as left, top, right and bottom, in the frame where the middle of the
      * line is the origin and the baseline lies the baseline drop below it. `None` where the
      * environment does not measure the ink or the font's ascent and descent, as in jsdom without a
      * canvas, or where nothing is inked.
      */
    def inkBox: Option[(Double, Double, Double, Double)] =
      for
        (left, right, ascent, descent) <- ink
        drop                           <- baselineDrop
        if left + right > 0 || ascent + descent > 0
      yield (-left, drop - ascent, right, drop + descent)

    /** The layout box of the line, as wide as its glyphs advance and as tall as the font's ascent
      * and descent, or the font's `size` where the environment does not tell those. It stands in
      * for the browser's layout engine where there is none, as in jsdom.
      */
    def lineBox(size: Double): (Double, Double) =
      (advance, line.fold(size)((ascent, descent) => ascent + descent))
  end TextMeasurement

  /** `content` in `font` at `size` pixels and `weight` as the hidden measurement context measures
    * it, with one `measureText`. `None` where there is no such context.
    */
  private[smile] def measured(
      content: String,
      font: String,
      size: Double,
      weight: Int
  ): Option[TextMeasurement] =
    measurementContext.map: ctx =>
      measurements += 1
      ctx.font = cssFont(font, size, weight)
      ctx.textAlign = "center"
      ctx.textBaseline = "alphabetic"
      val metrics                                = ctx.measureText(content)
      val dynamic                                = metrics.asInstanceOf[js.Dynamic]
      def metric(names: String*): Option[Double] =
        names
          .map(dynamic.selectDynamic)
          .collectFirst:
            case value if js.typeOf(value) == "number" && !value.asInstanceOf[Double].isNaN =>
              value.asInstanceOf[Double]
      TextMeasurement(
        advance = metrics.width,
        line = for
          ascent  <- metric("fontBoundingBoxAscent", "emHeightAscent")
          descent <- metric("fontBoundingBoxDescent", "emHeightDescent")
        yield (ascent, descent),
        ink = for
          left    <- metric("actualBoundingBoxLeft")
          right   <- metric("actualBoundingBoxRight")
          ascent  <- metric("actualBoundingBoxAscent")
          descent <- metric("actualBoundingBoxDescent")
        yield (left, right, ascent, descent)
      )

  /** How many times a text has been measured, which tests read. */
  private[smile] var measurements: Long = 0
end JSTextMetrics
