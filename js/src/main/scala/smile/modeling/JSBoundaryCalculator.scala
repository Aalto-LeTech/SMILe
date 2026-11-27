package smile.modeling

import org.scalajs.dom
import org.scalajs.dom.document
import scalatags.JsDom.implicits.{doubleAttr, intAttr, stringAttr, stringFrag}
import scalatags.JsDom.svgAttrs.{fontFamily, fontSize, fontWeight, xmlns}
import scalatags.JsDom.svgTags.{svg, text}
import smile.infrastructure.{FontFamily, JSTextMetrics}

import scala.scalajs.js
import scala.util.control.NonFatal

private[smile] object JSBoundaryCalculator:

  /** The metrics of texts measured so far, for `smile.modeling.TextLayout`. Each measurement
    * appends a hidden SVG to the document and forces a layout, which made a moving label cost a
    * layout per frame.
    *
    * Unlike on the JVM, the metrics can change under a running page. A web font that is still
    * loading is measured in its fallback font, and lays out wider or narrower once it arrives. So
    * nothing is cached or reused while the document's fonts are loading, and everything is
    * forgotten whenever a round of font loading finishes (see `fontsSettled`).
    */
  private[smile] val textMetricsMeasurements =
    new TextMeasurementCache[TextMetrics](TextMeasurementCache.DefaultCapacity)

  /** The metrics of a line of text: its layout box, as the browser's SVG layout measures it, and
    * the box around its glyphs' ink and the baseline drop from one `measureText` of the canvas,
    * both boxes in the frame where the middle of the layout box is the origin. Where there is no
    * SVG layout, that measurement gives the layout box too, and where the environment cannot
    * measure the ink, the visual box is left out and the layout box alone stands for the text.
    */
  def textMetrics(content: String, font: String, size: Double, weight: Int): TextMetrics =
    def measured =
      val line = JSTextMetrics.measured(content, font, size, weight)
      // Where there is no layout engine for SVG text, as in jsdom, the canvas measures the line.
      val (width, height) =
        try measure(content, font, size, weight)
        catch case NonFatal(error) => line.map(_.lineBox(size)).getOrElse(throw error)
      TextMetrics(width, height, line.flatMap(_.inkBox), line.flatMap(_.baselineDrop))
    if fontsSettled then textMetricsMeasurements.getOrMeasure(content, font, size, weight)(measured)
    else measured

  /** Measures the glyph box of a text in the browser's layout engine. */
  private def measure(
      content: String,
      font: String,
      size: Double,
      weight: Int
  ): TextMeasurementCache.Dimensions =
    val mySvg = svg(
      xmlns := "http://www.w3.org/2000/svg",
      text(
        fontFamily := FontFamily.css(font),
        fontWeight := weight,
        fontSize   := size,
        content
      )
    ).render
    mySvg.setAttribute("visibility", "hidden")
    val _ = document.body.appendChild(mySvg)

    // The measurement throws in a JS environment without a layout engine, which has no getBBox.
    // A text's bounds are a lazy val, so a failed initializer runs again on the next access, and
    // a probe node left in the document would accumulate one per attempt. A failed measurement
    // is not cached either, so the next attempt measures again.
    val boundingRect =
      try mySvg.asInstanceOf[dom.SVGLocatable].getBBox()
      finally
        val _ = document.body.removeChild(mySvg)

    (boundingRect.width, boundingRect.height)

  /** Whether the document's fonts are settled enough for a measurement to stay valid. True when
    * `document.fonts` reports `"loaded"`, and in an environment with no `document.fonts` such as
    * jsdom, where no font will ever arrive to change the metrics.
    */
  private[smile] def fontsSettled: Boolean =
    documentFonts match
      case None        => true
      case Some(fonts) =>
        clearOnFontLoads
        "loaded".equals(fonts.status)

  /** The document's `FontFaceSet`, where the environment has one. */
  private lazy val documentFonts: Option[js.Dynamic] =
    val fonts = document.asInstanceOf[js.Dynamic].fonts
    if js.isUndefined(fonts) || fonts == null then None else Some(fonts)

  /** Forgets every measurement each time a round of font loading finishes, since a font that
    * arrived may be one a cached text was measured without. Registered once, the first time it is
    * needed.
    */
  private lazy val clearOnFontLoads: Unit =
    documentFonts.foreach: fonts =>
      fonts.addEventListener("loadingdone", (_: js.Any) => textMetricsMeasurements.clear())
