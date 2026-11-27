package smile.modeling

import smile.infrastructure.GlyphCache

import java.awt.font.FontRenderContext
import java.awt.geom.AffineTransform

/** Measures the boundaries of texts with AWT fonts.
  */
private[smile] object JVMBoundaryCalculator:
  /** The context every measurement and layout of text on the JVM uses. Its fractional metrics make
    * glyphs advance by what the font says, as they do in a browser, so a text is as wide here as
    * the renderer lays it out.
    */
  private[smile] val fontRenderContext = new FontRenderContext(new AffineTransform(), true, true)

  /** The metrics of texts measured so far, for `smile.modeling.TextLayout`. Which font a family
    * resolves to changes only if a program registers a font of its own, which SMILe never does, so
    * a measurement stays valid for as long as it is kept.
    */
  private[smile] val textMetricsMeasurements =
    new TextMeasurementCache[TextMetrics](TextMeasurementCache.DefaultCapacity)

  /** The metrics of a line of text: its layout box, from the font's line metrics and the string's
    * advance, and the visual box around its glyphs' outlines, both in the frame where the middle of
    * the layout box is the origin. The glyphs sit on a baseline that leaves the ascent above and
    * the descent below, as the renderer lays them out.
    */
  def textMetrics(content: String, font: String, size: Double, weight: Int): TextMetrics =
    textMetricsMeasurements.getOrMeasure(content, font, size, weight):
      val awtFont      = GlyphCache.font(font, size, weight)
      val lineMetrics  = awtFont.getLineMetrics(content, fontRenderContext)
      val height       = (lineMetrics.getAscent + lineMetrics.getDescent).toDouble
      val width        = awtFont.getStringBounds(content, fontRenderContext).getWidth
      val baselineDrop = (lineMetrics.getAscent - lineMetrics.getDescent) / 2.0
      val glyphs       = awtFont.createGlyphVector(fontRenderContext, content)
      // The renderer centers the glyphs by their own advance, as GlyphCache lays them out.
      val advance = glyphs.getLogicalBounds.getWidth
      val ink     = glyphs.getVisualBounds
      val visual  = Option.when(!ink.isEmpty):
        (
          ink.getMinX - advance / 2,
          ink.getMinY + baselineDrop,
          ink.getMaxX - advance / 2,
          ink.getMaxY + baselineDrop
        )
      TextMetrics(width, height, visual, Some(baselineDrop))
