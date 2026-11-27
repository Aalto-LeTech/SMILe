package smile.modeling

import smile.infrastructure.PlatformSpecific
import smile.modeling.{Bounds, Pos, TextMetrics}

/** A line of text laid out in its local frame, `content` in `font` at `size` pixels and the
  * CSS-style `weight`. The middle of the layout box is the origin, and the glyphs sit on a baseline
  * that leaves the font's ascent above and its descent below, as every backend lays them out.
  *
  * The measurements come from the platform (Java2D on the JVM, the browser in Scala.js) and are
  * remembered by content, font, size and weight, so equal layouts are measured once.
  */
private[smile] final case class TextLayout private (
    content: String,
    font: String,
    size: Double,
    weight: Int
) derives CanEqual:

  private lazy val metrics: TextMetrics =
    PlatformSpecific.textMetrics(content, font, size, weight)

  /** The layout box, as wide as the glyphs advance and as tall as the font's ascent and descent,
    * centered on the origin.
    */
  lazy val layoutBox: Bounds = Bounds(Pos.Origin, metrics.width, metrics.height)

  /** The box around the glyphs' ink, which italics, overhangs and accents may extend past the
    * layout box. Where the platform cannot measure the ink (a JS environment without a canvas) or
    * nothing is inked (an empty or blank text), it is the layout box.
    */
  lazy val visualBox: Bounds =
    metrics.visual match
      case Some((left, top, right, bottom)) => Bounds(left, top, right, bottom)
      case None                             => layoutBox

  /** How far below the middle of the layout box the glyphs' baseline lies, half of what the font's
    * ascent exceeds its descent by. `None` where the platform cannot tell (a JS environment without
    * a canvas), and the renderers then center the line themselves.
    *
    * It comes from the same measurement as the boxes. In a browser, a layout measured while a web
    * font is still loading is measured in the fallback font and keeps that baseline, as it keeps
    * those boxes, until the text is laid out again.
    */
  lazy val baselineDrop: Option[Double] = metrics.baselineDrop
end TextLayout

private[smile] object TextLayout:
  /** The layout of `content` in `font` at `size` pixels and the CSS-style `weight`.
    *
    * @throws IllegalArgumentException
    *   if the size is negative or not finite
    */
  def apply(content: String, font: String, size: Double, weight: Int): TextLayout =
    require(size >= 0 && size.isFinite, s"A text's size must be finite and not negative: $size")
    new TextLayout(content, font, size + 0.0, weight)
