package smile.infrastructure

import smile.modeling.JVMBoundaryCalculator

import java.awt.{Font, Rectangle, Shape}
import java.awt.geom.{AffineTransform, PathIterator, Point2D, Rectangle2D}

/** Caches what text rendering derives from a text's content and font alone: the AWT font, and the
  * glyphs' outline laid out around the origin together with the line's measurements. A text drawn
  * every frame is then laid out once.
  *
  * Both caches are bounded, least recently used entries going first, and safe to use from several
  * threads. An entry is computed outside the lock, so two threads that miss at once may both
  * compute it. The results are equal, and whichever is stored last wins.
  */
private[smile] object GlyphCache:

  /** How many fonts and how many laid-out texts are kept. */
  private[smile] val FontCapacity    = 64
  private[smile] val OutlineCapacity = 512

  /** A line of text laid out around the origin, independent of where it is drawn.
    *
    * @param outline
    *   The glyphs' outline, its origin the middle of the natural layout box. It is read-only. It
    *   can be filled, stroked and transformed into a new shape, but not changed.
    * @param advance
    *   How far the glyphs advance, the width of the natural layout box.
    * @param lineHeight
    *   The font's ascent plus its descent, the height of the natural layout box.
    */
  final case class NaturalText(outline: Shape, advance: Double, lineHeight: Double)

  private final case class FontKey(font: String, size: Double, weight: Int)
  private final case class TextKey(text: String, fontKey: FontKey)

  private val fonts    = new BoundedCache[FontKey, Font](FontCapacity)
  private val outlines = new BoundedCache[TextKey, NaturalText](OutlineCapacity)

  /** The AWT font for a font name, size and CSS-style weight, as
    * `smile.infrastructure.AwtConversions.fontToAWT` resolves it.
    */
  def font(font: String, size: Double, weight: Int): Font =
    fonts.getOrCompute(FontKey(font, size, weight))(AwtConversions.fontToAWT(font, size, weight))

  /** The line of text as the font measures it, as wide as the glyphs advance and as tall as the
    * font's ascent and descent. It is laid out around the origin, the glyphs starting from its left
    * edge on a baseline that leaves the ascent above and the descent below. The origin is therefore
    * the middle of the text's natural layout box, and where the glyphs sit does not depend on which
    * ones they are.
    */
  def natural(text: String, font: String, size: Double, weight: Int): NaturalText =
    val fontKey = FontKey(font, size, weight)
    outlines.getOrCompute(TextKey(text, fontKey)):
      val awtFont      = this.font(font, size, weight)
      val context      = JVMBoundaryCalculator.fontRenderContext
      val glyphs       = awtFont.createGlyphVector(context, text)
      val lineMetrics  = awtFont.getLineMetrics(text, context)
      val advance      = glyphs.getLogicalBounds.getWidth
      val lineHeight   = (lineMetrics.getAscent + lineMetrics.getDescent).toDouble
      val baselineDrop = (lineMetrics.getAscent - lineMetrics.getDescent) / 2.0
      val outline      = AffineTransform
        .getTranslateInstance(-advance / 2, baselineDrop)
        .createTransformedShape(glyphs.getOutline)
      NaturalText(new ReadOnlyShape(outline), advance, lineHeight)

  /** How many laid-out texts are currently kept. Used by tests. */
  private[smile] def outlineCount: Int = outlines.size

  /** How many fonts are currently kept. Used by tests. */
  private[smile] def fontCount: Int = fonts.size

  /** Forgets every cached font and outline. Used by tests. */
  private[smile] def clear(): Unit =
    fonts.clear()
    outlines.clear()

  /** A shape that only reads the one it wraps. The outlines AWT builds are `Path2D`s, which anyone
    * holding one could append to. A cached outline is shared by every draw of its text, so it is
    * handed out behind this wrapper.
    */
  private final class ReadOnlyShape(underlying: Shape) extends Shape:
    def getBounds: Rectangle                                            = underlying.getBounds
    def getBounds2D: Rectangle2D                                        = underlying.getBounds2D
    def contains(x: Double, y: Double): Boolean                         = underlying.contains(x, y)
    def contains(p: Point2D): Boolean                                   = underlying.contains(p)
    def intersects(x: Double, y: Double, w: Double, h: Double): Boolean =
      underlying.intersects(x, y, w, h)
    def intersects(r: Rectangle2D): Boolean                           = underlying.intersects(r)
    def contains(x: Double, y: Double, w: Double, h: Double): Boolean =
      underlying.contains(x, y, w, h)
    def contains(r: Rectangle2D): Boolean                  = underlying.contains(r)
    def getPathIterator(at: AffineTransform): PathIterator = underlying.getPathIterator(at)
    def getPathIterator(at: AffineTransform, flatness: Double): PathIterator =
      underlying.getPathIterator(at, flatness)

  /** A least-recently-used map of at most `maximumEntries` entries, guarded by its own lock. */
  private final class BoundedCache[K, V](maximumEntries: Int):
    private val entries =
      new java.util.LinkedHashMap[K, V](16, 0.75f, true):
        override def removeEldestEntry(eldest: java.util.Map.Entry[K, V]): Boolean =
          this.size > maximumEntries

    def getOrCompute(key: K)(compute: => V): V =
      val cached = entries.synchronized(entries.get(key))
      if cached.asInstanceOf[AnyRef] ne null then cached
      else
        val computed = compute
        entries.synchronized { val _ = entries.put(key, computed) }
        computed

    def size: Int     = entries.synchronized(entries.size)
    def clear(): Unit = entries.synchronized(entries.clear())
end GlyphCache
