package smile.modeling

/** A bounded, least-recently-used memo of texts' measurements, shared by the platform calculators.
  * A calculator keeps either the full `smile.modeling.TextMetrics` of a text layout or the natural
  * dimensions they are made from.
  *
  * Measuring a text is expensive on both platforms. A browser lays out a hidden SVG, and the JVM
  * derives font metrics. A measurement depends only on the content, font, size and weight, never on
  * where the text is or how it is transformed, so an animated label is measured once for all its
  * frames.
  *
  * The cache holds at most `capacity` entries, evicting the least recently used, so a program
  * generating unboundedly many distinct texts keeps a fixed footprint. Every access is
  * synchronized. The JVM may measure from rendering threads, and in Scala.js the lock is free.
  */
private[smile] final class TextMeasurementCache[V](val capacity: Int):
  import TextMeasurementCache.Key

  require(capacity > 0, s"capacity must be positive: $capacity")

  // An access-ordered LinkedHashMap is an LRU. Reads move an entry to the end, and the eldest is
  // dropped once an insertion goes over capacity. The bound is copied into a local because inside
  // the subclass the names capacity and size would also refer to the map's own members.
  private val entries =
    val bound = capacity
    new java.util.LinkedHashMap[Key, V](16, 0.75f, true):
      override def removeEldestEntry(eldest: java.util.Map.Entry[Key, V]): Boolean =
        this.size() > bound

  /** The cached measurement of the text with this content, font, size and weight, measuring and
    * remembering it if it is not cached.
    *
    * The measurement runs outside the lock, so one slow measurement does not stall other threads.
    * Two threads measuring the same text at once both store the same answer. A measurement that
    * throws is not cached, so it is attempted again next time.
    */
  def getOrMeasure(content: String, font: String, size: Double, weight: Int)(measure: => V): V =
    getOrMeasure(Key(content, font, size, weight))(measure)

  private def get(key: Key): Option[V] = entries.synchronized(Option(entries.get(key)))

  private def put(key: Key, measurement: V): Unit =
    entries.synchronized:
      val _ = entries.put(key, measurement)

  private def getOrMeasure(key: Key)(measure: => V): V =
    get(key).getOrElse:
      val measurement = measure
      put(key, measurement)
      measurement

  /** Forgets every measurement, for when the environment's metrics may have changed. */
  def clear(): Unit = entries.synchronized(entries.clear())

  /** The number of measurements currently kept. */
  def size: Int = entries.synchronized(entries.size())

private[smile] object TextMeasurementCache:
  /** A measured glyph box's width and height in pixels. */
  type Dimensions = (Double, Double)

  /** How many distinct texts the platform calculators remember. Enough for every label of a busy
    * scene, and at a few tens of kilobytes small enough to keep indefinitely.
    */
  val DefaultCapacity = 512

  /** Everything a measurement depends on. The position does not affect the dimensions. */
  private final case class Key(content: String, font: String, size: Double, weight: Int)

/** A line of text as a platform measures it, in the text's local frame, with the middle of its
  * layout box at the origin.
  *
  * @param width
  *   How far the glyphs advance, the width of the layout box.
  * @param height
  *   The font's ascent plus its descent, the height of the layout box.
  * @param visual
  *   The box around the glyphs' ink as left, top, right and bottom, or `None` where the platform
  *   cannot tell or nothing is inked.
  * @param baselineDrop
  *   How far below the middle of the layout box the baseline lies, half of what the font's ascent
  *   exceeds its descent by, or `None` where the platform cannot tell.
  */
private[smile] final case class TextMetrics(
    width: Double,
    height: Double,
    visual: Option[(Double, Double, Double, Double)],
    baselineDrop: Option[Double]
) derives CanEqual
