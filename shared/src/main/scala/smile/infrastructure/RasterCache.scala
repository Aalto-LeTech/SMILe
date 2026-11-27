package smile.infrastructure

import smile.RenderSettings
import smile.modeling.RasterInterpolation
import smile.pictures.PixelSource

/** What a raster kept in a `smile.infrastructure.RasterCache` is of. Keys hold no pixels, and they
  * name what they are of by identity, so a key of a bitmap that is gone pins nothing.
  */
private[smile] enum RasterKey derives CanEqual:

  /** The rendering of the part `region` of a picture source at `density` buffer pixels per logical
    * pixel with `settings`.
    */
  case Rendering(
      source: PixelSource.SourceIdentity,
      region: PixelSource.SourceRegion,
      density: Double,
      settings: RenderSettings
  )

  /** A buffer's pixels, at revision `revision`, scaled by `density` with `interpolation` and
    * `settings`. That is what a bitmap at its own size on an output of that density shows. `pixels`
    * is a token identifying the buffer. Only the JVM caches scaled rasters.
    */
  case Scaled(
      pixels: AnyRef,
      revision: Long,
      density: Double,
      interpolation: RasterInterpolation,
      settings: RenderSettings
  )

/** Rasters computed for keys, kept while their bytes fit in `initialBudget`, at 4 bytes a pixel,
  * and there are no more than `limit` of them. After an insert the least recently used entries go
  * until the rest fit, a hit refreshes its entry, and a raster larger than the budget is given back
  * and not kept. On the JVM the collector may also clear a kept raster before running out of
  * memory, which is then a miss and is dropped when it is looked up or when an insert goes over the
  * budget, before any live entry (see `smile.infrastructure.KeptBuffer`).
  *
  * What is kept never changes what a raster is. Every key is computed the same way whenever it is
  * computed, so a hit and a miss give the same pixels. A raster that left something out, as an SVG
  * document the browser has not decoded yet, is not kept, since computing it later gives more.
  *
  * Every access is synchronized, and a raster is computed outside the lock, because computing one
  * may ask the cache for others. Two threads that miss one key both compute it, and both get the
  * raster that was stored first.
  */
private[smile] final class RasterCache(initialBudget: Long, limit: Int = Int.MaxValue):
  private var budget = initialBudget
  private var bytes  = 0L

  // In access order, so that the eldest entry is the least recently used.
  private val entries =
    new java.util.LinkedHashMap[RasterKey, (KeptBuffer, Long)](16, 0.75f, true)

  /** The raster kept for `key`, without computing it. */
  def peek(key: RasterKey): Option[NativeBuffer] = synchronized(kept(key))

  /** The raster kept for `key`, or the one `compute` gives, kept when it fits in the budget and
    * nothing was left out of it (see `smile.infrastructure.Completeness`).
    */
  def getOrCompute(key: RasterKey)(compute: => NativeBuffer): NativeBuffer =
    peek(key).getOrElse:
      val (computed, whole) = Completeness.tracked(compute)
      synchronized:
        kept(key).getOrElse:
          val size = computed.width.toLong * computed.height * 4
          if whole && size <= budget then
            val _ = entries.put(key, (KeptBuffer(computed), size))
            bytes += size
            fit()
          computed

  /** `body` run with nothing kept past `limit` bytes, for tests. What does not fit goes at once,
    * and the budget comes back afterwards.
    */
  private[smile] def withBudget[A](limit: Long)(body: => A): A =
    val previous = synchronized:
      val previous = budget
      budget = limit
      fit()
      previous
    try body
    finally synchronized { budget = previous }

  /** How many bytes the rasters kept may take. */
  private[smile] def budgetBytes: Long = synchronized(budget)

  /** How many bytes the kept rasters take, which tests read. */
  private[smile] def keptBytes: Long = synchronized(bytes)

  /** How many rasters are kept, which tests read. */
  private[smile] def keptCount: Int = synchronized(entries.size)

  /** The raster kept for `key`, dropping its entry when the collector has cleared it. */
  private def kept(key: RasterKey): Option[NativeBuffer] =
    Option(entries.get(key)).flatMap: (buffer, size) =>
      val raster = buffer.get
      if raster.isEmpty then
        val _ = entries.remove(key)
        bytes -= size
      raster

  /** Once the entries take more than the budget or are more than the limit, drops every entry the
    * collector has cleared, and then the least recently used entries until the rest fit, so that
    * cleared entries never push out live ones. Within the budget and the limit nothing is walked.
    */
  private def fit(): Unit =
    def over = bytes > budget || entries.size > limit
    if over then
      drop((buffer, _) => buffer.get.isEmpty)
      drop((_, _) => over)

  /** Drops the entries `gone` accepts, eldest first. */
  private def drop(gone: (KeptBuffer, Long) => Boolean): Unit =
    val eldest = entries.values.iterator
    while eldest.hasNext do
      val (buffer, size) = eldest.next()
      if gone(buffer, size) then
        bytes -= size
        eldest.remove()
end RasterCache

private[smile] object RasterCache:
  /** The cache every bitmap of a picture source keeps its renderings in, and the JVM renderer the
    * scaled rasters of bitmaps drawn at display density.
    */
  val shared: RasterCache =
    new RasterCache(PlatformSpecific.rasterCacheBudget, PlatformSpecific.rasterCacheLimit)
