package smile.infrastructure

import smile.{RenderSettings, ResamplingMode}
import smile.pictures.PixelSource

/** The process-wide raster cache keeps what was computed for a key until its byte budget runs out,
  * and then lets the least recently used entries go first. Each test uses a cache of its own.
  */
class RasterCacheSuite extends munit.FunSuite:

  /** The bytes a 10 by 10 buffer costs. */
  private val tenByTen = 10L * 10 * 4

  private def rendering(
      source: PixelSource.SourceIdentity,
      settings: RenderSettings = RenderSettings.Default
  ): RasterKey =
    RasterKey.Rendering(source, PixelSource.SourceRegion(0, 0, 10, 10), 1.0, settings)

  test("a key asked twice computes once and gives the same buffer") {
    val cache               = new RasterCache(10 * tenByTen)
    val key                 = rendering(new PixelSource.SourceIdentity)
    var calls               = 0
    def ask(): NativeBuffer = cache.getOrCompute(key):
      calls += 1
      BufferAdapter(10, 10)
    val first = ask()
    assert(ask() eq first, "the second ask gives the kept buffer")
    assertEquals(calls, 1)
  }

  test("entries over the budget go, the least recently used first") {
    val cache                             = new RasterCache(3 * tenByTen)
    def key(): RasterKey                  = rendering(new PixelSource.SourceIdentity)
    val (a, b, c, d)                      = (key(), key(), key(), key())
    def put(key: RasterKey): NativeBuffer = cache.getOrCompute(key)(BufferAdapter(10, 10))
    val kept                              = put(a)
    val _                                 = (put(b), put(c))
    assert(cache.peek(a).exists(_ eq kept), "A is kept")
    val _ = put(d)
    assertEquals(cache.peek(b), None, "B, used least recently, is gone")
    assert(Seq(a, c, d).forall(cache.peek(_).isDefined), "A, C and D stay")
    assertEquals(cache.keptCount, 3)
    assert(cache.keptBytes <= 3 * tenByTen, s"kept ${cache.keptBytes} bytes")
  }

  test("entries over the limit go, the least recently used first, whatever their bytes") {
    val cache                             = new RasterCache(100 * tenByTen, limit = 2)
    def key(): RasterKey                  = rendering(new PixelSource.SourceIdentity)
    val (a, b, c)                         = (key(), key(), key())
    def put(key: RasterKey): NativeBuffer = cache.getOrCompute(key)(BufferAdapter(10, 10))
    val _                                 = (put(a), put(b), put(c))
    assertEquals(cache.peek(a), None, "A, used least recently, is gone")
    assert(Seq(b, c).forall(cache.peek(_).isDefined), "B and C stay")
    assertEquals(cache.keptCount, 2)
  }

  test("a buffer larger than the budget is returned and not kept") {
    val cache  = new RasterCache(tenByTen)
    val key    = rendering(new PixelSource.SourceIdentity)
    val large  = BufferAdapter(20, 20)
    val result = cache.getOrCompute(key)(large)
    assert(result eq large)
    assertEquals(cache.keptCount, 0)
    assertEquals(cache.keptBytes, 0L)
  }

  test("peek does not compute") {
    val cache = new RasterCache(10 * tenByTen)
    val key   = rendering(new PixelSource.SourceIdentity)
    assertEquals(cache.peek(key), None)
    assertEquals(cache.keptCount, 0)
  }

  test("different sources and settings do not answer for each other") {
    val cache    = new RasterCache(10 * tenByTen)
    val source   = new PixelSource.SourceIdentity
    val stepwise = RenderSettings(resamplingMode = ResamplingMode.Stepwise)
    val kept     = cache.getOrCompute(rendering(source))(BufferAdapter(10, 10))
    assertEquals(cache.peek(rendering(new PixelSource.SourceIdentity)), None)
    assertEquals(cache.peek(rendering(source, stepwise)), None)
    assert(cache.peek(rendering(source)).exists(_ eq kept))
  }

  test("a smaller budget lets entries go at once, and the old budget comes back after") {
    val cache = new RasterCache(3 * tenByTen)
    val key   = rendering(new PixelSource.SourceIdentity)
    val _     = cache.getOrCompute(key)(BufferAdapter(10, 10))
    cache.withBudget(0):
      assertEquals(cache.peek(key), None)
      val _ = cache.getOrCompute(key)(BufferAdapter(10, 10))
      assertEquals(cache.keptCount, 0)
    val _ = cache.getOrCompute(key)(BufferAdapter(10, 10))
    assertEquals(cache.keptCount, 1)
  }
end RasterCacheSuite
