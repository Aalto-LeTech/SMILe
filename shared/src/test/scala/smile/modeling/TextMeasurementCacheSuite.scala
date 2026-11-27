package smile.modeling

import scala.util.Try

/** The memo both platform calculators keep a text's measured metrics in, keyed by what a
  * measurement depends on: the content, font, size and weight. Each test uses a cache of its own,
  * so nothing here depends on what other suites happen to measure meanwhile.
  */
class TextMeasurementCacheSuite extends munit.FunSuite:

  private type Cache = TextMeasurementCache[TextMeasurementCache.Dimensions]

  /** What `cache` holds for `content` in the label's font, measuring nothing new. */
  private def cached(cache: Cache, content: String = "SMILe", font: String = "sans-serif")(
      size: Double = 20,
      weight: Int = 400
  ): Option[TextMeasurementCache.Dimensions] =
    // A measurement that fails is not remembered, so asking with one leaves the cache as it was.
    scala.util
      .Try(cache.getOrMeasure(content, font, size, weight)(throw new NoSuchElementException))
      .toOption

  test("content, font, size and weight each make a measurement of their own") {
    val cache    = new Cache(8)
    val variants = Seq(
      ("SMILe", "sans-serif", 20.0, 400),
      ("SMILE", "sans-serif", 20.0, 400),
      ("SMILe", "serif", 20.0, 400),
      ("SMILe", "sans-serif", 21.0, 400),
      ("SMILe", "sans-serif", 20.0, 700)
    )
    for ((content, font, size, weight), index) <- variants.zipWithIndex do
      val _ = cache.getOrMeasure(content, font, size, weight)((index.toDouble, index.toDouble))

    assertEquals(cache.size, variants.size)
    for ((content, font, size, weight), index) <- variants.zipWithIndex do
      assertEquals(
        cache.getOrMeasure(content, font, size, weight)((-1.0, -1.0)),
        (index.toDouble, index.toDouble)
      )
  }

  test("the same four are measured once") {
    val cache        = new Cache(8)
    var measurements = 0
    def measure      = cache.getOrMeasure("SMILe", "sans-serif", 20, 400):
      measurements += 1
      (40.0, 12.0)

    for _ <- 1 to 3 do assertEquals(measure, (40.0, 12.0))
    assertEquals(measurements, 1)
  }

  test("the cache keeps at most its capacity, dropping the least recently used") {
    val cache                      = new Cache(3)
    def put(content: String): Unit =
      val _ = cache.getOrMeasure(content, "sans-serif", 20, 400)((1.0, 1.0))

    Seq("a", "b", "c").foreach(put)
    put("a") // "b" is now the least recently used.
    put("d")

    assertEquals(cache.size, 3)
    assertEquals(cached(cache, "b")(), None)
    for content <- Seq("a", "c", "d") do assert(cached(cache, content)().isDefined, content)

    for index <- 0 until 100 do put(s"many $index")
    assertEquals(cache.size, 3)
  }

  test("a measurement that fails is not remembered") {
    val cache   = new Cache(8)
    val failure =
      Try(cache.getOrMeasure("SMILe", "sans-serif", 20, 400)(throw new IllegalStateException("no")))

    assert(failure.isFailure)
    assertEquals(cache.size, 0)
    assertEquals(cache.getOrMeasure("SMILe", "sans-serif", 20, 400)((3.0, 4.0)), (3.0, 4.0))
  }

  test("clearing forgets every measurement") {
    val cache = new Cache(8)
    val _     = cache.getOrMeasure("SMILe", "sans-serif", 20, 400)((1.0, 2.0))
    cache.clear()

    assertEquals(cache.size, 0)
    assertEquals(cached(cache)(), None)
  }
end TextMeasurementCacheSuite
