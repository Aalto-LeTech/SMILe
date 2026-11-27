package smile.pictures

import smile.colors.Color

/** The heap a bitmap turned on every tick keeps, which needs the JVM's runtime to measure. */
class RasterMemorySuite extends munit.FunSuite:

  /** The heap in use after collecting garbage, the least of a few measurements, since a collection
    * may leave some garbage for the next.
    */
  private def retainedHeap(): Long =
    val runtime = Runtime.getRuntime
    (1 to 5).map { _ =>
      System.gc()
      runtime.totalMemory - runtime.freeMemory
    }.min

  test("a bitmap turned and read 2000 times keeps under a megabyte more of the heap") {
    var ship: Bitmap = Bitmap(40, 30, (x, y) => Color(x * 6, y * 8, 128))
    ship = ship.rotateBy(3)
    val _     = ship.pixels()
    val first = retainedHeap()
    for _ <- 2 to 2000 do
      ship = ship.rotateBy(3)
      val _ = ship.pixels()
    val last = retainedHeap()
    assert(last - first < 1024 * 1024, s"the heap grew from $first to $last bytes")
    assertEquals(ship.source.steps.length, 1)
  }
end RasterMemorySuite
