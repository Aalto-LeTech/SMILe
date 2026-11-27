package smile.infrastructure

import java.awt.image.BufferedImage

/** The exact coverage the JVM renderer masks turned bitmaps with. Each pixel's share of a convex
  * polygon is checked against a reference sampled finely. Covered pixels are kept and missed ones
  * cleared.
  */
class PolygonCoverageSuite extends munit.FunSuite:

  private val (width, height) = (24, 20)

  /** A pattern of premultiplied opaque pixels, which the mask scales. */
  private def pattern(x: Int, y: Int): Int = 0xff000000 | (x * 9 << 16) | (y * 11 << 8) | 0x40

  /** The share of the pixel at `(x, y)` inside the convex polygon, from 64 by 64 samples. */
  private def sampled(xs: Array[Double], ys: Array[Double], x: Int, y: Int): Double =
    val n      = 64
    val inside =
      for
        i <- 0 until n
        j <- 0 until n
        px = x + (i + 0.5) / n
        py = y + (j + 0.5) / n
        if contains(xs, ys, px, py)
      yield 1
    inside.size.toDouble / (n * n)

  private def contains(xs: Array[Double], ys: Array[Double], px: Double, py: Double): Boolean =
    val sides = xs.indices.map: i =>
      val j = (i + 1) % xs.length
      math.signum((xs(j) - xs(i)) * (py - ys(i)) - (ys(j) - ys(i)) * (px - xs(i)))
    sides.forall(_ >= 0) || sides.forall(_ <= 0)

  /** A buffer of the pattern, larger than the layer it holds, after masking the layer by the
    * polygon.
    */
  private def masked(xs: Array[Double], ys: Array[Double]): BufferedImage =
    val image = new BufferedImage(width + 5, height + 3, BufferedImage.TYPE_INT_ARGB_PRE)
    for x <- 0 until width + 5; y <- 0 until height + 3 do
      image.getRaster.setDataElements(x, y, Array(pattern(x % width, y % height)))
    PolygonCoverage.mask(image, width, height, xs, ys)
    image

  private def pixel(image: BufferedImage, x: Int, y: Int): Int =
    image.getRaster.getDataElements(x, y, null).asInstanceOf[Array[Int]](0)

  private val polygons = Seq(
    "a square turned by 30 degrees" -> {
      val angle   = math.toRadians(30)
      val (c, s)  = (math.cos(angle), math.sin(angle))
      val corners = Seq((0.0, 0.0), (12.0, 0.0), (12.0, 12.0), (0.0, 12.0))
        .map((x, y) => (11.3 + x * c - y * s, 1.7 + x * s + y * c))
      (corners.map(_._1).toArray, corners.map(_._2).toArray)
    },
    "a thin sliver nearly along the rows" -> (
      Array(1.2, 22.6, 22.9, 1.0),
      Array(4.1, 5.3, 6.0, 4.6)
    ),
    "a triangle given clockwise on screen" -> (
      Array(3.5, 20.25, 9.0),
      Array(2.0, 8.5, 18.75)
    ),
    "an upright rectangle between pixels" -> (
      Array(2.5, 15.25, 15.25, 2.5),
      Array(3.75, 3.75, 11.5, 11.5)
    ),
    "a polygon reaching past the pixels" -> (
      Array(-4.0, 30.0, 26.0, -2.0),
      Array(-3.0, 6.0, 25.0, 17.0)
    )
  )

  for (label, (xs, ys)) <- polygons do
    test(s"each pixel takes its share of $label") {
      val image    = masked(xs, ys)
      val problems =
        for
          y <- 0 until height
          x <- 0 until width
          share    = sampled(xs, ys, x, y)
          expected = (share * 255).round.toInt
          got      = pixel(image, x, y) >>> 24
          if (got - expected).abs > 2
        yield s"($x, $y): alpha $got, share ${share * 255}"
      assert(problems.isEmpty, problems.take(10).mkString("; "))
    }

    test(s"$label keeps the pixels it covers whole and clears the ones it misses") {
      val image = masked(xs, ys)
      for y <- 0 until height; x <- 0 until width do
        val share = sampled(xs, ys, x, y)
        if share == 1 then assertEquals(pixel(image, x, y), pattern(x, y), s"($x, $y)")
        if share == 0 then assertEquals(pixel(image, x, y), 0, s"($x, $y)")
      // Outside the layer, nothing changes.
      assertEquals(pixel(image, width + 2, 1), pattern(2, 1))
      assertEquals(pixel(image, 1, height + 1), pattern(1, 1))
    }

  test("a partly covered pixel scales every premultiplied channel by its share") {
    val image = masked(Array(0.0, 10.0, 10.0, 0.0), Array(0.0, 0.0, 10.5, 10.5))
    val argb  = pixel(image, 3, 10)
    val full  = pattern(3, 10)
    def channel(value: Int, shift: Int) = (value >>> shift) & 0xff
    for shift <- Seq(24, 16, 8, 0) do
      assertEquals(channel(argb, shift), (channel(full, shift) * 128 + 127) / 255, s"at $shift")
  }

  test("a polygon of no area clears every pixel") {
    val image = masked(Array(1.0, 5.0, 9.0), Array(1.0, 5.0, 9.0))
    for y <- 0 until height; x <- 0 until width do assertEquals(pixel(image, x, y), 0)
  }
end PolygonCoverageSuite
