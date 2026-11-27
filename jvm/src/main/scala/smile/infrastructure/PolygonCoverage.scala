package smile.infrastructure

import java.awt.image.BufferedImage

/** The antialiased coverage of a convex polygon, computed exactly. Each pixel gets the share of its
  * area that the polygon covers, as a box filter gives it.
  *
  * The JVM renderer masks a turned bitmap's pixels with a coverage like this one (see
  * `smile.infrastructure.JVMPlanRenderer`). Java2D's own antialiased fill would do, but it costs
  * several times more, and a game turns many sprites on every frame. Here each row is split by the
  * polygon's edges into pixels it covers whole, which are left as they are, pixels it misses, which
  * are cleared at once, and the few pixels its edges cross. A pixel that one edge crosses is
  * covered by the share of a square that a line cuts off, which has a closed form, and a pixel that
  * a corner lies in is measured by clipping the pixel's square to the polygon.
  */
private[infrastructure] object PolygonCoverage:
  /** Scales the `width` by `height` pixels at the upper left of `image`, a buffer of premultiplied
    * ARGB, by the share of each pixel that the convex polygon of the corners `xs` and `ys` covers,
    * in their own coordinates. Pixel `(0, 0)` is the unit square at the origin. Pixels the polygon
    * covers whole keep their values, and pixels it misses become transparent whatever they held, so
    * only the pixels it touches need to have been drawn.
    */
  def mask(
      image: BufferedImage,
      width: Int,
      height: Int,
      xs: Array[Double],
      ys: Array[Double]
  ): Unit =
    val edges  = Edges(xs, ys)
    val row    = new Array[Int](width)
    val raster = image.getRaster
    val cell   = new Cell(edges.count)
    var y      = 0
    while y < height do
      var from = 0
      var to   = 0
      if edges.count >= 3 then
        val center = y + 0.5
        // The pixels whose centers lie where every edge keeps the whole pixel inside, and those
        // where no edge keeps it all outside, as ranges of the column of the pixel's left side.
        var fullFrom  = Double.NegativeInfinity
        var fullTo    = Double.PositiveInfinity
        var touchFrom = Double.NegativeInfinity
        var touchTo   = Double.PositiveInfinity
        var i         = 0
        while i < edges.count do
          val nx = edges.nx(i)
          // How far along the normal the edge's line lies from the row's centers at column -0.5.
          val inside  = edges.c(i) - edges.ny(i) * center - edges.half(i)
          val outside = edges.c(i) - edges.ny(i) * center + edges.half(i)
          if nx > 0 then
            fullTo = fullTo.min(inside / nx - 0.5)
            touchTo = touchTo.min(outside / nx - 0.5)
          else if nx < 0 then
            fullFrom = fullFrom.max(inside / nx - 0.5)
            touchFrom = touchFrom.max(outside / nx - 0.5)
          else
            if inside < 0 then fullTo = Double.NegativeInfinity
            if outside <= 0 then touchTo = Double.NegativeInfinity
          i += 1
        from = math.floor(touchFrom).max(0).min(width).toInt
        to = (math.ceil(touchTo) + 1).max(from).min(width).toInt
        if from < to then
          raster.getDataElements(0, y, width, 1, row)
          // The range of whole pixels is narrowed by a small margin, so that a pixel that float
          // error puts on the line is measured.
          val wholeFrom = math.ceil(fullFrom + 1e-9).max(from).toInt
          val wholeTo   = (math.floor(fullTo - 1e-9) + 1).min(to).toInt
          if wholeFrom < wholeTo then
            measure(edges, cell, row, from, wholeFrom, y)
            measure(edges, cell, row, wholeTo, to, y)
          else measure(edges, cell, row, from, to, y)
      java.util.Arrays.fill(row, 0, from, 0)
      java.util.Arrays.fill(row, to, width, 0)
      raster.setDataElements(0, y, width, 1, row)
      y += 1
  end mask

  /** Scales the pixels from column `from` until `to` of `row`, row `y` of the image, by their
    * coverage.
    */
  private def measure(edges: Edges, cell: Cell, row: Array[Int], from: Int, to: Int, y: Int): Unit =
    var x = from
    while x < to do
      val share = coverage(edges, cell, x, y)
      val alpha = (share * 255 + 0.5).toInt.max(0).min(255)
      row(x) =
        if alpha == 255 then row(x)
        else if alpha == 0 then 0
        else
          val argb = row(x)
          // Each premultiplied channel times the coverage, rounded.
          def scaled(shift: Int) = ((((argb >>> shift) & 0xff) * alpha + 127) / 255) << shift
          scaled(24) | scaled(16) | scaled(8) | scaled(0)
      x += 1

  /** The share of the pixel at column `x` and row `y` that the polygon of `edges` covers. */
  private def coverage(edges: Edges, cell: Cell, x: Int, y: Int): Double =
    val cx       = x + 0.5
    val cy       = y + 0.5
    var crossing = -1
    var crossed  = 0
    var outside  = false
    var i        = 0
    while i < edges.count && !outside do
      val d = edges.nx(i) * cx + edges.ny(i) * cy - edges.c(i)
      if d >= edges.half(i) then outside = true
      else if d > -edges.half(i) then
        crossing = i
        crossed += 1
      i += 1
    if outside then 0.0
    else if crossed == 0 then 1.0
    else if crossed == 1 then
      val i = crossing
      cut(edges.nx(i).abs, edges.ny(i).abs, edges.c(i) - edges.nx(i) * cx - edges.ny(i) * cy)
    else cell.clippedArea(edges, x, y)

  /** The share of a unit square on the side of a line with the unit normal `(a, b)`, taken
    * positive, toward which its center lies `u` inside the line, negative where it lies outside.
    */
  private def cut(a: Double, b: Double, u: Double): Double =
    val long  = a.max(b)
    val short = a.min(b)
    val half  = (long + short) / 2
    if u <= -half then 0.0
    else if u >= half then 1.0
    else if u.abs <= (long - short) / 2 then 0.5 + u / long
    else if u > 0 then 1 - (half - u) * (half - u) / (2 * long * short)
    else (half + u) * (half + u) / (2 * long * short)

  /** The edges of a convex polygon as lines: the outward unit normal `(nx, ny)` of each, `c` such
    * that a point `p` lies `nx * p.x + ny * p.y - c` outside the line, and `half`, how far a
    * pixel's corners reach along the normal from its center. Edges of no length are left out.
    */
  private final class Edges(
      val count: Int,
      val nx: Array[Double],
      val ny: Array[Double],
      val c: Array[Double],
      val half: Array[Double]
  )

  private object Edges:
    def apply(xs: Array[Double], ys: Array[Double]): Edges =
      val n     = xs.length
      var twice = 0.0
      var i     = 0
      while i < n do
        val j = if i + 1 == n then 0 else i + 1
        twice += xs(i) * ys(j) - xs(j) * ys(i)
        i += 1
      // Outward is to the right of an edge where the corners run counterclockwise in the
      // coordinates' own orientation, the sense of a positive area, and to its left otherwise.
      val sense                = if twice >= 0 then 1.0 else -1.0
      val (nx, ny, c, halfway) =
        (new Array[Double](n), new Array[Double](n), new Array[Double](n), new Array[Double](n))
      var count = 0
      i = 0
      while i < n do
        val j      = if i + 1 == n then 0 else i + 1
        val dx     = xs(j) - xs(i)
        val dy     = ys(j) - ys(i)
        val length = math.hypot(dx, dy)
        if length > 0 then
          nx(count) = sense * dy / length
          ny(count) = -sense * dx / length
          c(count) = nx(count) * xs(i) + ny(count) * ys(i)
          halfway(count) = (nx(count).abs + ny(count).abs) / 2
          count += 1
        i += 1
      new Edges(if twice == 0 then 0 else count, nx, ny, c, halfway)
  end Edges

  /** Room to clip a pixel's square to a polygon of `edges` edges. A clip to each edge's half-plane
    * adds at most one corner to the convex square.
    */
  private final class Cell(edges: Int):
    private val capacity = 2 * (edges + 4)
    private val ax       = new Array[Double](capacity)
    private val ay       = new Array[Double](capacity)
    private val bx       = new Array[Double](capacity)
    private val by       = new Array[Double](capacity)

    /** The area of the pixel at column `x` and row `y` inside the polygon of `edges`. */
    def clippedArea(edges: Edges, x: Int, y: Int): Double =
      var fromX = ax
      var fromY = ay
      var toX   = bx
      var toY   = by
      fromX(0) = x; fromY(0) = y
      fromX(1) = x + 1; fromY(1) = y
      fromX(2) = x + 1; fromY(2) = y + 1
      fromX(3) = x; fromY(3) = y + 1
      var n = 4
      var e = 0
      while e < edges.count && n >= 3 do
        n = clip(fromX, fromY, n, toX, toY, edges.nx(e), edges.ny(e), edges.c(e))
        val swapX = fromX
        val swapY = fromY
        fromX = toX
        fromY = toY
        toX = swapX
        toY = swapY
        e += 1
      if n < 3 then 0.0 else area(fromX, fromY, n)

    /** Clips the convex polygon of the first `n` corners of `xs` and `ys` to where
      * `nx * x + ny * y <= c`, into `outX` and `outY`, and returns how many corners the result has.
      * Plain loops over doubles, with no tuples or closures, which would box them.
      */
    private def clip(
        xs: Array[Double],
        ys: Array[Double],
        n: Int,
        outX: Array[Double],
        outY: Array[Double],
        nx: Double,
        ny: Double,
        c: Double
    ): Int =
      var count = 0
      var i     = 0
      while i < n && count < capacity - 1 do
        val j  = if i + 1 == n then 0 else i + 1
        val d0 = nx * xs(i) + ny * ys(i) - c
        val d1 = nx * xs(j) + ny * ys(j) - c
        if d0 <= 0 then
          outX(count) = xs(i)
          outY(count) = ys(i)
          count += 1
        if (d0 <= 0) != (d1 <= 0) then
          val t = d0 / (d0 - d1)
          outX(count) = xs(i) + (xs(j) - xs(i)) * t
          outY(count) = ys(i) + (ys(j) - ys(i)) * t
          count += 1
        i += 1
      count
    end clip

    /** The area of the polygon of the first `n` corners of `xs` and `ys`. */
    private def area(xs: Array[Double], ys: Array[Double], n: Int): Double =
      var twice = 0.0
      var i     = 0
      while i < n do
        val j = if i + 1 == n then 0 else i + 1
        twice += xs(i) * ys(j) - xs(j) * ys(i)
        i += 1
      (twice / 2).abs
  end Cell
end PolygonCoverage
