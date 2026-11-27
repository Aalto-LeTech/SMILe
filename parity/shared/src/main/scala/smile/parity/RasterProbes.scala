package smile.parity

import smile.colors.Color
import smile.infrastructure.DeviceGrid
import smile.modeling.Pos
import smile.pictures.Element
import smile.testing.Raster

/** Where the catalog's probes fall in a rendering, and whether the rendering meets them. */
object RasterProbes:
  /** The device pixel of a picture's rendering at `scale` that a world position falls into. The
    * rendering covers the whole device pixels the picture's bounds touch.
    */
  def pixelOf(picture: Element, at: Pos, scale: Double = 1.0): (Int, Int) =
    val grid        = DeviceGrid(scale)
    val (left, top) =
      picture.bounds.fold((0, 0))(b => (grid.covering(b).left, grid.covering(b).top))
    ((at.x * scale).floor.toInt - left, (at.y * scale).floor.toInt - top)

  private def channels(argb: Int): (Int, Int, Int, Int) =
    (argb >>> 24, (argb >> 16) & 0xff, (argb >> 8) & 0xff, argb & 0xff)

  /** Checks a rendering at `scale` against the case's probes for that scale, and against its
    * crispness where it asks for that at `scale`.
    *
    * @return
    *   One message per probe the rendering does not meet, and one for a rendering that is not crisp
    *   where it has to be. Empty when it meets them all.
    */
  def probeFailures(parityCase: ParityCase, raster: Raster, scale: Double = 1.0): Seq[String] =
    val picture = parityCase.picture()
    val probed  = parityCase.probes
      .filter(_.scales.contains(scale))
      .flatMap: probe =>
        val (x, y)                             = pixelOf(picture, probe.at, scale)
        val (alpha, red, green, blue)          = channels(raster.at(x, y))
        def shown                              = s"rgba($red, $green, $blue, $alpha)"
        def near(color: Color, tolerance: Int) =
          (alpha - color.opacity).abs <= tolerance && (red - color.red).abs <= tolerance &&
            (green - color.green).abs <= tolerance && (blue - color.blue).abs <= tolerance
        val met = probe.ink match
          case Ink.Empty                  => alpha == 0
          case Ink.Solid(color)           => near(color, 0)
          case Ink.Near(color, tolerance) => near(color, tolerance)
          case Ink.Opaque                 => alpha == 255
        Option.when(!met)(
          s"at ${probe.at} (pixel $x,$y at ${ParityCase.label(scale)}): expected ${probe.ink}, " +
            s"found $shown"
        )
    val blended = parityCase.crisp
      .filter(_.scales.contains(scale))
      .toSeq
      .flatMap: crisp =>
        val allowed = crisp.palette.map(c => (c.opacity, c.red, c.green, c.blue))
        val off     =
          for
            y <- 0 until raster.height
            x <- 0 until raster.width
            pixel = channels(raster(x, y))
            if pixel._1 != 0 && !allowed(pixel)
          yield (x, y)
        Option.when(off.nonEmpty)(
          s"not crisp at ${ParityCase.label(scale)}: ${off.size} pixels are none of the " +
            s"palette's colors, the first at pixel ${off.head._1},${off.head._2}"
        )
    probed ++ blended
