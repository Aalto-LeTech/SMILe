package smile.testing

/** What the shared rasterization assertions need to know about the platform's rasterizer. The JVM
  * tests have their own object of this name.
  */
object Rasterizer:

  /** How far outside an element's boundary, in pixels, the rasterizer's own approximation of a
    * shape may put ink.
    *
    * Skia, which draws for Chromium and for jsdom's canvas, draws ellipses, round caps and round
    * joins as chains of conic sections and places geometry on a subpixel grid. A curve whose true
    * extreme lies on a pixel's edge, or within a few hundredths of a pixel inside it, shades the
    * next pixel. For the arcs among this library's test fixtures that is five steps of alpha out of
    * 255, and somewhat more for a round join. A sixteenth of a pixel of slack covers it, and
    * matters only where a boundary's edge falls that close to a pixel's.
    */
  val edgeSlack: Double = 1.0 / 16

  /** How far apart, of 255, a channel may be where a scaled and turned bitmap is drawn in one go
    * and where it is replayed step by step. The two resample differently. Chromium and Java2D stay
    * within 10. WebKit smooths the crests of the test's wave more, by up to 18. There 140 of 3125
    * pixels were 13 to 18 apart and 1383 under 4, where a picture drawn half a pixel off would be
    * 10 apart all along the slopes.
    */
  val replayDifference: Int = if BrowserCapabilities.isWebKit then 18 else 12

  /** The scaling method whose buffer results the stepwise replay suite compares. A canvas has no
    * area averaging, so it is smooth scaling here.
    */
  val replayScaling: smile.ScalingMethod = smile.ScalingMethod.Smooth

  /** Whether buffers can be scaled by averaging areas, as SMILe 1 scaled them on the JVM. A canvas
    * cannot.
    */
  val averagesAreas: Boolean = false

  /** How many levels a channel of a half transparent pixel may move when its bitmap is drawn. A
    * canvas stores colors multiplied by their alpha, so drawing them loses up to two levels.
    */
  val halfTransparentRounding: Int = 2

  /** How many levels a channel of an antialiased edge pixel, multiplied by its alpha, may move when
    * the geometry moves by a step of the grid of 2^-20 pixel that every move lies on.
    *
    * Skia samples coverage on a subpixel grid, and an edge that lies exactly on a sample's
    * boundary, as the edges of shapes at whole or dyadic positions do, flips that sample when it
    * moves by a grid step. In Chromium that measured up to 4 levels at 1x and 18 at 1.5x, about one
    * of the 16 samples of a pixel.
    */
  val gridStepLevels: Int = 20
