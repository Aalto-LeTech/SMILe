package smile.testing

/** What the shared rasterization assertions need to know about the platform's rasterizer. The
  * Scala.js tests have their own object of this name.
  */
object Rasterizer:

  /** How far outside an element's boundary, in pixels, the rasterizer's own approximation of a
    * shape may put ink. Java2D, drawing with pure stroke control, needs none. Nothing at all may be
    * painted outside a boundary rounded outward to whole pixels.
    */
  val edgeSlack: Double = 0.0

  /** How far apart, of 255, a channel may be where a scaled and turned bitmap is drawn in one go
    * and where it is replayed step by step. The two resample differently, by up to 10.
    */
  val replayDifference: Int = 12

  /** The scaling method whose buffer results the stepwise replay suite compares. The JVM has area
    * averaging.
    */
  val replayScaling: smile.ScalingMethod = smile.ScalingMethod.AreaAveraging

  /** Whether buffers can be scaled by averaging areas, as SMILe 1 scaled them on the JVM. A canvas
    * cannot.
    */
  val averagesAreas: Boolean = true

  /** How many levels a channel of a half transparent pixel may move when its bitmap is drawn. An
    * ARGB image keeps colors as they are, so none.
    */
  val halfTransparentRounding: Int = 0

  /** How many levels a channel of an antialiased edge pixel, multiplied by its alpha, may move when
    * the geometry moves by a step of the grid of 2^-20 pixel that every move lies on. Java2D's
    * coverage changes by far less than a level, so none.
    */
  val gridStepLevels: Int = 0
