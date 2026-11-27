package smile.infrastructure

import smile.modeling.{Affine, Bounds}

/** The device pixels a box touches. The box starts at `(left, top)` and is `width` by `height`
  * device pixels in size.
  */
private[smile] final case class DeviceBox(left: Int, top: Int, width: Int, height: Int)
    derives CanEqual

/** The one place where rounding to device pixels happens, shared by every backend.
  *
  * @param scale
  *   The output scale, device pixels per logical unit. Any positive finite number, fractional
  *   included.
  */
private[smile] final case class DeviceGrid(scale: Double) derives CanEqual:
  require(scale > 0 && scale.isFinite, s"The device scale must be positive and finite: $scale")

  /** The logical coordinate `x` moved to the nearest whole device pixel, so that `snap(x) * scale`
    * is a whole number.
    */
  def snap(x: Double): Double = DeviceGrid.snapDevice(x * scale) / scale

  /** The device pixels that the logical box `b` touches. They run from the floor of its minima to
    * the ceil of its maxima, taken in device pixels. Edges within a tiny float error of a whole
    * device pixel count as being on it, so an exact edge never rounds outward.
    */
  def covering(b: Bounds): DeviceBox =
    DeviceGrid.coveringDevice(
      b.upperLeftCorner.x * scale,
      b.upperLeftCorner.y * scale,
      b.lowerRightCorner.x * scale,
      b.lowerRightCorner.y * scale
    )

  /** The map from logical to device coordinates. */
  def toDevice: Affine = Affine.scaling(scale, scale)

private[smile] object DeviceGrid:

  /** How far from a whole device pixel an edge may be and still be on it. An affine keeps its
    * translation on a grid of 2^-20 logical pixels (see [[smile.modeling.Affine]]), so an edge
    * meant to be whole, such as a text fitted to a box by `scaleTo` and a move, can be off by half
    * a grid step, about 5e-7 logical pixels, times the scale. 1e-5 covers that up to a scale of 20
    * and is still far too little to paint.
    */
  private val Epsilon = 1e-5

  /** Values this close to a whole number are taken to be that number. */
  private def settle(v: Double): Double =
    val nearest = math.rint(v)
    if math.abs(v - nearest) < Epsilon then nearest else v

  /** The device coordinate `x` moved to the nearest whole device pixel, `floor(x + 0.5)`. This is
    * the rule of `smile.infrastructure.DeviceGrid.snap` for a coordinate already in device pixels.
    */
  private[smile] def snapDevice(x: Double): Double = math.floor(x + 0.5)

  /** The whole number of device pixels nearest to the device length or coordinate `v`, by the rule
    * of `smile.infrastructure.DeviceGrid.snapDevice`.
    */
  private[smile] def wholeDevice(v: Double): Int = snapDevice(v).toInt

  /** The device pixels that the device box `b` touches, by the rule of
    * `smile.infrastructure.DeviceGrid.covering` for a box already in device pixels.
    */
  private[smile] def coveringDevice(b: Bounds): DeviceBox =
    coveringDevice(
      b.upperLeftCorner.x,
      b.upperLeftCorner.y,
      b.lowerRightCorner.x,
      b.lowerRightCorner.y
    )

  /** The device pixels whose centers lie inside the device box from `left` to `right` and from
    * `top` to `bottom`, edges included at the left and top and left out at the right and bottom.
    * These are the pixels a clip that is not antialiased keeps, as Java2D's do. No pixels where the
    * box is empty or crosses no pixel's center. This is the rule for the clips of images (see
    * `smile.infrastructure.DrawOp.Image`), while a `smile.pictures.Clip` covers every device pixel
    * it touches (see `smile.infrastructure.DeviceGrid.covering`).
    */
  private[smile] def centersCovered(
      left: Double,
      top: Double,
      right: Double,
      bottom: Double
  ): DeviceBox =
    def first(edge: Double): Int = math.ceil(settle(edge - 0.5)).toInt
    val (l, t)                   = (first(left), first(top))
    DeviceBox(l, t, (first(right) - l).max(0), (first(bottom) - t).max(0))

  private def coveringDevice(left: Double, top: Double, right: Double, bottom: Double): DeviceBox =
    val (l, t) = (floorDevice(left), floorDevice(top))
    DeviceBox(l, t, ceilDevice(right) - l, ceilDevice(bottom) - t)

  /** Whether the device length `v` is a whole number of device pixels, within the float error that
    * `smile.infrastructure.DeviceGrid.covering` forgives.
    */
  private[smile] def isWhole(v: Double): Boolean = settle(v) == math.rint(v)

  private def floorDevice(v: Double): Int = math.floor(settle(v)).toInt

  private def ceilDevice(v: Double): Int = math.ceil(settle(v)).toInt
