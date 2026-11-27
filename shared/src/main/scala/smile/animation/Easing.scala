package smile.animation

/** Curves take normalized progress. Inputs are clamped to [0, 1]. Back curves deliberately
  * overshoot that range on output. Any `Double => Double` can be used as a custom curve.
  */
object Easing:
  private def unit(progress: Double): Double =
    require(progress.isFinite, "Progress must be finite")
    progress.max(0).min(1)

  def linear(progress: Double): Double = unit(progress)
  def inQuad(progress: Double): Double =
    val t = unit(progress)
    t * t
  def outQuad(progress: Double): Double =
    val t = unit(progress)
    t * (2 - t)
  def inOutQuad(progress: Double): Double =
    val t = unit(progress)
    if t < 0.5 then 2 * t * t else 1 - math.pow(-2 * t + 2, 2) / 2
  def inCubic(progress: Double): Double    = math.pow(unit(progress), 3)
  def outCubic(progress: Double): Double   = 1 - math.pow(1 - unit(progress), 3)
  def inOutCubic(progress: Double): Double =
    val t = unit(progress)
    if t < 0.5 then 4 * t * t * t else 1 - math.pow(-2 * t + 2, 3) / 2
  def inOutSine(progress: Double): Double = (1 - math.cos(math.Pi * unit(progress))) / 2
  def outBack(progress: Double): Double   =
    val t = unit(progress) - 1
    1 + 2.70158 * t * t * t + 1.70158 * t * t
  def outBounce(progress: Double): Double =
    val t = unit(progress)
    if t < 1 / 2.75 then 7.5625 * t * t
    else if t < 2 / 2.75 then 7.5625 * math.pow(t - 1.5 / 2.75, 2) + 0.75
    else if t < 2.5 / 2.75 then 7.5625 * math.pow(t - 2.25 / 2.75, 2) + 0.9375
    else 7.5625 * math.pow(t - 2.625 / 2.75, 2) + 0.984375
