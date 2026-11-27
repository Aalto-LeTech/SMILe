package smile.animation

/** Stateless timing, in seconds. Phase is measured in cycles, and positive phase starts ahead.
  * Repeat wraps to zero at its boundary. PingPong and sine are continuous at theirs.
  */
object Timing:
  private def cycles(time: Double, period: Double, phase: Double): Double =
    require(time.isFinite && phase.isFinite, "Time and phase must be finite")
    require(period.isFinite && period > 0, "Period must be finite and positive")
    val value = time / period + phase
    require(value.isFinite, "Time divided by period must be finite")
    value

  /** A one-shot transition. Stays at zero before delay, and at one after duration. */
  def once(time: Double, duration: Double, delay: Double = 0): Double =
    require(delay.isFinite, "Delay must be finite")
    Easing.linear(cycles(time - delay, duration, 0))

  def repeat(time: Double, period: Double, phase: Double = 0): Double =
    val value = cycles(time, period, phase)
    value - math.floor(value)

  /** Zero to one and back to zero over one complete period. */
  def pingPong(time: Double, period: Double, phase: Double = 0): Double =
    1 - math.abs(2 * repeat(time, period, phase) - 1)

  /** Smooth zero to one and back to zero over one complete period. */
  def sine(time: Double, period: Double, phase: Double = 0): Double =
    (1 - math.cos(2 * math.Pi * repeat(time, period, phase))) / 2

  /** Wrap a value into [min, max), including negative values. */
  def wrap(value: Double, min: Double, max: Double): Double =
    require(min.isFinite && max.isFinite && max > min, "Wrap bounds must be finite and increasing")
    min + (max - min) * repeat(value - min, max - min)
