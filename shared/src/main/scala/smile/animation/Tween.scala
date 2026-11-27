package smile.animation

import smile.colors.Color
import smile.modeling.Pos

/** Interpolation uses progress as given, so easing overshoot carries through. Apply an easing
  * function first to clamp raw timing input. Color blends interpolate gamma-encoded sRGB channel
  * values.
  */
object Tween:
  def number(from: Double, to: Double, progress: Double): Double =
    require(from.isFinite && to.isFinite && progress.isFinite, "Values and progress must be finite")
    from * (1 - progress) + to * progress

  def position(from: Pos, to: Pos, progress: Double): Pos =
    Pos(number(from.x, to.x, progress), number(from.y, to.y, progress))

  /** RGBA channels are rounded and clamped to their representable range after interpolation. */
  def color(from: Color, to: Color, progress: Double): Color =
    def channel(a: Int, b: Int): Int = math.round(number(a, b, progress).max(0).min(255)).toInt
    Color(
      channel(from.red, to.red),
      channel(from.green, to.green),
      channel(from.blue, to.blue),
      channel(from.opacity, to.opacity)
    )
