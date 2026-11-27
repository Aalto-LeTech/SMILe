package smile.modeling

/** Sines and cosines of angles given in degrees, exact at the multiples of 90. */
private[smile] object Angles:

  /** The degrees in a full turn. */
  private[smile] val FullTurn: Double = 360.0

  /** The sine and cosine of `degrees`, in that order. The angle is wrapped into [0, 360) first, so
    * 450, 720 and -360 are as exact as 90, 0 and 0. A right angle has a sine and a cosine of
    * exactly 0, 1 or -1, where `math.cos(math.toRadians(90))` is `6e-17`.
    *
    * Positive angles are the library's counterclockwise ones. The sine is the mathematical one, so
    * a positive sine is above the x axis, where y on screen is negative.
    */
  def exactSinCos(degrees: Double): (Double, Double) =
    val wrapped = degrees % FullTurn
    val inRange = if wrapped < 0 then wrapped + FullTurn else wrapped
    // A tiny negative angle wraps to exactly 360, which is zero again.
    val angle = if inRange >= FullTurn then 0.0 else inRange
    angle match
      case 0.0   => (0.0, 1.0)
      case 90.0  => (1.0, 0.0)
      case 180.0 => (0.0, -1.0)
      case 270.0 => (-1.0, 0.0)
      case _     =>
        val radians = math.toRadians(angle)
        (math.sin(radians), math.cos(radians))
end Angles
