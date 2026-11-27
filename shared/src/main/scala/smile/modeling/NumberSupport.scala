package smile.modeling

/** The number helpers [[smile.modeling.Pos]], [[smile.modeling.Direction]] and
  * [[smile.modeling.Velocity]] are written with.
  *
  * `atLeast` and `atMost` are `max` and `min`, so a NaN stays NaN and `-0.0 atLeast 0.0` is `0.0`,
  * and `clamp` is `atLeast` followed by `atMost`, so a `low` above `high` gives `high`.
  */
private[smile] object NumberSupport:

  extension (value: Double)
    /** `num atLeast limit` is equivalent to `num.max(limit)`. */
    infix def atLeast(minimum: Double): Double = value.max(minimum)

    /** `num atMost limit` is equivalent to `num.min(limit)`. */
    infix def atMost(maximum: Double): Double = value.min(maximum)

    /** `num.clamp(low, high)` is equivalent to `num.max(low).min(high)`. */
    def clamp(low: Double, high: Double): Double = value atLeast low atMost high

  /** A `Double` as the JVM's `Double.toString` writes it, on every platform.
    *
    * `Pos`, `Direction` and `Velocity` print their coordinates with string interpolation, which on
    * the JVM writes `3.0`, `-0.0` and `6.123233995736766E-17`. JavaScript writes the same numbers
    * as `3`, `0` and `6.123233995736766e-17`, so on Scala.js the text would differ. Both platforms
    * pick the shortest digits that read back as the same double. This lays those digits out as the
    * JVM does. That is plain decimals with at least one fraction digit from 10⁻³ up to but not
    * including 10⁷, and `d.dddE±n` outside that range.
    */
  def jvmText(value: Double): String =
    if platformWritesLikeTheJVM then value.toString else relaidAsOnTheJVM(value)

  private val platformWritesLikeTheJVM = 1.0.toString == "1.0"

  /** Rewrites the platform's text for `value` in the JVM's layout. On the JVM it returns the JVM's
    * own text unchanged. Visible for the suites, which check it on both platforms.
    */
  private[smile] def relaidAsOnTheJVM(value: Double): String =
    if value.isNaN then "NaN"
    else if value.isInfinite then (if value > 0 then "Infinity" else "-Infinity")
    else if value == 0.0 then (if 1.0 / value < 0 then "-0.0" else "0.0")
    else
      val sign                 = if value < 0 then "-" else ""
      val (digits, pointIndex) = significantDigits(math.abs(value).toString)
      val magnitude            = math.abs(value)
      if magnitude >= 1e-3 && magnitude < 1e7 then
        if pointIndex <= 0 then sign + "0." + "0" * -pointIndex + digits
        else if pointIndex >= digits.length then
          sign + digits + "0" * (pointIndex - digits.length) + ".0"
        else sign + digits.take(pointIndex) + "." + digits.drop(pointIndex)
      else
        val fraction = if digits.length > 1 then digits.tail else "0"
        s"$sign${digits.head}.${fraction}E${pointIndex - 1}"

  /** The significant digits of a positive, finite, non-zero number's text, without leading or
    * trailing zeros, and the index of the decimal point among them. The number is
    * `0.digits × 10^pointIndex`. Reads both `1.5e-7` and `1.5E-7` and plain decimals.
    */
  private def significantDigits(text: String): (String, Int) =
    val exponentAt           = text.indexWhere(c => c == 'e' || c == 'E')
    val (mantissa, exponent) =
      if exponentAt < 0 then (text, 0)
      else (text.take(exponentAt), text.drop(exponentAt + 1).stripPrefix("+").toInt)
    val pointAt           = mantissa.indexOf('.')
    val (whole, fraction) =
      if pointAt < 0 then (mantissa, "") else (mantissa.take(pointAt), mantissa.drop(pointAt + 1))
    val allDigits    = whole + fraction
    val leadingZeros = allDigits.takeWhile(_ == '0').length
    val digits       = allDigits.drop(leadingZeros).reverse.dropWhile(_ == '0').reverse
    (digits, whole.length + exponent - leadingZeros)
