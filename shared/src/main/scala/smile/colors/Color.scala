package smile.colors

import smile.infrastructure.Constants.*
import smile.modeling.Angles

/** Utility methods for creating and converting colors in various color spaces and formats,
  * including HSI (Hue, Saturation, Intensity) and RGBA (Red, Green, Blue, Alpha), and for
  * lightening or darkening colors.
  *
  * @see
  *   [[java.awt.Color]]
  */
object Color:

  /** The hue [[smile.colors.Color.rgbToHSI]] reports for a gray, which has no hue of its own. It is
    * zero, the start of the color circle. [[smile.colors.Color.fromHSI]] and
    * [[smile.colors.Color.hsiToRGB]] reject a NaN hue.
    */
  val GrayHSIHue: Double = 0.0

  /** Creates a `Color` instance from HSI values with optional opacity. Validates the input values
    * for HSI and opacity before conversion.
    *
    * @param hue
    *   The hue component of the color, in degrees [0, 360).
    * @param saturation
    *   The saturation component of the color, in the range [0, 1].
    * @param intensity
    *   The intensity component of the color, in the range [0, 255].
    * @param opacity
    *   The opacity of the color, in the range [0, 255]. Default is 255.
    */
  def fromHSI(
      hue: Double,
      saturation: Double,
      intensity: Double,
      opacity: Int = MaximumOpacity
  ): Color =
    validateHSI(hue, saturation, intensity, opacity)
    val (r, g, b) = hsiToRGB(hue, saturation, intensity)
    new Color(r, g, b, opacity)

  /** Converts RGB values to HSI (Hue, Saturation, Intensity). Validates the input RGB values before
    * conversion.
    *
    * @param red
    *   The red component, in the range [0, 255].
    * @param green
    *   The green component, in the range [0, 255].
    * @param blue
    *   The blue component, in the range [0, 255].
    * @return
    *   A tuple containing the HSI components: (hue, saturation, intensity). A gray has no hue of
    *   its own, so its hue is reported as [[smile.colors.Color.GrayHSIHue]] and its saturation as
    *   zero. [[smile.colors.Color.fromHSI]] and [[smile.colors.Color.hsiToRGB]] accept every
    *   returned triple and ignore the hue when saturation is zero.
    */
  def rgbToHSI(red: Int, green: Int, blue: Int): (Double, Double, Double) =
    validateRGBA(red, green, blue)

    val rgbSum: Double = red + green + blue

    val hue: Double =
      if isGray(red, green, blue) then GrayHSIHue
      else
        val redMinusGreen = red - green
        val redMinusBlue  = red - blue

        val root = Math.sqrt(redMinusGreen * redMinusGreen + redMinusBlue * (green - blue))

        // Rounding can carry the cosine a hair past one, which acos answers with NaN.
        val cosine         = ((redMinusGreen + redMinusBlue) / (2.0 * root)).max(-1.0).min(1.0)
        val angleCandidate = Math.toDegrees(Math.acos(cosine))

        if green >= blue then angleCandidate
        else Angles.FullTurn - angleCandidate

    val saturation: Double =
      if isBlack(red, green, blue) then MinimumHSISaturation
      else 1.0 - 3.0 * (min(red, green, blue) / rgbSum)

    val intensity = rgbSum / 3.0

    (hue, saturation, intensity)
  end rgbToHSI

  /** Converts HSI values to RGB. Validates the input HSI values before conversion.
    *
    * @param hueInDegrees
    *   The hue component of the color, in degrees [0, 360).
    * @param saturation
    *   The saturation component of the color, in the range [0, 1].
    * @param intensity
    *   The intensity component of the color, in the range [0, 255].
    */
  def hsiToRGB(hueInDegrees: Double, saturation: Double, intensity: Double): (Int, Int, Int) =
    validateHSI(hueInDegrees, saturation, intensity)

    val nHueInDeg = (hueInDegrees % Angles.FullTurn + Angles.FullTurn) % Angles.FullTurn

    // This is the exact inverse of rgbToHSI. Within each 120-degree sector, one channel is
    // I * (1 - S), one follows the hue, and the third takes the remainder of 3 * I.
    val sectorHue = Math.toRadians(nHueInDeg % 120.0)

    val second = intensity * (1 - saturation)
    val first  = intensity *
      (1 + saturation * Math.cos(sectorHue) / Math.cos(Math.toRadians(60) - sectorHue))
    val third = 3 * intensity - first - second

    val (red, green, blue) =
      if nHueInDeg < 120 then (first, third, second)
      else if nHueInDeg < 240 then (second, first, third)
      else (third, second, first)

    def toChannel(value: Double): Int = Math.round(value).toInt.max(0).min(255)

    (toChannel(red), toChannel(green), toChannel(blue))

  /** Creates an ARGB integer value from RGBA components. Validates the RGBA components before
    * conversion.
    *
    * @param red
    *   The red component, in the range [0, 255].
    * @param green
    *   The green component, in the range [0, 255].
    * @param blue
    *   The blue component, in the range [0, 255].
    * @param opacity
    *   The opacity component, in the range [0, 255]. Default is 255.
    */
  def argbIntFrom(red: Int, green: Int, blue: Int, opacity: Int = MaximumOpacity): Int =
    validateRGBA(red, green, blue, opacity)
    (opacity << 24) | (red << 16) | (green << 8) | blue

  def rgbaIntFrom(red: Int, green: Int, blue: Int, opacity: Int = MaximumOpacity): Int =
    validateRGBA(red, green, blue, opacity)
    (red << 24) | (green << 16) | (blue << 8) | opacity

  def fromRgbaInt(rgba: Int): Color = new Color(
    (rgba >> 24) & 0xff,
    (rgba >> 16) & 0xff,
    (rgba >> 8) & 0xff,
    rgba & 0xff
  )

  /** Determines if the specified RGB color is a shade of gray. A color is considered gray if its
    * red, green, and blue components are equal.
    *
    * @param red
    *   The red component of the color, in the range [0, 255].
    * @param green
    *   The green component of the color, in the range [0, 255].
    * @param blue
    *   The blue component of the color, in the range [0, 255].
    */
  def isGray(red: Int, green: Int, blue: Int): Boolean = red == blue && green == blue

  /** Determines if the specified RGB color is black. A color is considered black if its red, green,
    * and blue components are all at their minimum value, 0.
    */
  def isBlack(red: Int, green: Int, blue: Int): Boolean =
    red == MinimumRed && green == MinimumGreen && blue == MinimumBlue

  private def min(red: Int, green: Int, blue: Int): Int = red.min(green).min(blue)

  private def validateRGBA(red: Int, green: Int, blue: Int, opacity: Int = 255): Unit =
    require(
      red >= MinimumRed && red <= MaximumRed,
      s"Red must be between $MinimumRed and $MaximumRed (was: $red)"
    )
    require(
      green >= MinimumGreen && green <= MaximumGreen,
      s"Green must be between $MinimumGreen and $MaximumGreen (was: $green)"
    )
    require(
      blue >= MinimumBlue && blue <= MaximumBlue,
      s"Blue must be between $MinimumBlue and $MaximumBlue (was: $blue)"
    )
    require(
      opacity >= MinimumOpacity && opacity <= MaximumOpacity,
      s"Opacity must be between $MinimumOpacity and $MaximumOpacity (was: $opacity)"
    )

  /** Checks a tinting or shading factor under its own name. An out-of-range factor would otherwise
    * fail with an error about a color channel the caller never passed.
    */
  private def validateFactor(factor: Double, name: String): Unit =
    require(factor >= 0.0 && factor <= 1.0, s"$name must be between 0 and 1 (was: $factor)")

  private def validateHSI(
      hue: Double,
      saturation: Double,
      intensity: Double,
      opacity: Int = 255
  ): Unit =
    require(
      hue >= MinimumHSIHue && hue <= MaximumHSIHue,
      s"Hue must be between $MinimumHSIHue and $MaximumHSIHue"
    )
    require(
      saturation >= MinimumHSISaturation && saturation <= MaximumHSISaturation,
      s"Saturation must be between $MinimumHSISaturation and $MaximumHSISaturation"
    )
    require(
      intensity >= MinimumHSIIntensity && intensity <= MaximumHSIIntensity,
      s"Intensity must be between $MinimumHSIIntensity and $MaximumHSIIntensity"
    )
    require(
      opacity >= MinimumOpacity && opacity <= MaximumOpacity,
      s"Opacity must be between $MinimumOpacity and $MaximumOpacity"
    )

end Color

/** Represents a color with RGB and opacity components. The predefined colors in
  * [[smile.colors.PresetColor]] are plain values of this class.
  *
  * @param red
  *   The red component of the color, in the range [0, 255].
  * @param green
  *   The green component of the color, in the range [0, 255].
  * @param blue
  *   The blue component of the color, in the range [0, 255].
  * @param opacity
  *   The opacity of the color, in the range [0, 255]. Defaults to 255.
  */
final case class Color(
    red: Int,
    green: Int,
    blue: Int,
    opacity: Int = MaximumOpacity
) extends Paint derives CanEqual:
  Color.validateRGBA(red, green, blue, opacity)

  def this(argb: Int) = this(
    (argb >> 16) & 0xff,
    (argb >> 8) & 0xff,
    argb & 0xff,
    (argb >> 24) & 0xff
  )

  /** Checks if the color is fully opaque, with an opacity of 255. */
  def isOpaque: Boolean = opacity == MaximumOpacity

  /** Converts the color to HSI (Hue, Saturation, Intensity) format. */
  def toHSI: (Double, Double, Double) = Color.rgbToHSI(red, green, blue)

  override lazy val averageColor: Color = this

  override private[smile] def paints: Set[Int] = Set(toARGBInt)

  override private[smile] def mapPaints(table: Int => Int): Color = new Color(table(toARGBInt))

  /** Converts the color to an ARGB integer. */
  def toARGBInt: Int =
    (opacity << 24) | (red << 16) | (green << 8) | blue

  /** Converts the color to an RGBA integer. */
  def toRGBAInt: Int =
    (red << 24) | (green << 16) | (blue << 8) | opacity

  private def toRGBInt: Int = (red << 16) | (green << 8) | blue

  /** Lightens the color by a default factor.
    *
    * @see
    *   [[smile.colors.Color.tintByFactor]]
    */
  def lighter: Color = tintByFactor(DefaultTintingFactor)

  /** Darkens the color by a default factor.
    *
    * @see
    *   [[smile.colors.Color.shadeByFactor]]
    */
  def darker: Color = shadeByFactor(DefaultShadingFactor)

  /** Lightens the color by a specified factor.
    *
    * @param tintingFactorFromZeroToOne
    *   The factor to lighten the color by, in the range [0, 1].
    */
  def tintByFactor(tintingFactorFromZeroToOne: Double): Color =
    Color.validateFactor(tintingFactorFromZeroToOne, "Tinting factor")
    val newRed   = (red + tintingFactorFromZeroToOne * (MaximumRed - red)).ceil.toInt
    val newGreen = (green + tintingFactorFromZeroToOne * (MaximumGreen - green)).ceil.toInt
    val newBlue  = (blue + tintingFactorFromZeroToOne * (MaximumBlue - blue)).ceil.toInt

    new Color(newRed, newGreen, newBlue, opacity)
  end tintByFactor

  /** Darkens the color by a specified factor.
    *
    * @param shadingFactorFromZeroToOne
    *   The factor to darken the color by, in the range [0, 1].
    */
  def shadeByFactor(shadingFactorFromZeroToOne: Double): Color =
    Color.validateFactor(shadingFactorFromZeroToOne, "Shading factor")
    val invertedFactor: Double = 1.0 - shadingFactorFromZeroToOne
    val newRed                 = (invertedFactor * red).toInt
    val newGreen               = (invertedFactor * green).toInt
    val newBlue                = (invertedFactor * blue).toInt

    new Color(newRed, newGreen, newBlue, opacity)
  end shadeByFactor

  /** Lightens the color by a specified percentage.
    *
    * ```scala
    * import smile.colors.PresetColor.*
    * import smile.pictures.*
    *
    * val tints   = (0 to 80 by 20).map(percent => Blue.tintByPercentage(percent))
    * val squares = tints.map(tint => Rectangle(30, fillWith(tint), None))
    * val ramp    = squares.reduce[Element]((row, next) => row.addToRight(next))
    * ```
    *
    * ![Blue squares, each lighter than the one before](pictures/ramp.png)
    *
    * @param tintingFactorInPercents
    *   The percentage to lighten the color by, in the range [0, 100].
    */
  def tintByPercentage(tintingFactorInPercents: Double): Color =
    require(
      tintingFactorInPercents >= 0.0 && tintingFactorInPercents <= 100.0,
      "Tinting factor must be between 0 and 100"
    )

    tintByFactor(tintingFactorInPercents / 100.0)
  end tintByPercentage

  /** Darkens the color by a specified percentage.
    *
    * @param shadingFactorInPercents
    *   The percentage to darken the color by, in the range [0, 100].
    */
  def shadeByPercentage(shadingFactorInPercents: Double): Color =
    require(
      shadingFactorInPercents >= 0.0 && shadingFactorInPercents <= 100.0,
      "Shading factor must be between 0 and 100"
    )

    shadeByFactor(shadingFactorInPercents / 100.0)
  end shadeByPercentage

  override def toString: String =
    if opacity == MaximumOpacity then f"#$toRGBInt%06X"
    else f"#$toRGBAInt%08X"
