package aalto.smcl.common


/**
 *
 *
 * @author Aleksi Lukkarinen
 */
object ColorOps {

  /**
   *
   *
   * @param pixelInt
   * @param newRed
   * @return
   */
  @inline
  def withNewRedComponent(pixelInt: Int, newRed: Int): Int = {
    ColorValidator.validateRgbRedComponent(newRed)

    (pixelInt & ~ThirdByte) | (newRed << TwoBytes)
  }

  /**
   *
   *
   * @param pixelInt
   * @param newGreen
   * @return
   */
  @inline
  def withNewGreenComponent(pixelInt: Int, newGreen: Int): Int = {
    ColorValidator.validateRgbGreenComponent(newGreen)

    (pixelInt & ~SecondByte) | (newGreen << OneByte)
  }

  /**
   *
   *
   * @param pixelInt
   * @param newBlue
   * @return
   */
  @inline
  def withNewBlueComponent(pixelInt: Int, newBlue: Int): Int = {
    ColorValidator.validateRgbBlueComponent(newBlue)

    (pixelInt & ~FirstByte) | newBlue
  }

  /**
   *
   *
   * @param pixelInt
   * @param newOpacity
   * @return
   */
  @inline
  def withNewOpacityComponent(pixelInt: Int, newOpacity: Int): Int = {
    ColorValidator.validateRgbaOpacityComponent(newOpacity)

    (pixelInt & ~FourthByte) | (newOpacity << ThreeBytes)
  }

  /**
   *
   *
   * @param pixelInt
   * @return
   */
  @inline
  def redComponentOf(pixelInt: Int): Int =
    (pixelInt & ThirdByte) >>> TwoBytes

  /**
   *
   *
   * @param pixelInt
   * @return
   */
  @inline
  def greenComponentOf(pixelInt: Int): Int =
    (pixelInt & SecondByte) >>> OneByte

  /**
   *
   *
   * @param pixelInt
   * @return
   */
  @inline
  def blueComponentOf(pixelInt: Int): Int =
    pixelInt & FirstByte

  /**
   *
   *
   * @param pixelInt
   * @return
   */
  @inline
  def opacityComponentOf(pixelInt: Int): Int =
    pixelInt >>> ThreeBytes

  /**
   *
   *
   * @param color
   * @return
   */
  @inline
  def rgbaTupleFrom(color: RGBAColor): (Int, Int, Int, Int) =
    (color.red, color.green, color.blue, color.opacity)

  /**
   *
   *
   * @param pixelInt
   * @return
   */
  @inline
  def rgbaTupleFrom(pixelInt: Int): (Int, Int, Int, Int) =
    (redComponentOf(pixelInt),
      greenComponentOf(pixelInt),
      blueComponentOf(pixelInt),
      opacityComponentOf(pixelInt))

  /**
   *
   *
   * @param rgbaTuple
   * @return
   */
  //noinspection ScalaUnnecessaryParentheses
  @inline
  def normalizeRgba(rgbaTuple: (Int, Int, Int, Int)): (Double, Double, Double, Double) =
    (normalizeRgba(_: Int, _: Int, _: Int, _: Int)).tupled.apply(rgbaTuple)

  /**
   *
   *
   * @param red
   * @param green
   * @param blue
   * @return
   */
  @inline
  def normalizeRgba(red: Int, green: Int, blue: Int, opacity: Int): (Double, Double, Double, Double) = {
    ColorValidator.validateRgbaColor(red, green, blue, opacity)

    def rgbSum: Double = (red + green + blue).toDouble

    (red.toDouble / rgbSum,
      green.toDouble / rgbSum,
      blue.toDouble / rgbSum,
      opacity.toDouble / ColorValidator.MaximumRgbaOpacity)
  }

  /**
   *
   *
   * @param color
   * @return
   */
  @inline
  def rgbTupleFrom(color: RGBAColor): (Int, Int, Int) =
    (color.red, color.green, color.blue)

  /**
   *
   *
   * @param pixelInt
   * @return
   */
  @inline
  def rgbTupleFrom(pixelInt: Int): (Int, Int, Int) =
    (redComponentOf(pixelInt),
      greenComponentOf(pixelInt),
      blueComponentOf(pixelInt))

  /**
   *
   *
   * @param rgbTuple
   * @return
   */
  //noinspection ScalaUnnecessaryParentheses
  @inline
  def normalizeRgb(rgbTuple: (Int, Int, Int)): (Double, Double, Double) =
    (normalizeRgb(_: Int, _: Int, _: Int)).tupled.apply(rgbTuple)

  /**
   *
   *
   * @param red
   * @param green
   * @param blue
   * @return
   */
  @inline
  def normalizeRgb(red: Int, green: Int, blue: Int): (Double, Double, Double) = {
    ColorValidator.validateRgbColor(red, green, blue)

    def sum: Double = (red + green + blue).toDouble

    (red.toDouble / sum, green.toDouble / sum, blue.toDouble / sum)
  }

  /**
   *
   *
   * @param red
   * @param green
   * @param blue
   * @param opacity
   * @return
   */
  @inline
  def pixelIntFrom(
    red: Int = ColorValidator.MinimumRgbRed,
    green: Int = ColorValidator.MinimumRgbGreen,
    blue: Int = ColorValidator.MinimumRgbBlue,
    opacity: Int = ColorValidator.MaximumRgbaOpacity): Int = {

    ColorValidator.validateRgbaColor(red, green, blue, opacity)

    (opacity << ThreeBytes) | (red << TwoBytes) | (green << OneByte) | blue
  }

  /**
   *
   *
   * @param rgbTuple
   * @return
   */
  //noinspection ScalaUnnecessaryParentheses
  @inline
  def max(rgbTuple: (Int, Int, Int)): Int =
    (max(_: Int, _: Int, _: Int)).tupled.apply(rgbTuple)

  /**
   *
   *
   * @param red
   * @param green
   * @param blue
   * @return
   */
  @inline
  def max(red: Int, green: Int, blue: Int): Int =
    red.max(green).max(blue)

  /**
   *
   *
   * @param rgbTuple
   * @return
   */
  //noinspection ScalaUnnecessaryParentheses
  @inline
  def min(rgbTuple: (Int, Int, Int)): Int =
    (min(_: Int, _: Int, _: Int)).tupled.apply(rgbTuple)

  /**
   *
   *
   * @param red
   * @param green
   * @param blue
   * @return
   */
  @inline
  def min(red: Int, green: Int, blue: Int): Int =
    red.min(green).min(blue)

  /**
   *
   *
   * @param color
   * @return
   */
  @inline
  def isBlack(color: RGBAColor): Boolean =
    isBlack(rgbTupleFrom(color))

  /**
   *
   *
   * @param pixelInt
   * @return
   */
  @inline
  def isBlack(pixelInt: Int): Boolean =
    isBlack(rgbTupleFrom(pixelInt))

  /**
   *
   *
   * @param rgbTuple
   * @return
   */
  //noinspection ScalaUnnecessaryParentheses
  @inline
  def isBlack(rgbTuple: (Int, Int, Int)): Boolean =
    (isBlack(_: Int, _: Int, _: Int)).tupled.apply(rgbTuple)

  /**
   *
   *
   * @param red
   * @param green
   * @param blue
   * @return
   */
  @inline
  def isBlack(red: Int, green: Int, blue: Int): Boolean =
    red == ColorValidator.MinimumRgbRed.toDouble &&
      green == ColorValidator.MinimumRgbGreen.toDouble &&
      blue == ColorValidator.MinimumRgbBlue.toDouble

  /**
   *
   *
   * @param color
   * @return
   */
  @inline
  def isGray(color: RGBAColor): Boolean =
    isGray(rgbTupleFrom(color))

  /**
   *
   *
   * @param pixelInt
   * @return
   */
  @inline
  def isGray(pixelInt: Int): Boolean =
    isGray(rgbTupleFrom(pixelInt))

  /**
   *
   *
   * @param rgbTuple
   * @return
   */
  //noinspection ScalaUnnecessaryParentheses
  @inline
  def isGray(rgbTuple: (Int, Int, Int)): Boolean =
    (isGray(_: Int, _: Int, _: Int)).tupled.apply(rgbTuple)

  /**
   *
   *
   * @param red
   * @param green
   * @param blue
   * @return
   */
  @inline
  def isGray(red: Int, green: Int, blue: Int): Boolean =
    red == blue && green == blue

  /**
   *
   *
   * @param color
   * @return
   */
  @inline
  def isWhite(color: RGBAColor): Boolean =
    isWhite(rgbTupleFrom(color))

  /**
   *
   *
   * @param pixelInt
   * @return
   */
  @inline
  def isWhite(pixelInt: Int): Boolean =
    isWhite(rgbTupleFrom(pixelInt))

  /**
   *
   *
   * @param rgbTuple
   * @return
   */
  //noinspection ScalaUnnecessaryParentheses
  @inline
  def isWhite(rgbTuple: (Int, Int, Int)): Boolean =
    (isWhite(_: Int, _: Int, _: Int)).tupled.apply(rgbTuple)

  /**
   *
   *
   * @param red
   * @param green
   * @param blue
   * @return
   */
  @inline
  def isWhite(red: Int, green: Int, blue: Int): Boolean =
    red == ColorValidator.MaximumRgbRed.toDouble &&
      green == ColorValidator.MaximumRgbGreen.toDouble &&
      blue == ColorValidator.MaximumRgbBlue.toDouble

  /**
   *
   *
   * @param hueCandidateInDegrees
   * @return
   */
  @inline
  def normalizedHsiHueInDegreesFrom(hueCandidateInDegrees: Double): Double =
    hueCandidateInDegrees % FullCircleInDegrees

  /**
   *
   *
   * @param hueCandidateInDegrees
   * @return
   */
  @inline
  def normalizedHsvHueInDegreesFrom(hueCandidateInDegrees: Double): Double =
    normalizedHsiHueInDegreesFrom(hueCandidateInDegrees)

  /**
   *
   *
   * @param pixelInt
   * @return
   */
  @inline
  def toHsi(pixelInt: Int): (Double, Double, Double) =
    rgbToHsi(rgbTupleFrom(pixelInt))

  /**
   *
   *
   * @param color
   * @return
   */
  @inline
  def toHsi(color: RGBAColor): (Double, Double, Double) =
    rgbToHsi(rgbTupleFrom(color))

  /**
   *
   *
   * @param rgbTuple
   * @return
   */
  //noinspection ScalaUnnecessaryParentheses
  @inline
  def rgbToHsi(rgbTuple: (Int, Int, Int)): (Double, Double, Double) =
    (rgbToHsi(_: Int, _: Int, _: Int)).tupled.apply(rgbTuple)

  /**
   *
   *
   * @param red
   * @param green
   * @param blue
   * @return
   */
  @inline
  def rgbToHsi(red: Int, green: Int, blue: Int): (Double, Double, Double) = {
    import Math._

    ColorValidator.validateRgbColor(red, green, blue)

    val rgbSum: Double = red + green + blue

    val hue: Double =
      if (isGray(red, green, blue)) // Not defined for grays
        ColorValidator.UndefinedHsiHue
      else {
        val RmG = red - green
        val RmB = red - blue

        val root = sqrt(RmG * RmG + RmB * (green - blue))

        val angleCandidate = toDegrees(acos((RmG + RmB) / (2.0 * root)))

        if (green >= blue) angleCandidate else FullCircleInDegrees - angleCandidate
      }

    val saturation: Double =
      if (isBlack(red, green, blue))
        ColorValidator.MinimumHsiSaturation
      else
        1.0 - 3.0 * (ColorOps.min(red, green, blue) / rgbSum)


    val intensity = rgbSum / 3.0

    (hue, saturation, intensity)
  }

  /**
   *
   *
   * @param hueInDegrees
   * @param saturation
   * @param intensity
   * @return
   */
  @inline
  def hsiToColor(hueInDegrees: Double, saturation: Double, intensity: Double): RGBAColor =
    hsiToColor(hueInDegrees, saturation, intensity, ColorValidator.MaximumRgbaOpacity)

  /**
   *
   *
   * @param hueInDegrees
   * @param saturation
   * @param intensity
   * @return
   */
  //noinspection ScalaUnnecessaryParentheses
  @inline
  def hsiToColor(hueInDegrees: Double, saturation: Double, intensity: Double, opacity: Int): RGBAColor =
    (RGBAColor.apply(_: Int, _: Int, _: Int, opacity)).tupled.apply(hsiToRgb(hueInDegrees, saturation, intensity))

  /**
   *
   *
   * @param hueInDegrees
   * @param saturation
   * @param intensity
   * @return
   */
  @inline
  def hsiToPixelInt(hueInDegrees: Double, saturation: Double, intensity: Double): Int =
    hsiToPixelInt(hueInDegrees, saturation, intensity, ColorValidator.MaximumRgbaOpacity)

  /**
   *
   *
   * @param hueInDegrees
   * @param saturation
   * @param intensity
   * @return
   */
  //noinspection ScalaUnnecessaryParentheses
  @inline
  def hsiToPixelInt(hueInDegrees: Double, saturation: Double, intensity: Double, opacity: Int): Int =
    (pixelIntFrom(_: Int, _: Int, _: Int, opacity)).tupled.apply(hsiToRgb(hueInDegrees, saturation, intensity))

  /**
   *
   *
   * @param hsiTuple
   * @return
   */
  //noinspection ScalaUnnecessaryParentheses
  @inline
  def hsiToRgb(hsiTuple: (Double, Double, Double)): (Int, Int, Int) =
    (hsiToRgb(_: Double, _: Double, _: Double)).tupled.apply(hsiTuple)

  /**
   *
   *
   * @param hueInDegrees
   * @param saturation
   * @param intensity
   * @return
   */
  @inline
  def hsiToRgb(hueInDegrees: Double, saturation: Double, intensity: Double): (Int, Int, Int) = {
    import Math._

    ColorValidator.validateHsiColor(hueInDegrees, saturation, intensity)

    // Special case
    if (saturation == ColorValidator.MinimumHsiSaturation) {
      val i: Int = round(intensity).toInt
      return (i, i, i)
    }

    val nHueInDeg = normalizedHsiHueInDegreesFrom(hueInDegrees)

    val (aThirdOfCircleHueInDegrees: Double, finalOrder: ((Int, Int, Int) => (Int, Int, Int))) =
      if (nHueInDeg <= 120.0)
        (nHueInDeg,
          (x: Int, y: Int, z: Int) => (x, z, y))
      else if (nHueInDeg <= 240.0)
        (nHueInDeg - 120.0,
          (x: Int, y: Int, z: Int) => (y, x, z))
      else
        (nHueInDeg - 240.0,
          (x: Int, y: Int, z: Int) => (z, y, x))

    val X = {
      val quotient =
        (saturation * toDegrees(cos(toRadians(aThirdOfCircleHueInDegrees)))) /
          toDegrees(cos(toRadians(60.0 - aThirdOfCircleHueInDegrees)))

      round(intensity * (1 + quotient)).toInt
    }

    val Y = round(intensity - intensity * saturation).toInt

    val Z = round(3.0 * intensity - X - Y).toInt

    val (red, green, blue) = finalOrder(X, Y, Z)

    if (!ColorValidator.rgbRedComponentIsInRange(red) ||
      !ColorValidator.rgbGreenComponentIsInRange(green) ||
      !ColorValidator.rgbBlueComponentIsInRange(blue)) {

      throw new SMCLInvalidHsiValueCombinationError(hueInDegrees, saturation, intensity)
    }

    (red, green, blue)
  }

  /**
   *
   *
   * @param pixelInt
   * @return
   */
  @inline
  def toHsv(pixelInt: Int): (Double, Double, Double) =
    rgbToHsv(rgbTupleFrom(pixelInt))

  /**
   *
   *
   * @param color
   * @return
   */
  @inline
  def toHsv(color: RGBAColor): (Double, Double, Double) =
    rgbToHsv(rgbTupleFrom(color))

  /**
   *
   *
   * @param rgbTuple
   * @return
   */
  //noinspection ScalaUnnecessaryParentheses
  @inline
  def rgbToHsv(rgbTuple: (Int, Int, Int)): (Double, Double, Double) =
    (rgbToHsv(_: Int, _: Int, _: Int)).tupled.apply(rgbTuple)

  /**
   *
   *
   * @param red
   * @param green
   * @param blue
   *
   *
   */
  @inline
  def rgbToHsv(red: Int, green: Int, blue: Int): (Double, Double, Double) = {
    ColorValidator.validateRgbColor(red, green, blue)

    val value: Double = max(red, green, blue)

    val vMinusMinRgb: Double = value - min(red, green, blue)

    val saturation: Double =
      if (isBlack(red, green, blue)) // Defined to be zero for black because the division-by-zero
        ColorValidator.MinimumHsvSaturation
      else
        vMinusMinRgb / value

    val hueInDegrees: Double =
      if (isGray(red, green, blue)) // Not defined for grays
        ColorValidator.UndefinedHsvHue
      else {
        if (value == red) {
          if (green >= blue)
            ((green - blue) / vMinusMinRgb + 0) * 60
          else
            ((red - blue) / vMinusMinRgb + 5) * 60
        }
        else if (value == green)
          ((blue - red) / vMinusMinRgb + 2) * 60
        else // value == blue
          ((red - green) / vMinusMinRgb + 4) * 60
      }

    (hueInDegrees, saturation, value)
  }

  /**
   *
   *
   * @param hue
   * @param saturation
   * @param value
   * @return
   */
  @inline
  def hsvToRgb(hue: Double, saturation: Double, value: Double): (Int, Int, Int) = {
    import Math._

    ColorValidator.validateHsvColor(hue, saturation, value)

    val huePer60 = normalizedHsvHueInDegreesFrom(hue) / 60.0
    val K = floor(huePer60)
    val T = huePer60 - K

    val V: Int = round(value).toInt
    val X: Int = round(value * (1.0 - saturation)).toInt
    val Y: Int = round(value * (1.0 - saturation * T)).toInt
    val Z: Int = round(value * (1.0 - saturation * (1.0 - T))).toInt

    if (K == 0)
      (V, Z, X)
    else if (K == 1)
      (Y, V, X)
    else if (K == 2)
      (X, V, Z)
    else if (K == 3)
      (X, Y, V)
    else if (K == 4)
      (Z, X, V)
    else
      (V, X, Y)
  }

  /**
   *
   *
   * @param color
   * @return
   */
  @inline
  def colorComponentMapFrom(color: RGBAColor): Map[Symbol, Double] =
    colorComponentMapFrom(rgbaTupleFrom(color))

  /**
   *
   *
   * @param pixelInt
   * @return
   */
  @inline
  def colorComponentMapFrom(pixelInt: Int): Map[Symbol, Double] =
    colorComponentMapFrom(rgbaTupleFrom(pixelInt))

  /**
   *
   *
   * @param rgbTuple
   * @return
   */
  //noinspection ScalaUnnecessaryParentheses
  @inline
  def colorComponentMapFrom(rgbTuple: (Int, Int, Int)): Map[Symbol, Double] =
    (colorComponentMapFrom(_: Int, _: Int, _: Int, ColorValidator.MaximumRgbaOpacity)).tupled.apply(rgbTuple)

  /**
   *
   *
   * @param red
   * @param green
   * @param blue
   * @return
   */
  @inline
  def colorComponentMapFrom(red: Int, green: Int, blue: Int): Map[Symbol, Double] =
    colorComponentMapFrom(red, green, blue, ColorValidator.MaximumRgbaOpacity)

  /**
   *
   *
   * @param rgbaTuple
   * @return
   */
  //noinspection ScalaUnnecessaryParentheses
  @inline
  def colorComponentMapFrom(rgbaTuple: (Int, Int, Int, Int)): Map[Symbol, Double] =
    (colorComponentMapFrom(_: Int, _: Int, _: Int, _: Int)).tupled.apply(rgbaTuple)

  /**
   *
   *
   * @param red
   * @param green
   * @param blue
   * @param opacity
   * @return
   */
  @inline
  def colorComponentMapFrom(red: Int, green: Int, blue: Int, opacity: Int): Map[Symbol, Double] = {
    val (hsiHue, hsiSaturation, hsiIntensity) = rgbToHsi(red, green, blue)
    val (hsvHue, hsvSaturation, hsvValue) = rgbToHsv(red, green, blue)

    Map[Symbol, Double](
      'red -> red.toDouble,
      'green -> green.toDouble,
      'blue -> blue.toDouble,
      'opacity -> opacity.toDouble,
      'hsiHue -> hsiHue,
      'hsiSaturation -> hsiSaturation,
      'hsiIntensity -> hsiIntensity,
      'hsvHue -> hsvHue,
      'hsvSaturation -> hsvSaturation,
      'hsvValue -> hsvValue)
  }

}
