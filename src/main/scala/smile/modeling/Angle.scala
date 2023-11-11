package smile.modeling

import smile.infrastructure.MathUtils

import scala.annotation.targetName
import scala.collection.Seq

object Angle:
  /** Number of divisions to a full angle to get a radian. */
  val RadianDivisions: Double = 2.0 * math.Pi

  /** Number of divisions to a full angle to get a degree. */
  val DegreeDivisions: Int = 360

  /** Number of divisions to a full angle to get a gradian. */
  val GradianDivisions: Int = 400

  /** Number of degrees in a radian. */
  val RadianInDegrees: Double =
    DegreeDivisions / RadianDivisions

  /** Number of radians in a degree. */
  val DegreeInRadians: Double =
    RadianDivisions / DegreeDivisions

  /** Number of degrees in a gradian. */
  val GradianInDegrees: Double =
    DegreeDivisions / GradianDivisions

  /** Number of gradians in a degree. */
  val DegreeInGradians: Double =
    GradianDivisions / DegreeDivisions

  /** Number of gradians in a radian. */
  val RadianInGradians: Double =
    GradianDivisions / RadianDivisions

  /** Number of radians in a gradian. */
  val GradianInRadians: Double =
    RadianDivisions / GradianDivisions

  /** A full angle in degrees. */
  val FullAngleInDegrees: Int = DegreeDivisions

  /** A full angle in radians. */
  val FullAngleInRadians: Double = RadianDivisions

  /** A full angle in gradians. */
  val FullAngleInGradians: Double = GradianDivisions

  /** A full angle. */
  val FullAngle: Angle = Angle(FullAngleInDegrees)

  /** A full turn (i.e., a full angle). */
  val FullTurn: Angle = FullAngle

  private def divideCircleInDegsInto(numberOfDivisions: Int): Seq[Double] =
    MathUtils.divideLength(FullAngleInDegrees, numberOfDivisions)

  private def divideCircleInRadsInto(numberOfDivisions: Int): Seq[Double] =
    MathUtils.divideLength(FullAngleInRadians, numberOfDivisions)

  private def divideCircleInGradsInto(numberOfDivisions: Int): Seq[Double] =
    MathUtils.divideLength(FullAngleInGradians, numberOfDivisions)

  /** Number of divisions to a full angle to get a straight angle. */
  val StraightAngleDivisions: Int = 2

  /** A straight angle in degrees. */
  val StraightAngleInDegrees: Int =
    FullAngleInDegrees / StraightAngleDivisions

  /** A straight angle in radians. */
  val StraightAngleInRadians: Double = math.Pi

  /** A straight angle in gradians. */
  val StraightAngleInGradians: Double =
    FullAngleInGradians / StraightAngleDivisions

  /** A straight angle. */
  val StraightAngle: Angle = Angle(StraightAngleInDegrees)

  /** A half turn (i.e., a straight angle). */
  val HalfTurn: Angle = StraightAngle

  /** Angles resulting from division of a full circle to half-turns, measured in degrees. */
  lazy val HalfTurnAnglesInDegrees: Seq[Double] =
    divideCircleInDegsInto(StraightAngleDivisions)

  /** Angles resulting from division of a full circle to half-turns, measured in radians. */
  lazy val HalfTurnAnglesInRadians: Seq[Double] =
    divideCircleInRadsInto(StraightAngleDivisions)

  /** Angles resulting from division of a full circle to half-turns, measured in gradians. */
  lazy val HalfTurnAnglesInGradians: Seq[Double] =
    divideCircleInGradsInto(StraightAngleDivisions)

  /** Angles resulting from division of a full circle to half-turns, given as [[Angle]] instances.
    */
  lazy val HalfTurnAngles: Seq[Angle] =
    HalfTurnAnglesInDegrees.map(Angle(_))

  /** Number of divisions to a full angle to get a right angle. */
  val RightAngleDivisions: Int = 4

  /** A right angle in degrees. */
  val RightAngleInDegrees: Int =
    FullAngleInDegrees / RightAngleDivisions

  /** A right angle in radians. */
  val RightAngleInRadians: Double =
    FullAngleInRadians / RightAngleDivisions.toDouble

  /** A right angle in gradians. */
  val RightAngleInGradians: Double =
    FullAngleInGradians / RightAngleDivisions

  /** A right angle. */
  val RightAngle = Angle(RightAngleInDegrees)

  /** A quadrant (i.e., a right angle). */
  val Quadrant: Angle = RightAngle

  /** A quarter turn (i.e., a right angle). */
  val QuarterTurn: Angle = RightAngle

  /** Angles resulting from division of a full circle to quarter-turns, measured in degrees. */
  lazy val QuarterTurnAnglesInDegrees: Seq[Double] =
    divideCircleInDegsInto(RightAngleDivisions)

  /** Angles resulting from division of a full circle to quarter-turns, measured in radians. */
  lazy val QuarterTurnAnglesInRadians: Seq[Double] =
    divideCircleInRadsInto(RightAngleDivisions)

  /** Angles resulting from division of a full circle to quarter-turns, measured in gradians. */
  lazy val QuarterTurnAnglesInGradians: Seq[Double] =
    divideCircleInGradsInto(RightAngleDivisions)

  /** Angles resulting from division of a full circle to quarter-turns, given as [[Angle]]
    * instances.
    */
  lazy val QuarterTurnAngles: Seq[Angle] =
    QuarterTurnAnglesInDegrees.map(Angle(_))

  /** Number of divisions to a full angle to get a sextant. */
  val SextantDivisions: Int = 6

  /** A sextant in degrees. */
  val SextantInDegrees: Int =
    FullAngleInDegrees / SextantDivisions

  /** A sextant in radians. */
  val SextantInRadians: Double =
    StraightAngleInRadians / SextantDivisions.toDouble

  /** A sextant in gradians. */
  val SextantAngleInGradians: Double =
    FullAngleInGradians / SextantDivisions

  /** A sextant. */
  val Sextant: Angle = Angle(SextantInDegrees)

  /** Angles resulting from division of a full circle to sextants, measured in degrees. */
  lazy val SextantAnglesInDegrees: Seq[Double] =
    divideCircleInDegsInto(SextantDivisions)

  /** Angles resulting from division of a full circle to sextants, measured in radians. */
  lazy val SextantAnglesInRadians: Seq[Double] =
    divideCircleInRadsInto(SextantDivisions)

  /** Angles resulting from division of a full circle to sextants, measured in gradians. */
  lazy val SextantAnglesInGradians: Seq[Double] =
    divideCircleInGradsInto(SextantDivisions)

  /** Angles resulting from division of a full circle to sextants, given as [[Angle]] instances. */
  lazy val SextantAngles: Seq[Angle] =
    SextantAnglesInDegrees.map(Angle(_))

  /** Number of divisions to a full angle to get a clock position angle. */
  val ClockPositionAngleDivisions: Int = 12

  /** A clock position angle in degrees. */
  val ClockPositionAngleInDegrees: Int =
    FullAngleInDegrees / ClockPositionAngleDivisions

  /** A clock position angle in radians. */
  val ClockPositionAngleInRadians: Double =
    FullAngleInRadians / ClockPositionAngleDivisions.toDouble

  /** A clock position angle in gradians. */
  val ClockPositionAngleInGradians: Double =
    FullAngleInGradians / ClockPositionAngleDivisions

  /** A clock position angle. */
  val ClockPositionAngle: Angle = Angle(ClockPositionAngleInDegrees)

  /** Angles resulting from division of a full circle to clock position angles, measured in degrees.
    */
  lazy val ClockPositionAnglesInDegrees: Seq[Double] =
    divideCircleInDegsInto(ClockPositionAngleDivisions)

  /** Angles resulting from division of a full circle to clock position angles, measured in radians.
    */
  lazy val ClockPositionAnglesInRadians: Seq[Double] =
    divideCircleInRadsInto(ClockPositionAngleDivisions)

  /** Angles resulting from division of a full circle to clock position angles, measured in
    * gradians.
    */
  lazy val ClockPositionAnglesInGradians: Seq[Double] =
    divideCircleInGradsInto(ClockPositionAngleDivisions)

  /** Angles resulting from division of a full circle to clock position angles, given as [[Angle]]
    * instances.
    */
  lazy val ClockPositionAngles: Seq[Angle] =
    ClockPositionAnglesInDegrees.map(Angle(_))

  /** Number of divisions to a full angle to get an hour angle. */
  val HourAngleDivisions: Int = 24

  /** An hour angle in degrees. */
  val HourAngleInDegrees: Int =
    FullAngleInDegrees / HourAngleDivisions

  /** An hour angle in radians. */
  val HourAngleInRadians: Double =
    FullAngleInRadians / HourAngleDivisions.toDouble

  /** An hour angle in gradians. */
  val HourAngleInGradians: Double =
    FullAngleInGradians / HourAngleDivisions

  /** An hour angle. */
  val HourAngle: Angle = Angle(HourAngleInDegrees)

  /** Angles resulting from division of a full circle to hour angles, measured in degrees. */
  lazy val HourAnglesInDegrees: Seq[Double] =
    divideCircleInDegsInto(HourAngleDivisions)

  /** Angles resulting from division of a full circle to hour angles, measured in radians. */
  lazy val HourAnglesInRadians: Seq[Double] =
    divideCircleInRadsInto(HourAngleDivisions)

  /** Angles resulting from division of a full circle to hour angles, measured in gradians. */
  lazy val HourAnglesInGradians: Seq[Double] =
    divideCircleInGradsInto(HourAngleDivisions)

  /** Angles resulting from division of a full circle to hour angles, given as [[Angle]] instances.
    */
  lazy val HourAngles: Seq[Angle] =
    HourAnglesInDegrees.map(Angle(_))

  /** Number of divisions to a full angle to get a compass point. */
  val CompassPointDivisions: Int = 32

  /** A compass point in degrees. */
  val CompassPointInDegrees: Int =
    FullAngleInDegrees / CompassPointDivisions

  /** A compass point in radians. */
  val CompassPointInRadians: Double =
    FullAngleInRadians / CompassPointDivisions.toDouble

  /** A compass point in gradians. */
  val CompassPointInGradians: Double =
    FullAngleInGradians / CompassPointDivisions

  /** A compass point. */
  val CompassPoint: Angle = Angle(CompassPointInDegrees)

  /** Angles resulting from division of a full circle to compass points, measured in degrees. */
  lazy val CompassPointAnglesInDegrees: Seq[Double] =
    divideCircleInDegsInto(CompassPointDivisions)

  /** Angles resulting from division of a full circle to compass points, measured in radians. */
  lazy val CompassPointAnglesInRadians: Seq[Double] =
    divideCircleInRadsInto(CompassPointDivisions)

  /** Angles resulting from division of a full circle to compass points, measured in gradians. */
  lazy val CompassPointAnglesInGradians: Seq[Double] =
    divideCircleInGradsInto(CompassPointDivisions)

  /** Angles resulting from division of a full circle to compass points, given as [[Angle]]
    * instances.
    */
  lazy val CompassPointAngles: Seq[Angle] =
    CompassPointAnglesInDegrees.map(Angle(_))

  /** Number of divisions to a full angle to get a binary degree. */
  val BinaryDegreeDivisions: Int = 256

  /** A binary degree in degrees. */
  val BinaryDegreeInDegrees: Int =
    FullAngleInDegrees / BinaryDegreeDivisions

  /** A binary degree in radians. */
  val BinaryDegreeInRadians: Double =
    FullAngleInRadians / BinaryDegreeDivisions.toDouble

  /** A binary degree in gradians. */
  val BinaryDegreeInGradians: Double =
    FullAngleInGradians / BinaryDegreeDivisions

  /** A binary degree. */
  val BinaryDegree: Angle = Angle(BinaryDegreeInDegrees)

  /** Angles resulting from division of a full circle to binary degrees, measured in degrees. */
  lazy val BinaryDegreeAnglesInDegrees: Seq[Double] =
    divideCircleInDegsInto(BinaryDegreeDivisions)

  /** Angles resulting from division of a full circle to binary degrees, measured in radians. */
  lazy val BinaryDegreeAnglesInRadians: Seq[Double] =
    divideCircleInRadsInto(BinaryDegreeDivisions)

  /** Angles resulting from division of a full circle to binary degrees, measured in gradians. */
  lazy val BinaryDegreeAnglesInGradians: Seq[Double] =
    divideCircleInGradsInto(BinaryDegreeDivisions)

  /** Angles resulting from division of a full circle to binary degrees, given as [[Angle]]
    * instances.
    */
  lazy val BinaryDegreeAngles: Seq[Angle] =
    BinaryDegreeAnglesInDegrees.map(Angle(_))

  /** An angle that represents positive infinity. */
  lazy val PositiveInfinity: Angle = Angle(Double.PositiveInfinity)

  /** An angle that represents negative infinity. */
  lazy val NegativeInfinity: Angle = Angle(Double.NegativeInfinity)

  /** An angle that represents a not-a-number. */
  lazy val NaN: Angle = Angle(Double.NaN)

  /** A zero angle. */
  lazy val Zero: Angle = Angle(0.0)

  /** An angle of one degree. */
  lazy val Deg1: Angle = Angle(1.0)

  /** An angle of 10 degrees. */
  lazy val Deg10: Angle = Angle(10.0)

  /** An angle of 20 degrees. */
  lazy val Deg20: Angle = Angle(20.0)

  /** An angle of 30 degrees. */
  lazy val Deg30: Angle = Angle(30.0)

  /** An angle of 1/6 * Pi radians, i.e., 30 degrees. */
  lazy val RadOneSixthPi: Angle = Deg30

  /** An angle of 40 degrees. */
  lazy val Deg40: Angle = Angle(40.0)

  /** An angle of 45 degrees. */
  lazy val Deg45: Angle = Angle(45.0)

  /** An angle of 1/4 * Pi radians, i.e., 45 degrees. */
  lazy val RadOneFourthPi: Angle = Deg45

  /** An angle of 50 degrees. */
  lazy val Deg50: Angle = Angle(50.0)

  /** An angle of 60 degrees. */
  lazy val Deg60: Angle = Angle(60.0)

  /** An angle of 1/3 * Pi radians, i.e., 60 degrees. */
  lazy val RadOneThirdPi: Angle = Deg60

  /** An angle of 70 degrees. */
  lazy val Deg70: Angle = Angle(70.0)

  /** An angle of 80 degrees. */
  lazy val Deg80: Angle = Angle(80.0)

  /** An angle of 90 degrees, i.e., a right angle. */
  lazy val Deg90: Angle = RightAngle

  /** An angle of 1/2 * Pi radians, i.e., a right angle (90 degrees). */
  lazy val RadHalfPi: Angle = RightAngle

  /** An angle of 100 degrees. */
  lazy val Deg100: Angle = Angle(100.0)

  /** An angle of 110 degrees. */
  lazy val Deg110: Angle = Angle(110.0)

  /** An angle of 120 degrees. */
  lazy val Deg120: Angle = Angle(120.0)

  /** An angle of 2/3 * Pi radians, i.e., 120 degrees. */
  lazy val RadTwoThirdsPi: Angle = Deg120

  /** An angle of 130 degrees. */
  lazy val Deg130: Angle = Angle(130.0)

  /** An angle of 45 degrees. */
  lazy val Deg135: Angle = Angle(135.0)

  /** An angle of 3/4 * Pi radians, i.e., 135 degrees. */
  lazy val RadThreeFourthsPi: Angle = Deg135

  /** An angle of 140 degrees. */
  lazy val Deg140: Angle = Angle(140.0)

  /** An angle of 150 degrees. */
  lazy val Deg150: Angle = Angle(150.0)

  /** An angle of 5/6 * Pi radians, i.e., 150 degrees. */
  lazy val RadFiveSixthsPi: Angle = Deg150

  /** An angle of 160 degrees. */
  lazy val Deg160: Angle = Angle(160.0)

  /** An angle of 170 degrees. */
  lazy val Deg170: Angle = Angle(170.0)

  /** An angle of 180 degrees, i.e., a straight angle. */
  lazy val Deg180: Angle = StraightAngle

  /** An angle of Pi radians, i.e., a straight angle (180 degrees). */
  lazy val RadPi: Angle = StraightAngle

  /** An angle of 190 degrees. */
  lazy val Deg190: Angle = Angle(190.0)

  /** An angle of 200 degrees. */
  lazy val Deg200: Angle = Angle(200.0)

  /** An angle of 210 degrees. */
  lazy val Deg210: Angle = Angle(210.0)

  /** An angle of 7/6 * Pi radians, i.e., 210 degrees. */
  lazy val RadSevenSixthsPi: Angle = Deg210

  /** An angle of 220 degrees. */
  lazy val Deg220: Angle = Angle(220.0)

  /** An angle of 255 degrees. */
  lazy val Deg225: Angle = Angle(225.0)

  /** An angle of 5/4 * Pi radians, i.e., 225 degrees. */
  lazy val RadFiveFourthsPi: Angle = Deg225

  /** An angle of 230 degrees. */
  lazy val Deg230: Angle = Angle(230.0)

  /** An angle of 240 degrees. */
  lazy val Deg240: Angle = Angle(240.0)

  /** An angle of 4/3 * Pi radians, i.e., 240 degrees. */
  lazy val RadFourThirdsPi: Angle = Deg240

  /** An angle of 250 degrees. */
  lazy val Deg250: Angle = Angle(250.0)

  /** An angle of 260 degrees. */
  lazy val Deg260: Angle = Angle(260.0)

  /** An angle of 270 degrees. */
  lazy val Deg270: Angle = Angle(270.0)

  /** An angle of 3/2 * Pi radians. */
  lazy val RadThreeHalfsPi: Angle = Deg270

  /** An angle of 280 degrees. */
  lazy val Deg280: Angle = Angle(280.0)

  /** An angle of 290 degrees. */
  lazy val Deg290: Angle = Angle(290.0)

  /** An angle of 300 degrees. */
  lazy val Deg300: Angle = Angle(300.0)

  /** An angle of 5/3 * Pi radians, i.e., 300 degrees. */
  lazy val RadFiveThirdsPi: Angle = Deg300

  /** An angle of 310 degrees. */
  lazy val Deg310: Angle = Angle(310.0)

  /** An angle of 315 degrees. */
  lazy val Deg315: Angle = Angle(315.0)

  /** An angle of 7/4 * Pi radians, i.e., 315 degrees. */
  lazy val RadSevenFourthsPi: Angle = Deg315

  /** An angle of 320 xdegrees. */
  lazy val Deg320: Angle = Angle(320.0)

  /** An angle of 330 degrees. */
  lazy val Deg330: Angle = Angle(330.0)

  /** An angle of 11/6 * Pi radians, i.e., 330 degrees. */
  lazy val RadElevenSixthsPi: Angle = Deg330

  /** An angle of 340 degrees. */
  lazy val Deg340: Angle = Angle(340.0)

  /** An angle of 350 degrees. */
  lazy val Deg350: Angle = Angle(350.0)

  /** An angle of 360 degrees, i.e., a full angle. */
  lazy val Deg360: Angle = FullAngle

  /** An angle of 2 * Pi radians, i.e., a full angle (360 degrees). */
  lazy val RadTwoPi: Angle = FullAngle

class Angle(val inDegrees: Double):
  def this(inRadians: Double, inDegrees: Boolean) =
    this(if inDegrees then inRadians else inRadians * 180 / math.Pi)

  lazy val inRadians: Double = inDegrees * math.Pi / 180

  lazy val sin: Double = MathUtils.sinRads(inRadians)
  lazy val cos: Double = MathUtils.cosRads(inRadians)

  @targetName("divideAngle")
  inline infix def /(divisor: Double): Angle = Angle(inDegrees / divisor)

  override def toString: String = s"$inDegrees°"
