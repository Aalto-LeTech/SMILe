package smile.infrastructure

import scala.annotation.tailrec

/** Provides utility mathematical functions for operations commonly used throughout the library.
  */
object MathUtils:

  /** Divides a length into a number of positions.
    *
    * @param lengthToDivide
    *   The length to divide
    * @param numberOfDivisions
    *   The number of divisions
    * @return
    *   A sequence of positions
    */
  def divideLength(lengthToDivide: Double, numberOfDivisions: Int): Seq[Double] =

    @tailrec
    def divideLengthRec(
        currentDivision: Int,
        numberOfDivisions: Int,
        lengthToDivide: Double,
        resultSeq: Seq[Double]
    ): Seq[Double] =

      if currentDivision >= numberOfDivisions then return resultSeq

      divideLengthRec(
        currentDivision + 1,
        numberOfDivisions,
        lengthToDivide,
        resultSeq :+ ((currentDivision * lengthToDivide) / numberOfDivisions)
      )

    divideLengthRec(0, numberOfDivisions, lengthToDivide, Seq[Double]())

  /** Sorts two numbers in ascending order.
    *
    * @param a
    *   The first number.
    * @param b
    *   The second number.
    * @return
    *   A tuple with the smaller number first and the larger number second.
    */
  def sort(a: Double, b: Double): (Double, Double) =
    if a < b then (a, b) else (b, a)

  def sinCosRads(angleInRadians: Double): (Double, Double) =
    val sin = sinRads(angleInRadians)
    val cos = cosRads(angleInRadians)

    (sin, cos)

  def sinRads(angleInRadians: Double): Double =
    math.sin(angleInRadians)

  def cosRads(angleInRadians: Double): Double =
    math.cos(angleInRadians)

  def acosRads(cos: Double): Double =
    math.acos(cos)

  def cos(angleInDegrees: Double): Double =
    cosRads(math.toRadians(angleInDegrees))

  def sin(angleInDegrees: Double): Double =
    sinRads(math.toRadians(angleInDegrees))

  def acos(cos: Double): Double =
    math.toDegrees(acosRads(cos))
