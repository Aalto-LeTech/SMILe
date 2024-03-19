package smile.colors

import smile.modeling.Pos

object LinearGradient:
  /** Defines the cycle methods available for a `LinearGradient`.
    *
    * These cycle methods determine how the gradient pattern repeats (or doesn't) beyond its defined
    * start and end points.
    */
  enum CycleMethod:
    /** The gradient does not repeat; it will display the terminal colors beyond the start and end
      * points.
      */
    case NO_CYCLE

    /** The gradient pattern repeats by mirroring the gradient along the axis perpendicular to the
      * gradient's start and end points.
      */
    case REFLECT

    /** The gradient pattern repeats by cycling the colors from start to end. */
    case REPEAT

/** Represents a linear gradient paint with two or more colors. The start and end points are
  * specified in the local coordinate space of the shape.
  *
  * @param start
  *   The starting position of the gradient.
  * @param end
  *   The ending position of the gradient.
  * @param fractions
  *   A sequence of float values in the range [0.0f, 1.0f] indicating the fractional distances along
  *   the gradient at which color changes occur.
  * @param colors
  *   A sequence of `Color` objects corresponding to each fraction in the gradient.
  * @param cycleMethod
  *   The method used to repeat the gradient pattern if the shape to be filled is larger than the
  *   gradient defined by the start and end points.
  */
class LinearGradient(
    val start: Pos,
    val end: Pos,
    val fractions: Seq[Float],
    val colors: Seq[Color],
    val cycleMethod: LinearGradient.CycleMethod
) extends Paint:
  require(
    fractions.size == colors.size,
    "The number of fractions must be equal to the number of colors"
  )

  def this(
      start: Pos,
      end: Pos,
      startColor: Color,
      endColor: Color,
      cycleMethod: LinearGradient.CycleMethod = LinearGradient.CycleMethod.NO_CYCLE
  ) =
    this(
      start,
      end,
      Seq(0.0f, 1.0f),
      Seq(startColor, endColor),
      cycleMethod
    )

  /** Calculates the average color of the gradient.
    *
    * The average color is calculated by averaging the RGB and opacity values of all colors defined
    * in the gradient.
    *
    * @return
    *   The average `Color` of the gradient.
    */
  lazy val averageColor: Color =
    val r = colors.map(_.red).sum / colors.size
    val g = colors.map(_.green).sum / colors.size
    val b = colors.map(_.blue).sum / colors.size
    val a = colors.map(_.opacity).sum / colors.size
    Color(r, g, b, a)
