/* .            .           .                   .                 +             .          +      */
/*         +-----------+  +---+    +  +---+  +-----------+  +---+    Media Programming in Scala   */
/*   *     |           |  |    \     /    |  |           | +|   |            Since 2015           */
/*         |   +-------+  |     \   /     |  |   +-------+  |   |   .                        .    */
/*         |   |          |      \ /      |  |   |          |   |         Aalto University        */
/*       . |   +-------+  |   .   V   .   |  |   |   .      |   |      .   Espoo, Finland       . */
/*  +      |           |  |   |\     /|   |  |   |          |   |                  .    +         */
/*         +------+    |  |   | \   / |   |  |   |          |   |    +        *                   */
/*    *           |    |  |   |  \ /  |   |  |   |      *   |   |                     .      +    */
/*      -- +------+    |  |   |   V  *|   |  |   +-------+  |   +-------+ --    .                 */
/*    ---  |           |  |   | .     |   |  |           |  |           |  ---      +      *      */
/*  ------ +-----------+  +---+       +---+  +-----------+  +-----------+ ------               .  */
/*                                                                                     .          */
/*     T H E   S C A L A   M E D I A   C O M P U T A T I O N   L I B R A R Y      .         +     */
/*                                                                                    *           */

package smcl.modeling.d1


import smcl.infrastructure.{CommonDoubleMathOps, FlatMap, ItemItemMap, MathUtils, MinMaxOps}
import smcl.modeling.Len
import smcl.modeling.misc.CartesianPosition




/**
 * Companion object for [[Pos]].
 *
 * @author Aleksi Lukkarinen
 */
object Pos {

  /** The zero of a one-dimensional coordinate system. */
  lazy val Zero = Pos(0.0)

  /** Positive one in a one-dimensional coordinate system. */
  lazy val One = Pos(1.0)

  /** A [[Pos]] instance that represents positive infinity. */
  lazy val PositiveInfinity = Pos(Double.PositiveInfinity)

  /** A [[Pos]] instance that represents negative infinity. */
  lazy val NegativeInfinity = Pos(Double.NegativeInfinity)

  /** A [[Pos]] instance that represents a non-existing position. */
  lazy val NotDefined = apply(0.0, isDefined = false) // TODO: Mutation methods shouldn't do anything

  /**
   * Creates a new [[Pos]] instance.
   *
   * @param valuePixels
   *
   * @return
   */
  def apply(valuePixels: Double): Pos =
    new Pos(valuePixels, isDefined = true)

}




/**
 * Position in a one-dimensional coordinate system.
 *
 * @param inPixels
 * @param isDefined
 *
 * @author Aleksi Lukkarinen
 */
case class Pos private(
    inPixels: Double,
    isDefined: Boolean)
    extends CartesianPosition[Dims]
        with Ordered[Pos]
        with ItemItemMap[Pos, Double]
        with FlatMap[Pos, Double]
        with CommonDoubleMathOps[Pos]
        with MinMaxOps[Pos]
        with HasDims
        with Movable[Pos] {

  /** */
  lazy val coordinates: Seq[Double] = Seq(inPixels)

  /** */
  lazy val dimensions: Dims = Dims.Zero

  /**
   * Returns <code>true</code> if this instance represents
   * the zero position; otherwise <code>false</code>.
   */
  val isZero: Boolean = inPixels == 0.0

  /**
   * Returns <code>true</code> if this instance represents
   * positive infinity; otherwise <code>false</code>.
   */
  val isPositiveInfinity: Boolean =
    inPixels.isPosInfinity

  /**
   * Returns <code>true</code> if this instance represents
   * negative infinity; otherwise <code>false</code>.
   */
  val isNegativeInfinity: Boolean =
    inPixels.isNegInfinity

  /**
   * Returns <code>true</code> if this instance represents
   * a not-a-number; otherwise <code>false</code>.
   */
  val isNaN: Boolean = inPixels.isNaN

  /**
   *
   *
   * @return
   */
  def inPixelsFloored: Int = inPixels.floor.toInt

  /**
   *
   *
   * @param f
   * @tparam ResultType
   *
   * @return
   */
  def toTupleWith[ResultType](
      f: Double => ResultType): (Pos, ResultType) = {

    (this, convertWith(f))
  }

  /**
   *
   *
   * @param f
   * @tparam ResultType
   *
   * @return
   */
  def toTupleFlooredWith[ResultType](
      f: Int => ResultType): (Pos, ResultType) = {

    val fPos = floor

    (fPos, f(fPos.inPixelsFloored))
  }

  /**
   *
   *
   * @param f
   * @tparam ResultType
   *
   * @return
   */
  def convertWith[ResultType](
      f: Double => ResultType): ResultType = {

    f(inPixels)
  }

  /**
   *
   *
   * @param f
   * @tparam ResultType
   *
   * @return
   */
  def convertFlooredWith[ResultType](
      f: Int => ResultType): ResultType = {

    f(inPixelsFloored)
  }

  /**
   *
   *
   * @param offsetsInPixels
   *
   * @return
   */
  override
  def moveBy(offsetsInPixels: Seq[Double]): Pos = {
    require(
      offsetsInPixels.length == NumberOfDimensions,
      s"Exactly $NumberOfDimensions offset must be given (found: ${offsetsInPixels.length})")

    Pos(inPixels + offsetsInPixels.head)
  }

  /**
   *
   *
   * @param offsetInPixels
   *
   * @return
   */
  override
  def moveBy(offsetInPixels: Double): Pos = Pos(inPixels + offsetInPixels)

  /**
   *
   *
   * @param coordinatesInPixels
   *
   * @return
   */
  override
  def moveUpperLeftCornerTo(coordinatesInPixels: Seq[Double]): Pos =
    moveCenterTo(coordinatesInPixels)

  /**
   *
   *
   * @param coordinateInPixels
   *
   * @return
   */
  override
  def moveUpperLeftCornerTo(coordinateInPixels: Double): Pos = Pos(coordinateInPixels)

  /**
   *
   *
   * @param coordinatesInPixels
   *
   * @return
   */
  override
  def moveCenterTo(coordinatesInPixels: Seq[Double]): Pos = {
    require(
      coordinatesInPixels.length == NumberOfDimensions,
      s"Exactly $NumberOfDimensions coordinate must be given (found: ${coordinatesInPixels.length})")

    Pos(coordinatesInPixels.head)
  }

  /**
   *
   *
   * @param coordinateInPixels
   *
   * @return
   */
  override
  def moveCenterTo(coordinateInPixels: Double): Pos = Pos(coordinateInPixels)

  /**
   *
   * @param f
   *
   * @return
   */
  override
  def flatMap(f: (Double) => Pos): Pos = {
    f(inPixels)
  }

  /**
   *
   *
   * @param other
   *
   * @return
   */
  override
  def canEqual(other: Any): Boolean = {
    other.isInstanceOf[Pos]
  }

  /**
   *
   *
   * @param that
   *
   * @return
   */
  override
  def compare(that: Pos): Int = {
    inPixels.compare(that.inPixels)
  }

  /**
   *
   * @param f
   *
   * @return
   */
  override
  def map(f: (Double) => Double): Pos = {
    Pos(f(inPixels))
  }

  /**
   * Returns the minimum of the given objects.
   *
   * @return
   */
  override
  def min(others: Pos*): Pos = {
    (this +: others).minBy(_.inPixels)
  }

  /**
   * Returns the maximum of the given objects.
   *
   * @return
   */
  override
  def max(others: Pos*): Pos = {
    (this +: others).maxBy(_.inPixels)
  }

  /**
   *
   *
   * @param other
   *
   * @return
   */
  def distanceTo(other: Pos): Len = {
    Len(math.abs(other.inPixels - inPixels))
  }

  /**
   *
   *
   * @param other
   *
   * @return
   */
  def toMinMaxWith(other: Pos): (Pos, Pos) = {
    val (min, max) = MathUtils.sort(inPixels, other.inPixels)

    (Pos(min), Pos(max))
  }

  /**
   *
   *
   * @param newValueInPixels
   *
   * @return
   */
  def copy(newValueInPixels: Double = inPixels): Pos = {
    Pos(newValueInPixels)
  }

  /**
   *
   *
   * @return
   */
  override
  def toString: String = {
    s"Pos(x: $inPixels px)"
  }

}
