package smile.modeling

import scala.annotation.targetName

/** Companion object for the `Len` case class, providing a constant for zero length.
  */
object Len:
  /** A `Len` instance representing zero length. */
  lazy val Zero: Len = Len(0.0)

/** Represents a length in a graphical context, measured in pixels.
  */
case class Len(inPixels: Double):
  /** Floors the length to the nearest whole number.
    *
    * @return
    *   The floored value of the length as an integer.
    */
  def floor: Int = inPixels.floor.toInt

  /** Calculates half of the length.
    *
    * @return
    *   A `Len` instance representing half of the original length.
    */
  def half: Len = Len(inPixels / 2)

  @targetName("plus")
  def +(other: Len): Len = Len(this.inPixels + other.inPixels)

  @targetName("minus")
  def -(other: Len): Len = Len(this.inPixels - other.inPixels)
