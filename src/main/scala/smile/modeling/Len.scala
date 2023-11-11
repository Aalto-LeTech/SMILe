package smile.modeling

import scala.annotation.targetName
import scala.math
import scala.math.Numeric.Implicits.infixNumericOps

object Len:
  lazy val Zero: Len = Len(0.0)

case class Len(inPixels: Double):
  def floor: Int = inPixels.floor.toInt

  def half: Len = Len(inPixels / 2)

  @targetName("plus")
  def +(other: Len): Len = Len(this.inPixels + other.inPixels)

  @targetName("minus")
  def -(other: Len): Len = Len(this.inPixels - other.inPixels)
