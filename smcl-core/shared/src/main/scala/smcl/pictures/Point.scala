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

package smcl.pictures


import scala.language.implicitConversions

import smcl.colors.rgb
import smcl.infrastructure.{FlatMap, Identity}
import smcl.modeling.d2.{Bounds, CoordinateTuple, Pos}
import smcl.modeling.{Angle, Len}




/**
 *
 *
 * @author Aleksi Lukkarinen
 */
object Point {

  /**
   *
   *
   * @param position
   * @param color
   */
  def apply(
      position: Pos,
      color: rgb.Color): Point = {

    apply(position, color)
  }

  /**
   *
   *
   * @param xInPixels
   * @param yInPixels
   */
  def apply(
      xInPixels: Double,
      yInPixels: Double,
      color: rgb.Color): Point = {

    new Point(
      Identity(),
      Pos(xInPixels, yInPixels),
      color)
  }

  /**
   *
   *
   * @param p
   *
   * @return
   */
  implicit def asPair(p: Point): (Double, Double) = {
    val pos = p.position

    (pos.xInPixels, pos.yInPixels)
  }

  /**
   *
   *
   * @param p
   *
   * @return
   */
  implicit def asFlooredIntPair(p: Point): (Int, Int) = {
    val pos = p.position

    (pos.xInPixels.floor.toInt, pos.yInPixels.floor.toInt)
  }

}




/**
 *
 *
 * @param identity
 * @param position
 * @param color
 *
 * @author Aleksi Lukkarinen
 */
class Point private(
    override val identity: Identity,
    override val position: Pos,
    val color: rgb.Color)
    extends VectorGraphic
        with FlatMap[Point, (Pos, rgb.Color)] {

  /**
   *
   *
   * @return
   */
  def boundary: Bounds = position.boundary

  /** Tells if this [[Point]] can be rendered on a bitmap. */
  def isRenderable: Boolean = true

  /**
   *
   *
   * @return
   */
  override
  def isPoint: Boolean = true

  /**
   *
   *
   * @param newPosition
   *
   * @return
   */
  def copy(
      newPosition: Pos = position,
      newColor: rgb.Color = color): Point = {

    new Point(identity, newPosition, newColor)
  }

  /**
   *
   *
   * @return
   */
  override
  def toString: String = {
    s"Point(x: ${position.xInPixels} px, y: ${position.yInPixels} px)"
  }

  /**
   * Returns the coordinates of this point as a tuple.
   *
   * @return
   */
  def toCoordinateTuple: CoordinateTuple = {
    (position.xInPixels, position.yInPixels)
  }

  /**
   *
   *
   * @param f
   *
   * @return
   */
  def flatMap(f: ((Pos, rgb.Color)) => Point): Point = {
    f(position, color)
  }

  /**
   *
   *
   * @param offsetsInPixels
   *
   * @return
   */
  override
  def moveBy(offsetsInPixels: Seq[Double]): Point =
    copy(newPosition = position.moveBy(offsetsInPixels))

  /**
   *
   *
   * @param xOffsetInPixels
   * @param yOffsetInPixels
   *
   * @return
   */
  override
  def moveBy(
      xOffsetInPixels: Double,
      yOffsetInPixels: Double): PictureElement = {

    copy(newPosition = position.moveBy(xOffsetInPixels, yOffsetInPixels))
  }

  /**
   *
   *
   * @param coordinatesInPixels
   *
   * @return
   */
  override
  def moveUpperLeftCornerTo(coordinatesInPixels: Seq[Double]): PictureElement =
    copy(newPosition = position.moveUpperLeftCornerTo(coordinatesInPixels))

  /**
   *
   *
   * @param xCoordinateInPixels
   * @param yCoordinateInPixels
   *
   * @return
   */
  override
  def moveUpperLeftCornerTo(
      xCoordinateInPixels: Double,
      yCoordinateInPixels: Double): PictureElement = {

    copy(newPosition = position.moveUpperLeftCornerTo(xCoordinateInPixels, yCoordinateInPixels))
  }

  /**
   *
   *
   * @param coordinatesInPixels
   *
   * @return
   */
  override
  def moveCenterTo(coordinatesInPixels: Seq[Double]): PictureElement =
    copy(newPosition = position.moveCenterTo(coordinatesInPixels))

  /**
   *
   *
   * @param xCoordinateInPixels
   * @param yCoordinateInPixels
   *
   * @return
   */
  override
  def moveCenterTo(
      xCoordinateInPixels: Double,
      yCoordinateInPixels: Double): PictureElement = {

    copy(newPosition = position.moveCenterTo(xCoordinateInPixels, yCoordinateInPixels))
  }

  /**
   *
   *
   * @return
   */
  def isOnOrigo: Boolean = position.isOrigo

  /**
   *
   *
   * @return
   */
  override
  lazy val hashCode: Int = {
    val prime = 31
    var sum = 0

    sum = prime * sum + position.xInPixels.##
    sum = prime * sum + position.yInPixels.##
    sum = prime * sum + color.##

    sum
  }

  /**
   *
   *
   * @param other
   *
   * @return
   */
  def canEqual(other: Any): Boolean = {
    other.isInstanceOf[Point]
  }

  /**
   *
   *
   * @param other
   *
   * @return
   */
  override
  def equals(other: Any): Boolean = {
    other match {
      case that: Point =>
        that.canEqual(this) &&
            that.position == this.position &&
            that.color == this.color

      case _ => false
    }
  }

  /**
   *
   *
   * @param other
   *
   * @return
   */
  def distanceFrom(other: Pos): Len = {
    position.distanceFrom(other)
  }

  /**
   *
   *
   * @param other
   *
   * @return
   */
  def distanceFrom(other: Point): Len = {
    position.distanceFrom(other.position)
  }

  /**
   *
   *
   * @return
   */
  def isOnHorizontalAxis: Boolean = {
    position.isOnHorizontalAxis
  }

  /**
   *
   *
   * @return
   */
  def isOnVerticalAxis: Boolean = {
    position.isOnVerticalAxis
  }

  /**
   *
   *
   * @return
   */
  def isOnFirstQuadrant: Boolean = {
    position.isOnFirstQuadrant
  }

  /**
   *
   *
   * @return
   */
  def isOnSecondQuadrant: Boolean = {
    position.isOnSecondQuadrant
  }

  /**
   *
   *
   * @return
   */
  def isOnThirdQuadrant: Boolean = {
    position.isOnThirdQuadrant
  }

  /**
   *
   *
   * @return
   */
  def isOnFourthQuadrant: Boolean = {
    position.isOnFourthQuadrant
  }

  /**
   *
   */
  override
  def display(): Point = {
    super.display()

    this
  }

  /**
   * Rotates this object around origo (0,0) by 90 degrees clockwise.
   *
   * @return
   */
  override
  def rotateBy90DegsCWAroundOrigo: Point =
    copy(newPosition = position.rotateBy90DegsCWAroundOrigo)

  /**
   * Rotates this object around its center by 90 degrees clockwise.
   *
   * @return
   */
  override
  def rotateBy90DegsCW: Point = this

  /**
   * Rotates this object around a given point by 90 degrees clockwise.
   *
   * @param centerOfRotation
   *
   * @return
   */
  override
  def rotateBy90DegsCW(centerOfRotation: Pos): Point =
    copy(newPosition = position.rotateBy90DegsCW(centerOfRotation))

  /**
   * Rotates this object around origo (0,0) by 90 degrees counterclockwise.
   *
   * @return
   */
  override
  def rotateBy90DegsCCWAroundOrigo: Point =
    copy(newPosition = position.rotateBy90DegsCCWAroundOrigo)

  /**
   * Rotates this object around the its center by 90 degrees counterclockwise.
   *
   * @return
   */
  override
  def rotateBy90DegsCCW: Point = this

  /**
   * Rotates this object around a given point by 90 degrees counterclockwise.
   *
   * @param centerOfRotation
   *
   * @return
   */
  override
  def rotateBy90DegsCCW(centerOfRotation: Pos): Point =
    copy(newPosition = position.rotateBy90DegsCCW(centerOfRotation))

  /**
   * Rotates this object around origo (0,0) by 180 degrees.
   *
   * @return
   */
  override
  def rotateBy180DegsAroundOrigo: Point =
    copy(newPosition = position.rotateBy180DegsAroundOrigo)

  /**
   * Rotates this object around its center by 180 degrees.
   *
   * @return
   */
  override
  def rotateBy180Degs: Point = this

  /**
   * Rotates this object around a given point by 180 degrees.
   *
   * @param centerOfRotation
   *
   * @return
   */
  override
  def rotateBy180Degs(centerOfRotation: Pos): Point =
    copy(newPosition = position.rotateBy180Degs(centerOfRotation))

  /**
   * Rotates this object around its center by the specified angle.
   *
   * @param angle
   *
   * @return
   */
  override
  def rotateByAroundOrigo(angle: Angle): Point = rotateByAroundOrigo(angle)

  /**
   * Rotates this object around its center by the specified number of degrees.
   *
   * @param angleInDegrees
   *
   * @return
   */
  override
  def rotateByAroundOrigo(angleInDegrees: Double): Point =
    copy(newPosition = position.rotateByAroundOrigo(angleInDegrees))

  /**
   * Rotates this object around its center by the specified angle.
   *
   * @param angle
   *
   * @return
   */
  override
  def rotateBy(angle: Angle): Point = rotateBy(angle)

  /**
   * Rotates this object around its center by the specified number of degrees.
   *
   * @param angleInDegrees
   *
   * @return
   */
  override
  def rotateBy(angleInDegrees: Double): Point = this

  /**
   * Rotates this object around a given point by the specified angle.
   *
   * @param angle
   * @param centerOfRotation
   *
   * @return
   */
  override
  def rotateBy(
      angle: Angle,
      centerOfRotation: Pos): Point = {

    rotateBy(angle, centerOfRotation)
  }

  /**
   * Rotates this object around a given point by the specified number of degrees.
   *
   * @param angleInDegrees
   * @param centerOfRotation
   *
   * @return
   */
  override
  def rotateBy(
      angleInDegrees: Double,
      centerOfRotation: Pos): Point = {

    copy(newPosition = position.rotateBy(angleInDegrees, centerOfRotation))
  }

  /**
   *
   *
   * @param widthFactor
   * @param heightFactor
   *
   * @return
   */
  override
  def scaleBy(
      widthFactor: Double,
      heightFactor: Double): Point = {

    this
  }

}
