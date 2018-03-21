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


import smcl.modeling.Len
import smcl.modeling.misc.AbstractMovable
import smcl.settings._




/**
 *
 *
 * @tparam ReturnType
 *
 * @author Aleksi Lukkarinen
 */
trait Movable[ReturnType]
    extends AbstractMovable[ReturnType] {

  /**
   *
   *
   * @param offset
   *
   * @return
   */
  @inline
  def + (offset: Dims): ReturnType = moveBy(offset.lengthInPixels)

  /**
   *
   *
   * @param offset
   *
   * @return
   */
  @inline
  def + (offset: Len): ReturnType = moveBy(offset.inPixels)

  /**
   *
   *
   * @param offsetInPixels
   *
   * @return
   */
  @inline
  def + (offsetInPixels: Double): ReturnType = moveBy(offsetInPixels)

  /**
   *
   *
   * @param offset
   *
   * @return
   */
  @inline
  def - (offset: Dims): ReturnType = moveBy(-offset.lengthInPixels)

  /**
   *
   *
   * @param offset
   *
   * @return
   */
  @inline
  def - (offset: Len): ReturnType = moveBy(-offset.inPixels)

  /**
   *
   *
   * @param offsetInPixels
   *
   * @return
   */
  @inline
  def - (offsetInPixels: Double): ReturnType = moveBy(-offsetInPixels)

  /**
   *
   *
   * @param offset
   *
   * @return
   */
  @inline
  def moveBy(offset: Dims): ReturnType = moveBy(offset.lengthInPixels)

  /**
   *
   *
   * @param offset
   *
   * @return
   */
  @inline
  def moveBy(offset: Len): ReturnType = moveBy(offset.inPixels)

  /**
   *
   *
   * @param offsetInPixels
   *
   * @return
   */
  @inline
  def moveBy(offsetInPixels: Double): ReturnType

  /**
   *
   *
   * @param position
   * @param positionType
   *
   * @return
   */
  @inline
  def moveTo(
      position: Pos,
      positionType: PositionType): ReturnType = {

    moveTo(position, positionType)
  }

  /**
   *
   *
   * @param coordinateInPixels
   * @param positionType
   *
   * @return
   */
  @inline
  def moveTo(
      coordinateInPixels: Double,
      positionType: PositionType): ReturnType = {

    positionType match {
      case PosTypeCenter          => moveCenterTo(coordinateInPixels)
      case PosTypeUpperLeftCorner => moveUpperLeftCornerTo(coordinateInPixels)
    }
  }

  /**
   *
   *
   * @param position
   *
   * @return
   */
  @inline
  def moveUpperLeftCornerTo(position: Pos): ReturnType =
    moveUpperLeftCornerTo(position.inPixels)

  /**
   *
   *
   * @param coordinateInPixels
   *
   * @return
   */
  @inline
  def moveUpperLeftCornerTo(coordinateInPixels: Double): ReturnType

  /**
   *
   *
   * @param position
   *
   * @return
   */
  @inline
  def moveCenterTo(position: Pos): ReturnType = moveCenterTo(position.inPixels)

  /**
   *
   *
   * @param coordinateInPixels
   *
   * @return
   */
  @inline
  def moveCenterTo(coordinateInPixels: Double): ReturnType

}
