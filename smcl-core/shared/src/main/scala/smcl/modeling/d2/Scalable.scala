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

package smcl.modeling.d2


/**
 *
 *
 * @tparam ReturnType
 *
 * @author Aleksi Lukkarinen
 */
trait Scalable[ReturnType] {

  self: HasDims =>


  /**
   *
   *
   * @param targetSize
   *
   * @return
   */
  @inline
  def scaleTo(targetSize: Double): ReturnType = {
    scaleTo(targetSize, targetSize)
  }

  /**
   *
   *
   * @param targetWidth
   *
   * @return
   */
  @inline
  def scaleWidthTo(targetWidth: Double): ReturnType = {
    scaleBy(targetWidth / dimensions.width.inPixels, 1.0)
  }

  /**
   *
   *
   * @param targetHeight
   *
   * @return
   */
  @inline
  def scaleHeightTo(targetHeight: Double): ReturnType = {
    scaleBy(1.0, targetHeight / dimensions.height.inPixels)
  }

  /**
   *
   *
   * @param targetWidth
   * @param targetHeight
   *
   * @return
   */
  @inline
  def scaleTo(
      targetWidth: Double,
      targetHeight: Double): ReturnType = {

    scaleBy(
      targetWidth / dimensions.width.inPixels,
      targetHeight / dimensions.height.inPixels)
  }

  /**
   *
   *
   * @param factor
   *
   * @return
   */
  @inline
  def scaleBy(factor: Double): ReturnType = {
    scaleBy(factor, factor)
  }

  /**
   *
   *
   * @param factor
   *
   * @return
   */
  @inline
  def scaleWidthBy(factor: Double): ReturnType = {
    scaleBy(factor, 1.0)
  }

  /**
   *
   *
   * @param factor
   *
   * @return
   */
  @inline
  def scaleHeightBy(factor: Double): ReturnType = {
    scaleBy(1.0, factor)
  }

  /**
   *
   *
   * @param widthFactor
   * @param heightFactor
   *
   * @return
   */
  @inline
  def scaleBy(
      widthFactor: Double,
      heightFactor: Double): ReturnType

}
