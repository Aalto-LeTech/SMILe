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
 * @author Aleksi Lukkarinen
 */
object Scalable {

  /** The scaling factor (1.0) that does not cause any effect when scaling. */
  val IdentityScalingFactor: Double = 1.0

}




/**
 *
 *
 * @tparam ReturnType
 *
 * @author Aleksi Lukkarinen
 */
trait Scalable[ReturnType] {

  // Scale horizontally
  // -------------------------------------------------------------------------------------------- \\

  /**
   * Scales this object horizontally in relation to its center.
   *
   * @param factor
   *
   * @return
   */
  def scaleHorizontallyBy(factor: Double): ReturnType

  /**
   * Scales this object horizontally in relation to a given point.
   *
   * @param factor
   * @param relativityPoint
   *
   * @return
   */
  def scaleHorizontallyBy(
      factor: Double,
      relativityPoint: Pos): ReturnType

  /**
   * Scales this object horizontally in relation to the origo.
   *
   * @param factor
   *
   * @return
   */
  def scaleHorizontallyByRelativeToOrigo(factor: Double): ReturnType


  // Scale vertically
  // -------------------------------------------------------------------------------------------- \\

  /**
   * Scales this object vertically in relation to its center.
   *
   * @param factor
   *
   * @return
   */
  def scaleVerticallyBy(factor: Double): ReturnType

  /**
   * Scales this object vertically in relation to a given point.
   *
   * @param factor
   * @param relativityPoint
   *
   * @return
   */
  def scaleVerticallyBy(
      factor: Double,
      relativityPoint: Pos): ReturnType

  /**
   * Scales this object vertically in relation to the origo.
   *
   * @param factor
   *
   * @return
   */
  def scaleVerticallyByRelativeToOrigo(factor: Double): ReturnType


  // Scale using a given factor for both horizontal and vertical directions
  // -------------------------------------------------------------------------------------------- \\

  /**
   * Scales this object in relation to its center by using a given factor
   * for both horizontal and vertical directions.
   *
   * @param factor
   *
   * @return
   */
  def scaleBy(factor: Double): ReturnType

  /**
   * Scales this object in relation to a given point by using a given factor
   * for both horizontal and vertical directions.
   *
   * @param factor
   * @param relativityPoint
   *
   * @return
   */
  def scaleBy(
      factor: Double,
      relativityPoint: Pos): ReturnType

  /**
   * Scales this object in relation to the origo by using a given factor for
   * both horizontal and vertical directions.
   *
   * @param factor
   *
   * @return
   */
  def scaleByRelativeToOrigo(factor: Double): ReturnType


  // Scale using both horizontal and vertical factors
  // -------------------------------------------------------------------------------------------- \\

  /**
   * Scales this object by given horizontal and vertical factors in relation to its center.
   *
   * @param horizontalFactor
   * @param verticalFactor
   *
   * @return
   */
  def scaleBy(
      horizontalFactor: Double,
      verticalFactor: Double): ReturnType

  /**
   * Scales this object by given horizontal and vertical factors in relation to a given point.
   *
   * @param horizontalFactor
   * @param verticalFactor
   * @param relativityPoint
   *
   * @return
   */
  def scaleBy(
      horizontalFactor: Double,
      verticalFactor: Double,
      relativityPoint: Pos): ReturnType

  /**
   * Scales this object by given horizontal and vertical factors in relation to the origo.
   *
   * @param horizontalFactor
   * @param verticalFactor
   *
   * @return
   */
  def scaleByRelativeToOrigo(
      horizontalFactor: Double,
      verticalFactor: Double): ReturnType

}
