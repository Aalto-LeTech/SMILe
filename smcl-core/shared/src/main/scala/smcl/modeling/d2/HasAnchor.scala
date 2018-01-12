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


import smcl.modeling.Len




/**
 *
 *
 * @author Juha Sorva
 * @author Aleksi Lukkarinen
 */
trait HasAnchor {

  self: HasDims =>


  /** */
  def anchor: Anchor[HasAnchor]

  /** */
  @inline
  override
  def width: Len = dimensions.width

  /** */
  @inline
  override
  def height: Len = dimensions.height

  /**
   *
   *
   * @return
   */
  @inline
  def internalAnchorDims: Dims = {
    anchor.internalDimsWithin(self)
  }

  /**
   *
   *
   * @return
   */
  @inline
  def internalAnchorPos: Pos = {
    anchor.internalPosWithin(self)
  }

  /**
   *
   *
   * @return
   */
  @inline
  def internalAnchorX: Double = {
    anchor.internalXWithin(self)
  }

  /**
   *
   *
   * @return
   */
  @inline
  def internalAnchorY: Double = {
    anchor.internalYWithin(self)
  }

}