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


/**
 *
 *
 * @author Aleksi Lukkarinen
 */
case class NullPixelSnapshotIterator(
    relatedPixelSnapshot: NullPixelSnapshot)
    extends AbstractPixelSnapshotIterator {

  /**
   *
   *
   * @return
   */
  def currentXInPixels: Int = minXInPixels

  /**
   *
   *
   * @return
   */
  def currentYInPixels: Int = minYInPixels

  /**
   *
   *
   * @return
   */
  def columnHasChanged: Boolean = false

  /**
   *
   *
   * @return
   */
  def rowHasChanged: Boolean = false

  /**
   *
   *
   * @return
   */
  def hasNext: Boolean = false

  /**
   *
   *
   * @return
   */
  def next(): Pixel = Iterator.empty.next()

}
