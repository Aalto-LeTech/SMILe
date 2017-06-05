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

package aalto.smcl.bitmaps.simplified


import scala.collection.mutable

import aalto.smcl.colors.rgb.Color
import aalto.smcl.settings._




/**
 *
 *
 * @author Aleksi Lukkarinen
 */
private[bitmaps]
class SquareCreator private[bitmaps]() {

  /**
   * Creates a new empty [[Bitmap]] instance with a square drawn on it.
   *
   * @param sideLengthInPixels
   * @param color
   *
   * @return
   */
  def createOne(
      sideLengthInPixels: Int = DefaultBitmapWidthInPixels,
      color: Color = DefaultPrimaryColor): Bitmap = {

    val newShape = createArrayOf(1, sideLengthInPixels, color)(0)

    if (NewBitmapsAreDisplayedAutomatically)
      newShape.display()

    newShape
  }

  /**
   * Creates an array of [[Bitmap]] instances with a square drawn on each bitmap.
   *
   * @param collectionSize
   * @param sideLengthInPixels
   * @param color
   *
   * @return
   */
  def createArrayOf(
      collectionSize: Int = 5,
      sideLengthInPixels: Int = DefaultBitmapWidthInPixels,
      color: Color = DefaultPrimaryColor): Array[Bitmap] = {

    require(collectionSize >= 0, s"Size of the collection cannot be negative (was $collectionSize)")
    require(sideLengthInPixels > 0, s"Side length of the square must be at least 1 pixel (was $sideLengthInPixels)")
    require(color != null, "The color argument has to be a Color instance (was null).")

    val newCollection = mutable.ArrayBuffer.empty[Bitmap]

    for (_ <- 1 to collectionSize) {
      newCollection += Bitmap(
        sideLengthInPixels,
        sideLengthInPixels,
        color)
    }

    newCollection.toArray
  }

}
