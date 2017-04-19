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

package aalto.smcl.bitmaps


/**
 *
 *
 * @author Aleksi Lukkarinen
 */
final class SMCLMinimumBitmapSizeNotMetError private[smcl](
    realWidth: Option[Int] = None,
    realHeight: Option[Int] = None,
    resourcePath: Option[String] = None,
    imageIndexInResourceOption: Option[Int] = None,
    private val bitmapValidator: BitmapValidator)
    extends RuntimeException({
      val sb = new StringBuilder(200)

      sb ++= s"The minimum image size of ${
        bitmapValidator.MinimumBitmapHeightInPixels
      } x " +
          s"${
            bitmapValidator.MinimumBitmapHeightInPixels
          } px has not been met"

      if (realWidth.isDefined && realHeight.isDefined)
        sb ++= s" (was ${
          realWidth.get
        } x ${
          realHeight.get
        })"

      sb ++= "."

      resourcePath foreach {
        path => sb ++= s""" Resource: "$path"."""
      }
      imageIndexInResourceOption foreach {
        index => sb ++= s""" Index of the image in the resource: $index."""
      }

      sb.toString()
    }) {

}
