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

package aalto.smcl


import scala.language.implicitConversions


/**
 *
 *
 * @author Aleksi Lukkarinen
 */
package object colors
    extends ColorOperationsAPI {

  /** Color component value representing maximal opacity. */
  val FullyOpaque: Int = ColorValidator.MaximumRgbaOpacity

  /** Color component value representing minimal opacity. */
  val FullyTransparent: Int = ColorValidator.MinimumRgbaOpacity


  /** */
  lazy val ColorValidator: ColorValidator = new ColorValidator()

  /** */
  lazy val PresetColors: PresetColors = new PresetColors()

  /** */
  lazy val RGBATranslationTableValidator: RGBATranslationTableValidator =
    new RGBATranslationTableValidator()


  /** Application of the RichArgbInt class. */
  implicit def ARGBIntWrapper(self: Int): RichARGBInt = new RichARGBInt(self)

  /** Application of the RichRGBAColor class. */
  implicit def RGBAColorWrapper(self: RGBAColor): RichRGBAColor = new RichRGBAColor(self)

}
