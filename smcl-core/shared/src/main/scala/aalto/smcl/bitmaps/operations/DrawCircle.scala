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

package aalto.smcl.bitmaps.operations


import aalto.smcl.colors.{RGBAColor, _}
import aalto.smcl.infrastructure._




/**
 * Operation to draw a circle with given colors. If a color is not given, the default
 * primary/secondary colors will be used, as defined in the [[aalto.smcl.infrastructure.GS]].
 *
 * @param centerXInPixels
 * @param centerYInPixels
 * @param radiusInPixels
 * @param hasBorder
 * @param hasFilling
 * @param color
 * @param fillColor
 *
 * @author Aleksi Lukkarinen
 */
private[bitmaps]
case class DrawCircle(
    centerXInPixels: Int,
    centerYInPixels: Int,
    radiusInPixels: Int = GS.intFor(DefaultCircleRadiusInPixels),
    hasBorder: Boolean = GS.isTrueThat(ShapesHaveBordersByDefault),
    hasFilling: Boolean = GS.isTrueThat(ShapesHaveFillingsByDefault),
    color: RGBAColor = GS.colorFor(DefaultPrimary),
    fillColor: RGBAColor = GS.colorFor(DefaultSecondary))
    extends AbstractOperation
            with Renderable
            with Immutable {

  require(radiusInPixels > 0, s"The radius argument must be greater than zero (was $radiusInPixels).")
  require(color != null, "The line color argument has to be a Color instance (was null).")
  require(fillColor != null, "The fill color argument has to be a Color instance (was null).")

  /** X coordinate of the upper-left corner of the bounding box of the circle to be drawn. */
  val boundingBoxUpperLeftX: Int = centerXInPixels - radiusInPixels

  /** Y coordinate of the upper-left corner of the bounding box of the circle to be drawn. */
  val boundingBoxUpperLeftY: Int = centerYInPixels - radiusInPixels

  /** Length of a side of the bounding box of the circle to be drawn. */
  val boundingBoxSideLength: Int = 2 * radiusInPixels

  /** First text paragraph of the description of this class. */
  val descriptionTitle: String = "DrawCircle"

  /** Information about this [[Renderable]] instance */
  lazy val describedProperties = Map(
    "centerX" -> Option(s"$centerXInPixels px"),
    "centerY" -> Option(s"$centerYInPixels px"),
    "radius" -> Option(s"$radiusInPixels px"),
    "hasBorder" -> Option(hasBorder.toString),
    "hasFilling" -> Option(hasFilling.toString),
    "color" -> Option(s"0x${color.toArgbInt.toArgbHexColorString}"),
    "fillColor" -> Option(s"0x${fillColor.toArgbInt.toArgbHexColorString}")
  )

  /**
   * Draws a circle onto the given bitmap with the given colors.
   *
   * @param destination
   */
  override def render(destination: BitmapBufferAdapter): Unit = {
    destination.drawingSurface.drawEllipse(
      boundingBoxUpperLeftX, boundingBoxUpperLeftY,
      boundingBoxSideLength, boundingBoxSideLength,
      hasBorder, hasFilling,
      color, fillColor)
  }

}
