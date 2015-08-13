package aalto.smcl.bitmaps.operations


import aalto.smcl.bitmaps.BitmapSettingKeys._
import aalto.smcl.common._
import aalto.smcl.platform.PlatformBitmapBuffer




/**
 * Operation to draw a polygon with given colors. If a color is not given, the default primary/secondary
 * color will be used, as defined in the [[aalto.smcl.common.GS]]. The resulting polyline will be automatically closed.
 *
 * @param xCoordinates
 * @param yCoordinates
 * @param numberOfCoordinatesToDraw
 * @param hasBorder
 * @param hasFilling
 * @param color
 * @param fillColor
 *
 * @author Aleksi Lukkarinen
 */
private[bitmaps] case class DrawPolygon(
  xCoordinates: Seq[Int],
  yCoordinates: Seq[Int],
  numberOfCoordinatesToDraw: Int,
  hasBorder: Boolean = GS.isTrueThat(ShapesHaveBordersByDefault),
  hasFilling: Boolean = GS.isTrueThat(ShapesHaveFillingsByDefault),
  color: RGBAColor = GS.colorFor(DefaultPrimary),
  fillColor: RGBAColor = GS.colorFor(DefaultSecondary))
  extends AbstractOperation with RenderableOperation with Immutable {

  require(xCoordinates != null, "The x coordinate argument has to be an Seq[Int] instance (was null).")
  require(yCoordinates != null, "The y coordinate argument has to be an Seq[Int] instance (was null).")

  val numberOfCoordinatesPresent = xCoordinates.length.min(yCoordinates.length)

  require(numberOfCoordinatesPresent > 1, s"The coordinate sequences must have at least two coordinate pairs present.")

  require(numberOfCoordinatesToDraw > 1,
    s"At least two coordinate pairs (which equals one line segment) has to be drawn.")

  require(numberOfCoordinatesToDraw <= numberOfCoordinatesPresent,
    s"The coordinate sequences do not contain the requested amount of coordinate pairs " +
      s"(only $numberOfCoordinatesPresent pairs present, $numberOfCoordinatesToDraw requested).")

  require(color != null, "The line color argument has to be a Color instance (was null).")
  require(fillColor != null, "The fill color argument has to be a Color instance (was null).")

  /** Information about this [[RenderableOperation]] instance */
  lazy val metaInformation = MetaInformationMap(Map(
    "coordinates" -> Option(xCoordinates.zip(yCoordinates).mkString(StrSpace)),
    "numberOfCoordinatesPresent" -> Option(numberOfCoordinatesPresent.toString),
    "numberOfCoordinatesToDraw" -> Option(numberOfCoordinatesToDraw.toString),
    "hasBorder" -> Option(hasBorder.toString),
    "hasFilling" -> Option(hasFilling.toString),
    "color" -> Option(s"0x${color.toPixelInt.toArgbHexColorString}"),
    "fillColor" -> Option(s"0x${fillColor.toPixelInt.toArgbHexColorString}")))

  /**
   * Draws a polygon onto the given bitmap with the given colors.
   *
   * @param destination
   */
  override def render(destination: PlatformBitmapBuffer): Unit = {
    destination.drawingSurface().drawPolygon(
      xCoordinates, yCoordinates,
      numberOfCoordinatesToDraw,
      hasBorder, hasFilling,
      color, fillColor)
  }

}
