package aalto.smcl.bitmaps.operations


import aalto.smcl.bitmaps.BitmapSettingKeys.DefaultPrimary
import aalto.smcl.common.ColorOps.RichPixelInt
import aalto.smcl.common.{Color, GS, MetaInformationMap, _}
import aalto.smcl.platform.PlatformBitmapBuffer




/**
 * Operation to draw a polyline with given color. If the color is not given, the default primary color
 * will be used, as defined in the [[aalto.smcl.common.GS]]. If the start and end points do not point to the same pixel,
 * the resulting polyline will not be closed.
 *
 * @param xCoordinates
 * @param yCoordinates
 * @param numberOfCoordinatesToDraw
 * @param color
 *
 * @author Aleksi Lukkarinen
 */
private[bitmaps] case class DrawPolyline(
    xCoordinates: Seq[Int],
    yCoordinates: Seq[Int],
    numberOfCoordinatesToDraw: Int,
    color: Color = GS.colorFor(DefaultPrimary))
    extends AbstractSingleSourceOperation with Immutable {

  require(xCoordinates != null, "The x coordinate argument has to be an Seq[Int] instance (was null).")
  require(yCoordinates != null, "The y coordinate argument has to be an Seq[Int] instance (was null).")

  val numberOfCoordinatesPresent = xCoordinates.length.min(yCoordinates.length)

  require(numberOfCoordinatesPresent > 1, s"The coordinate sequences must have at least two coordinate pairs present.")

  require(numberOfCoordinatesToDraw > 1,
    s"At least two coordinate pairs (which equals one line segment) has to be drawn.")

  require(numberOfCoordinatesToDraw <= numberOfCoordinatesPresent,
    s"The coordinate sequences do not contain the requested amount of coordinate pairs " +
        s"(only $numberOfCoordinatesPresent pairs present, $numberOfCoordinatesToDraw requested).")

  require(color != null, "The color argument has to be a Color instance (was null).")

  /** This [[AbstractSingleSourceOperation]] does not have any child operations. */
  val childOperationListsOption: Option[Seq[BitmapOperationList]] = None

  /** Information about this [[AbstractSingleSourceOperation]] instance */
  lazy val metaInformation = MetaInformationMap(Map(
    "coordinates" -> Option(xCoordinates.zip(yCoordinates).mkString(StrSpace)),
    "numberOfCoordinatesPresent" -> Option(numberOfCoordinatesPresent.toString),
    "numberOfCoordinatesToDraw" -> Option(numberOfCoordinatesToDraw.toString),
    "color" -> Option(s"0x${color.asPixelInt.toArgbHexColorString}")))

  /**
   * Draws a polyline onto the given bitmap with the given colors.
   *
   * @param destination
   */
  override def render(destination: PlatformBitmapBuffer): Unit = {
    destination.drawingSurface().drawPolyline(
      xCoordinates, yCoordinates,
      numberOfCoordinatesToDraw,
      color)
  }

}
