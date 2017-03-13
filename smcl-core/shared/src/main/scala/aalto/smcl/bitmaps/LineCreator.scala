package aalto.smcl.bitmaps


import aalto.smcl.bitmaps.ViewerUpdateStyle.{PreventViewerUpdates, UpdateViewerPerDefaults}
import aalto.smcl.colors.RGBAColor
import aalto.smcl.infrastructure.GS




/**
 *
 *
 * @author Aleksi Lukkarinen
 */
private[bitmaps]
class LineCreator private[bitmaps]() {

  /**
   * Creates a new empty [[Bitmap]] instance with a line drawn on it. The line can be freely located in
   * the Cartesian coordinate system, but the bitmap will always contain the minimum amount of empty space.
   * That is, the line will always be drawn from one corner of the bitmap to the opposite corner.
   *
   * @param fromXInPixels
   * @param fromYInPixels
   * @param toXInPixels
   * @param toYInPixels
   * @param color
   * @param backgroundColor
   * @param viewerHandling
   * @return
   */
  def createOne(
    fromXInPixels: Int,
    fromYInPixels: Int,
    toXInPixels: Int,
    toYInPixels: Int,
    color: RGBAColor = GS.colorFor(DefaultPrimary),
    backgroundColor: RGBAColor = GS.colorFor(DefaultBackground),
    viewerHandling: ViewerUpdateStyle.Value = UpdateViewerPerDefaults): Bitmap = {

    require(color != null, "The line color argument has to be a Color instance (was null).")
    require(backgroundColor != null, "The background color argument has to be a Color instance (was null).")

    val differenceX = toXInPixels - fromXInPixels
    val differenceY = toYInPixels - fromYInPixels
    val slopeSign = Math.signum(differenceY.toDouble / differenceX)
    val bitmapWidth = Math.abs(differenceX)
    val bitmapHeight = Math.abs(differenceY)

    require(bitmapWidth > 0, s"Difference of the x coordinates must be at least 1 pixel (was $bitmapWidth)")
    require(bitmapHeight > 0, s"Difference of the y coordinates must be at least 1 pixel (was $bitmapHeight)")

    val newBitmap = Bitmap(bitmapWidth, bitmapHeight, backgroundColor, PreventViewerUpdates)

    val (x0, y0, x1, y1) =
      if (slopeSign < 0)
        (0, 0, bitmapWidth - 1, bitmapHeight - 1)
      else
        (0, bitmapHeight - 1, bitmapWidth - 1, 0)

    val newLine = newBitmap.drawLine(x0, y0, x1, y1, color, PreventViewerUpdates)

    if (viewerHandling == UpdateViewerPerDefaults) {
      if (GS.isTrueThat(NewBitmapsAreDisplayedAutomatically))
        newLine.display()
    }

    newLine
  }

}
