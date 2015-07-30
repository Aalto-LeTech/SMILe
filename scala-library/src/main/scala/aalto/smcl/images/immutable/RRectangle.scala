package aalto.smcl.images.immutable

import aalto.smcl.common.{Color, GS}
import aalto.smcl.images.SettingKeys._
import aalto.smcl.images.immutable.Bitmap.ViewerUpdateStyle
import aalto.smcl.images.immutable.Bitmap.ViewerUpdateStyle.{PreventViewerUpdates, UpdateViewerPerDefaults}




/**
 *
 *
 * @author Aleksi Lukkarinen
 */
object RRectangle {

  aalto.smcl.images.SettingsInitializer.perform()

  /**
   * Creates a new empty [[Bitmap]] instance with a rectangle drawn on it.
   */
  def apply(
      widthInPixels: Int = GS.intFor(DefaultBitmapWidthInPixels),
      heightInPixels: Int = GS.intFor(DefaultBitmapHeightInPixels),
      roundingWidthInPixels: Int = GS.intFor(DefaultRoundingWidthInPixels),
      roundingHeightInPixels: Int = GS.intFor(DefaultRoundingHeightInPixels),
      rectangleColor: Color = GS.colorFor(DefaultPrimary),
      backgroundColor: Color = GS.colorFor(DefaultBackground),
      viewerHandling: ViewerUpdateStyle.Value = UpdateViewerPerDefaults): Bitmap = {

    require(widthInPixels >= 10, s"Width of the rectangle must be at least 10 pixels (was $widthInPixels)")
    require(heightInPixels >= 10, s"Height of the rectangle must be at least 10 pixels (was $heightInPixels)")
    require(roundingWidthInPixels > 0, s"The rounding width argument must be greater than zero (was $roundingWidthInPixels).")
    require(roundingHeightInPixels > 0, s"The rounding height argument must be greater than zero (was $roundingHeightInPixels).")
    require(rectangleColor != null, "The rectangle color argument has to be a Color instance (was null).")
    require(backgroundColor != null, "The background color argument has to be a Color instance (was null).")

    val newBitmap = Bitmap(
      widthInPixels,
      heightInPixels,
      backgroundColor,
      viewerHandling = PreventViewerUpdates)

    newBitmap.drawRoundedRectangle(
      0, 0,
      widthInPixels - 1, heightInPixels - 1,
      roundingWidthInPixels, roundingHeightInPixels,
      isFilled = true,
      lineColor = rectangleColor,
      fillColor = rectangleColor,
      viewerHandling)

  }

}
