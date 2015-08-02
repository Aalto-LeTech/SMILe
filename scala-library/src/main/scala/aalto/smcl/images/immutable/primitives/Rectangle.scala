package aalto.smcl.images.immutable.primitives

import aalto.smcl.common.{Color, GS}
import aalto.smcl.images.SettingKeys.{DefaultBitmapHeightInPixels, DefaultBitmapWidthInPixels, DefaultPrimary}
import aalto.smcl.images.immutable.primitives.Bitmap.ViewerUpdateStyle
import aalto.smcl.images.immutable.primitives.Bitmap.ViewerUpdateStyle.UpdateViewerPerDefaults




/**
 *
 *
 * @author Aleksi Lukkarinen
 */
object Rectangle {

  aalto.smcl.images.SettingsInitializer.perform()

  /**
   * Creates a new empty [[Bitmap]] instance with a rectangle drawn on it.
   *
   * @param widthInPixels
   * @param heightInPixels
   * @param color
   * @param viewerHandling
   * @return
   */
  def apply(
      widthInPixels: Int = GS.intFor(DefaultBitmapWidthInPixels),
      heightInPixels: Int = GS.intFor(DefaultBitmapHeightInPixels),
      color: Color = GS.colorFor(DefaultPrimary),
      viewerHandling: ViewerUpdateStyle.Value = UpdateViewerPerDefaults): Bitmap = {

    require(widthInPixels > 0, s"Width of the rectangle must be at least 1 pixel (was $widthInPixels)")
    require(heightInPixels > 0, s"Height of the rectangle must be at least 1 pixel (was $heightInPixels)")
    require(color != null, "The color argument has to be a Color instance (was null).")

    Bitmap(widthInPixels, heightInPixels, color, viewerHandling)
  }

}
