package aalto.smcl.platform


import java.awt.{Color => AwtColor}

import aalto.smcl.colors.RGBAColor




/**
 *
 *
 * @param self
 *
 * @author Aleksi Lukkarinen
 */
private[platform] class RichAwtColor(val self: AwtColor) {

  /** This `java.awt.Color` as a [[RGBAColor]]. */
  def toApplicationColor: RGBAColor = PlatformColor(self).applicationColor

  /** This `java.awt.Color` as a [[RGBAColor]] with full opacity. */
  def withFullOpacity: RGBAColor = PlatformColor(self).applicationColor.withFullOpacity

}
