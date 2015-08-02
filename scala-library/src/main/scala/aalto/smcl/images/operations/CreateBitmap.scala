package aalto.smcl.images.operations


import java.awt.image.{BufferedImage => JBufferedImage}
import java.awt.{Color => JColor, Graphics2D => JGraphics2D}

import aalto.smcl.common._
import aalto.smcl.images.SettingKeys.{DefaultBitmapHeightInPixels, DefaultBitmapWidthInPixels}
import aalto.smcl.images.immutable._
import aalto.smcl.images.immutable.primitives.Bitmap


/**
 * An operation to create a bitmap buffer of a given size.
 *
 * @param widthInPixels
 * @param heightInPixels
 *
 * @author Aleksi Lukkarinen
 */
private[images] case class CreateBitmap(
    widthInPixels: Int = GS.intFor(DefaultBitmapWidthInPixels),
    heightInPixels: Int = GS.intFor(DefaultBitmapHeightInPixels))
    extends AbstractBufferProviderOperation with Immutable {

  /** This [[AbstractBufferProviderOperation]] does not have any child operations. */
  val childOperationListsOption: Option[Array[BitmapOperationList]] = None

  /** Information about this [[AbstractBufferProviderOperation]] instance */
  lazy val metaInformation = MetaInformationMap(Map(
    "width" -> Option("${widthInPixels} px"),
    "height" -> Option("${heightInPixels} px")
  ))

  /**
   * Returns a new bitmap buffer of a size given to this [[Bitmap]] instance.
   */
  def buffer: JBufferedImage =
    new JBufferedImage(widthInPixels, heightInPixels, JBufferedImage.TYPE_INT_ARGB)
}
