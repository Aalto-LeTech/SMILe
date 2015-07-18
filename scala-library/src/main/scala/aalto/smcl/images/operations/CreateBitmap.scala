package aalto.smcl.images.operations

import java.awt.{
  Color => JColor,
  Graphics2D => JGraphics2D
}
import java.awt.image.{ BufferedImage => JBufferedImage }
import aalto.smcl.common._

/**
 * An operation to create a bitmap buffer of a given size.
 *
 * @author Aleksi Lukkarinen
 */
private[images] case class CreateBitmap(widthInPixels: Int, heightInPixels: Int)
    extends AbstractBufferProviderOperation with Immutable {

  /** This [[BitmapOperation]] does not have any child operations. */
  val childOperationListsOption: Option[Array[BitmapOperationList]] = None

  /** Information about this [[BitmapOperation]] instance */
  val metaInformation = MetaInformationMap(Map(
    "width" -> Option("${widthInPixels} px"),
    "height" -> Option("${heightInPixels} px")))

  /**
   * Returns a new bitmap buffer of a size given to this [[BitmapOperation]] instance.
   */
  def buffer: JBufferedImage =
    new JBufferedImage(widthInPixels, heightInPixels, JBufferedImage.TYPE_INT_ARGB)
}
