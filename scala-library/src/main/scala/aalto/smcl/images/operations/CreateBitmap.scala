package aalto.smcl.images.operations

import java.awt.{
  Color => JColor,
  Graphics2D => JGraphics2D
}
import java.awt.image.{ BufferedImage => JBufferedImage }

/**
 * An operation to create a bitmap buffer of a given size.
 *
 * @author Aleksi Lukkarinen
 */
case class CreateBitmap(widthInPixels: Int, heightInPixels: Int)
    extends BitmapOperation with BufferProvider {

  /** This [[BitmapOperation]] does not have any child operations. */
  val childOperationListsOption: Option[Array[BitmapOperationList]] = None

  /** Information about this [[BitmapOperation]] instance */
  val metaInformation = Map(
    "name" -> "CreateBitmap",
    "width" -> "${widthInPixels} px",
    "height" -> "${heightInPixels} px"
  )

  /**
   * Returns a new bitmap buffer of a size given to this [[BitmapOperation]] instance.
   */
  def buffer(): JBufferedImage =
    new JBufferedImage(widthInPixels, heightInPixels, JBufferedImage.TYPE_INT_ARGB)
}
