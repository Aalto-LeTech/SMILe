package aalto.smcl.images

import java.util.Calendar

/**
 *
 *
 * @author Aleksi Lukkarinen
 */
case class BitmapImage (
    widthInPixels: Int = DEFAULT_IMAGE_HEIGHT_IN_PIXELS,
    heightInPixels: Int = DEFAULT_IMAGE_HEIGHT_IN_PIXELS,
    initialBackgroundColorOption: Option[Int] = None,
    titleOption: Option[String] = None,
    descriptionOption: Option[String] = None,
    courseNameOption: Option[String] = None,
    assignmentOption: Option[String] = None,
    creatorNameOption: Option[String] = None) {
 
  val EMPTY_STRING = ""
  
  val imageModel = BitmapImageModel(this, widthInPixels, heightInPixels, initialBackgroundColorOption)

  /** Creation time and date of this image. */
  val created = Calendar.getInstance.getTime

  /**
   *
   */
  override def toString() = {
    val p = imageModel.pixelBuffer

    s"[BitmapImage ${p.getWidth}x${p.getWidth} px" +
      titleOption.fold[String](EMPTY_STRING)(t => s"; Title: '${t}'") +
      s"; created: ${created}]"
  }

}
