package aalto.smcl.bitmaps.metadata


import java.awt.image.BufferedImage
import java.util.Date

import aalto.smcl.MetaInterfaceBase
import aalto.smcl.bitmaps.BitmapLoadingResult
import aalto.smcl.interfaces.{ResourceMetadataSource, StaticGeneralBitmapSource, StaticThumbnailBitmapSource}




/**
 *
 *
 * @author Aleksi Lukkarinen
 */
private[metadata]
case class BitmapLoadingResultMetadataSource(relatedBitmapLoadingResult: BitmapLoadingResult)
  extends MetaInterfaceBase
  with ResourceMetadataSource
  with StaticGeneralBitmapSource
  with StaticThumbnailBitmapSource {

  /**
   *
   *
   * @param bitmapNumber
   */
  def validateBitmapNumber(bitmapNumber: Int): Unit = {
    require(bitmapNumber >= 0 && bitmapNumber <= relatedBitmapLoadingResult.bitmaps.length,
      s"The valid bitmap indices are 0 to ${relatedBitmapLoadingResult.bitmaps.indices.end}")
  }

  /**
   *
   *
   * @param bitmapNumber
   * @return
   */
  override def resourceIdOption(bitmapNumber: Int = 0): Option[String] = {
    validateBitmapNumber(bitmapNumber)

    Some(relatedBitmapLoadingResult.bitmaps(bitmapNumber)._2.uniqueIdentifier.identity)
  }

  /**
   *
   *
   * @param bitmapNumber
   * @return
   */
  override def resourceAuthorsOption(bitmapNumber: Int = 0): Option[String] = {
    validateBitmapNumber(bitmapNumber)

    None
  }

  /**
   *
   *
   * @param bitmapNumber
   * @return
   */
  override def resourceKeywordsOption(bitmapNumber: Int = 0): Option[String] = {
    validateBitmapNumber(bitmapNumber)

    None
  }

  /**
   *
   *
   * @param bitmapNumber
   * @return
   */
  override def resourceTimestampOption(bitmapNumber: Int = 0): Option[Date] = {
    validateBitmapNumber(bitmapNumber)

    Some(relatedBitmapLoadingResult.bitmaps(bitmapNumber)._2.created.underlyingDate)
  }

  /**
   *
   *
   * @param bitmapNumber
   * @return
   */
  override def resourceDescriptionOption(bitmapNumber: Int = 0): Option[String] = {
    validateBitmapNumber(bitmapNumber)

    None
  }

  /**
   *
   *
   * @param bitmapNumber
   * @return
   */
  override def resourceTitleOption(bitmapNumber: Int = 0): Option[String] = {
    validateBitmapNumber(bitmapNumber)

    None
  }

  /**
   *
   *
   * @param bitmapNumber
   * @return
   */
  override def generalBitmapOption(bitmapNumber: Int = 0): Option[BufferedImage] = {
    validateBitmapNumber(bitmapNumber)

    Some(relatedBitmapLoadingResult.bitmaps(bitmapNumber)._2.toRenderedRepresentation.awtBufferedImage)
  }

  /**
   *
   *
   * @return
   */
  override def numberOfGeneralBitmaps(): Int = relatedBitmapLoadingResult.bitmaps.length

  /**
   *
   *
   * @return
   */
  override def generalBitmapsOption(): Option[Seq[BufferedImage]] =
    Some(relatedBitmapLoadingResult.bitmaps map (_._2.toRenderedRepresentation.awtBufferedImage))

  /**
   *
   *
   * @param thumbnailNumber
   * @param maximumWidthInPixels
   * @param maximumHeightInPixels
   * @return
   */
  override def thumbnailBitmapOption(
    thumbnailNumber: Int = 0,
    maximumWidthInPixels: Int,
    maximumHeightInPixels: Int): Option[BufferedImage] = {

    validateBitmapNumber(thumbnailNumber)

    // TODO: After Bitmap can tell a suitable scaling factor for a given target size and has scaling operation, refactor the following code to utilize them

    val bitmap = relatedBitmapLoadingResult.bitmaps(thumbnailNumber)._2
    var buffer = bitmap.toRenderedRepresentation.awtBufferedImage

    if (bitmap.widthInPixels > maximumWidthInPixels
      || bitmap.heightInPixels > maximumHeightInPixels) {

      val scalingFactor =
        if (bitmap.widthInPixels > maximumWidthInPixels)
          maximumWidthInPixels.toDouble / bitmap.widthInPixels
        else
          maximumHeightInPixels.toDouble / bitmap.heightInPixels

      buffer = bitmap.scale(scalingFactor).toRenderedRepresentation.awtBufferedImage
    }

    Some(buffer)
  }

  /**
   *
   *
   * @param maximumWidthInPixels
   * @param maximumHeightInPixels
   * @return
   */
  override def thumbnailBitmapsOption(
    maximumWidthInPixels: Int,
    maximumHeightInPixels: Int): Option[Seq[BufferedImage]] = {

    Some(relatedBitmapLoadingResult.bitmaps.indices
      .map(thumbnailBitmapOption(_: Int, maximumWidthInPixels, maximumHeightInPixels).get))
  }

  /**
   *
   *
   * @return
   */
  override def numberOfThumbnailBitmaps(): Int = relatedBitmapLoadingResult.bitmaps.length

}
