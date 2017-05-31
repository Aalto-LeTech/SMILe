/* .            .           .                   .                 +             .          +      */
/*         +-----------+  +---+    +  +---+  +-----------+  +---+    Media Programming in Scala   */
/*   *     |           |  |    \     /    |  |           | +|   |            Since 2015           */
/*         |   +-------+  |     \   /     |  |   +-------+  |   |   .                        .    */
/*         |   |          |      \ /      |  |   |          |   |         Aalto University        */
/*       . |   +-------+  |   .   V   .   |  |   |   .      |   |      .   Espoo, Finland       . */
/*  +      |           |  |   |\     /|   |  |   |          |   |                  .    +         */
/*         +------+    |  |   | \   / |   |  |   |          |   |    +        *                   */
/*    *           |    |  |   |  \ /  |   |  |   |      *   |   |                     .      +    */
/*      -- +------+    |  |   |   V  *|   |  |   +-------+  |   +-------+ --    .                 */
/*    ---  |           |  |   | .     |   |  |           |  |           |  ---      +      *      */
/*  ------ +-----------+  +---+       +---+  +-----------+  +-----------+ ------               .  */
/*                                                                                     .          */
/*     T H E   S C A L A   M E D I A   C O M P U T A T I O N   L I B R A R Y      .         +     */
/*                                                                                    *           */

package aalto.smcl.bitmaps.metadata


import java.awt.image.BufferedImage

import aalto.smcl.bitmaps.Bitmap
import aalto.smcl.infrastructure.jvmawt.AWTBitmapBufferAdapter
import aalto.smcl.interfaces.awt.{StaticGeneralBitmapSource, StaticThumbnailBitmapSource}
import aalto.smcl.interfaces.{MetaInterfaceBase, ResourceMetadataSource, Timestamp}




/**
 *
 *
 * @author Aleksi Lukkarinen
 */
private[metadata]
case class ImmutableBitmapMetadataSource(relatedBitmap: Bitmap)
    extends MetaInterfaceBase
            with ResourceMetadataSource
            with StaticGeneralBitmapSource
            with StaticThumbnailBitmapSource {


  /** Number of bitmaps provided per Bitmap instance by this metadata source. */
  val OneBitmap = 1

  /** First possible image index. */
  val FirstImageIndex = 0

  /**
   *
   *
   * @param bitmapNumber
   */
  def validateBitmapNumber(bitmapNumber: Int): Unit = {
    require(bitmapNumber == FirstImageIndex, "This resource supports only one bitmap (index 0).")
  }

  /**
   *
   *
   * @param bitmapNumber
   *
   * @return
   */
  override def resourceIdOption(bitmapNumber: Int = FirstImageIndex): Option[String] = {
    validateBitmapNumber(bitmapNumber)

    Some(relatedBitmap.uniqueIdentifier.identity)
  }

  /**
   *
   *
   * @param bitmapNumber
   *
   * @return
   */
  override def resourceAuthorsOption(bitmapNumber: Int = FirstImageIndex): Option[String] = {
    validateBitmapNumber(bitmapNumber)

    None
  }

  /**
   *
   *
   * @param bitmapNumber
   *
   * @return
   */
  override def resourceKeywordsOption(bitmapNumber: Int = FirstImageIndex): Option[String] = {
    validateBitmapNumber(bitmapNumber)

    None
  }

  /**
   *
   *
   * @param bitmapNumber
   *
   * @return
   */
  override def resourceTimestampOption(bitmapNumber: Int = FirstImageIndex): Option[Timestamp] = {
    validateBitmapNumber(bitmapNumber)

    Some(relatedBitmap.created)
  }

  /**
   *
   *
   * @param bitmapNumber
   *
   * @return
   */
  override def resourceDescriptionOption(bitmapNumber: Int = FirstImageIndex): Option[String] = {
    validateBitmapNumber(bitmapNumber)

    None
  }

  /**
   *
   *
   * @param bitmapNumber
   *
   * @return
   */
  override def resourceTitleOption(bitmapNumber: Int = FirstImageIndex): Option[String] = {
    validateBitmapNumber(bitmapNumber)

    None
  }

  /**
   *
   *
   * @param bitmapNumber
   *
   * @return
   */
  override def generalBitmapOption(bitmapNumber: Int = FirstImageIndex): Option[BufferedImage] = {
    validateBitmapNumber(bitmapNumber)

    Some(relatedBitmap.toRenderedRepresentation.asInstanceOf[AWTBitmapBufferAdapter].awtBufferedImage)
  }

  /**
   *
   *
   * @return
   */
  override def numberOfGeneralBitmaps(): Int = OneBitmap

  /**
   *
   *
   * @return
   */
  override def generalBitmapsOption(): Option[Seq[BufferedImage]] =
    Some(Seq(relatedBitmap.toRenderedRepresentation.asInstanceOf[AWTBitmapBufferAdapter].awtBufferedImage))

  /**
   *
   *
   * @param thumbnailNumber
   * @param maximumWidthInPixels
   * @param maximumHeightInPixels
   *
   * @return
   */
  override def thumbnailBitmapOption(
      thumbnailNumber: Int = FirstImageIndex,
      maximumWidthInPixels: Int,
      maximumHeightInPixels: Int): Option[BufferedImage] = {

    validateBitmapNumber(thumbnailNumber)

    // TODO: After Bitmap can tell a suitable scaling factor for a given target size and has scaling operation, refactor the following code to utilize them

    var buffer = relatedBitmap.toRenderedRepresentation.asInstanceOf[AWTBitmapBufferAdapter].awtBufferedImage

    if (relatedBitmap.widthInPixels > maximumWidthInPixels
        || relatedBitmap.heightInPixels > maximumHeightInPixels) {

      val scalingFactor =
        if (relatedBitmap.widthInPixels > maximumWidthInPixels)
          maximumWidthInPixels.toDouble / relatedBitmap.widthInPixels
        else
          maximumHeightInPixels.toDouble / relatedBitmap.heightInPixels

      buffer = relatedBitmap.scale(scalingFactor).toRenderedRepresentation.asInstanceOf[AWTBitmapBufferAdapter].awtBufferedImage
    }

    Some(buffer)
  }

  /**
   *
   *
   * @param maximumWidthInPixels
   * @param maximumHeightInPixels
   *
   * @return
   */
  override def thumbnailBitmapsOption(
      maximumWidthInPixels: Int,
      maximumHeightInPixels: Int): Option[Seq[BufferedImage]] = {

    Some(Seq(thumbnailBitmapOption(0, maximumWidthInPixels, maximumHeightInPixels).get))
  }

  /**
   *
   *
   * @return
   */
  override def numberOfThumbnailBitmaps(): Int = OneBitmap

}
