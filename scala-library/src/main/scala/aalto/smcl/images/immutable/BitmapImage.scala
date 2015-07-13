package aalto.smcl.images.immutable

import java.awt.{
  Color => JColor,
  Graphics2D => JGraphics2D
}
import java.awt.image.{ BufferedImage => JBufferedImage }
import scala.collection.immutable
import aalto.smcl._
import aalto.smcl.common._
import aalto.smcl.images._
import aalto.smcl.images.immutable._
import aalto.smcl.images.operations._

/**
 *
 *
 * @author Aleksi Lukkarinen
 */
object BitmapImage {

  /**
   *
   */
  def apply(
    widthInPixelsOption: Option[Int] = None,
    heightInPixelsOption: Option[Int] = None,
    initialBackgroundColorOption: Option[Int] = None,
    titleOption: Option[String] = None,
    descriptionOption: Option[String] = None,
    courseNameOption: Option[String] = None,
    assignmentOption: Option[String] = None,
    creatorNameOption: Option[String] = None): BitmapImage = {

    val width = widthInPixelsOption.fold(DEFAULT_IMAGE_WIDTH_IN_PIXELS) { w =>
      require(w > 0, s"Width of the image must be greater than zero (was $w)")
      w
    }

    val height = heightInPixelsOption.fold(DEFAULT_IMAGE_HEIGHT_IN_PIXELS) { h =>
      require(h > 0, s"Height of the image must be greater than zero (was $h)")
      h
    }

    val bgColor = initialBackgroundColorOption getOrElse 0xFFFFFFFF

    val buffer = new JBufferedImage(width, height, JBufferedImage.TYPE_INT_ARGB)

    new BitmapImage(buffer, bgColor,
      titleOption, descriptionOption, courseNameOption,
      assignmentOption, creatorNameOption)
  }

  private[images] def fromExistingImageBuffer(
    existingImageBuffer: JBufferedImage,
    titleOption: Option[String] = None,
    descriptionOption: Option[String] = None,
    courseNameOption: Option[String] = None,
    assignmentOption: Option[String] = None,
    creatorNameOption: Option[String] = None): BitmapImage = {

    new BitmapImage(
      existingImageBuffer,
      0xFFFFFFFF,
      titleOption, descriptionOption, courseNameOption,
      assignmentOption, creatorNameOption)
  }

  def fromSourceFile(
    sourceFilePath: String,
    titleOption: Option[String] = None,
    descriptionOption: Option[String] = None,
    courseNameOption: Option[String] = None,
    assignmentOption: Option[String] = None,
    creatorNameOption: Option[String] = None): BitmapImage = {

    // TODO: Load image from the given file and init the following buffer accordingly
    val buffer = new JBufferedImage(10, 10, JBufferedImage.TYPE_INT_ARGB)

    new BitmapImage(
      buffer,
      0xFFFFFFFF,
      titleOption, descriptionOption, courseNameOption,
      assignmentOption, creatorNameOption)
  }

}

/**
 *
 *
 * @author Aleksi Lukkarinen
 */
class BitmapImage private (
  private[images] val buffer: java.awt.image.BufferedImage,
  val initialBackgroundColor: Int,
  val titleOption: Option[String],
  val descriptionOption: Option[String],
  val courseNameOption: Option[String],
  val assignmentOption: Option[String],
  val creatorNameOption: Option[String]) extends {

  /** Width of this [[BitmapImage]] in pixels. */
  val widthInPixels: Int = buffer.getWidth

  /** Height of this [[BitmapImage]] in pixels. */
  val heightInPixels: Int = buffer.getHeight

  /** Java's `Graphics2D` interface to support more advanced graphic capabilities. */
  override private[images] val graphics2D: JGraphics2D = buffer.createGraphics()

} with PixelRectangle
    with ColorableBackground
    with OperableBitmap
    with Immutable
    with TimestampedCreation {

  /**
   *
   */
  override def applyOperation(operation: BitmapOperation) = {

  }

  /**
   *
   */
  def clear(colorOption: Option[Int] = None): Unit = {
    val g = graphics2D
    val color = colorOption getOrElse initialBackgroundColor
    val awtc = new JColor(color, true)

    g.setPaint(awtc)
    g.fillRect(0, 0, buffer.getWidth, buffer.getHeight)
  }
  clear()

  /**
   *
   */
  def pixelIntAt(x: Int, y: Int): Int = {
    require(widthRange.contains(x),
      s"The x coordinate must be >= zero and less than the width of the image (was $x)")

    require(heightRange.contains(y),
      s"The y coordinate must be >= zero and less than the height of the image (was $y)")

    buffer.getRGB(x, y)
  }

  /**
   *
   */
  def colorComponentsAt(x: Int, y: Int): collection.immutable.Map[Symbol, Int] = {
    colorComponentsFrom(pixelIntAt(x, y))
  }

  /**
   *
   */
  def setPixelIntAt(x: Int, y: Int, pixelInt: Int): Unit = {
    require(widthRange.contains(x),
      s"The x coordinate must be >= zero and less than the width of the image (was $x)")

    require(heightRange.contains(y),
      s"The y coordinate must be >= zero and less than the height of the image (was $y)")

    buffer.setRGB(x, y, pixelInt)
  }

  /**
   *
   */
  def setColorComponentsAt(x: Int, y: Int, red: Int, green: Int, blue: Int, transparency: Int): Unit = {
    require(widthRange.contains(x),
      s"The x coordinate must be >= zero and less than the width of the image (was $x)")

    require(heightRange.contains(y),
      s"The y coordinate must be >= zero and less than the height of the image (was $y)")

    buffer.setRGB(x, y, pixelIntFrom(red, green, blue, transparency))
  }

  /**
   *
   */
  def redComponentAt(x: Int, y: Int): Int = redComponentFrom(pixelIntAt(x, y))

  /**
   *
   */
  def setRedComponentAt(x: Int, y: Int, red: Int): Unit =
    setPixelIntAt(x, y, withNewRedComponent(pixelIntAt(x, y), red))

  /**
   *
   */
  def greenComponentAt(x: Int, y: Int): Int = greenComponentFrom(pixelIntAt(x, y))

  /**
   *
   */
  def setGreenComponentAt(x: Int, y: Int, green: Int): Unit =
    setPixelIntAt(x, y, withNewGreenComponent(pixelIntAt(x, y), green))

  /**
   *
   */
  def blueComponentAt(x: Int, y: Int): Int = blueComponentFrom(pixelIntAt(x, y))

  /**
   *
   */
  def setBlueComponentAt(x: Int, y: Int, blue: Int): Unit =
    setPixelIntAt(x, y, withNewBlueComponent(pixelIntAt(x, y), blue))

  /**
   *
   */
  def transparencyComponentAt(x: Int, y: Int): Int = transparencyComponentFrom(pixelIntAt(x, y))

  /**
   *
   */
  def transparencyComponentAt_=(x: Int, y: Int, transparency: Int): Unit =
    setPixelIntAt(x, y, withNewTransparencyComponent(pixelIntAt(x, y), transparency))

  /**
   *
   */
  override def toString() = {
    s"[BitmapImage ${widthInPixels}x${heightInPixels} px" +
      titleOption.fold[String](STR_EMPTY)(t => s"; Title: '${t}'") +
      s"; created: ${created}]"
  }

}
