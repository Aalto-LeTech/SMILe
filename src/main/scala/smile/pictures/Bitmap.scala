package smile.pictures

import smile.colors.Color
import smile.infrastructure.{BufferAdapter, ResourceFactory}
import smile.modeling.{AffineTransformation, BoundaryCalculator, Bounds, Pos}

object Bitmap:
  type LocationToColorGenerator = (Int, Int) => Color

  def apply(
      width: Int,
      height: Int,
      contentGenerator: LocationToColorGenerator
  ): Bitmap =
    new Bitmap(width, height).setColorsByLocation(contentGenerator)

  def apply(width: Int, height: Int, colors: Seq[Color]): Bitmap =
    require(
      colors.length == (width * height),
      "The colors sequence must have exactly width * height items"
    )
    val bitmap = new Bitmap(width, height)
    bitmap.buffer.setColorsFromSeq(colors)
    bitmap

  def apply(sourceResourcePath: String): Bitmap =
    val image = ResourceFactory.bufferedImageFromPath(sourceResourcePath)
    new Bitmap(image, Bounds(Pos.Origo, image.width, image.height))

  def apply(elements: PictureElement*): Bitmap = Renderer.createBitmapFrom(elements*)
  def apply(picture: Picture): Bitmap          = Renderer.createBitmapFrom(picture)

class Bitmap(val buffer: BufferAdapter, bounds: Bounds) extends PictureElement:
  def this(width: Int, height: Int) =
    this(new BufferAdapter(width, height), Bounds(Pos.Origo, width, height))
    require(width > 0 && height > 0, "Width and height must be positive")

  override lazy val boundary: Bounds = bounds

  override def copy(newPosition: Pos): PictureElement =
    new Bitmap(
      buffer,
      boundary.moveBy(newPosition.x, newPosition.y)
    )

  override def moveBy(xOffset: Double, yOffset: Double): PictureElement =
    new Bitmap(
      buffer,
      boundary.moveBy(xOffset, yOffset)
    )

  def deepCopy(): Bitmap = new Bitmap(buffer.deepCopy, boundary)

  def getColor(x: Int, y: Int): Color = buffer.pixelColor(x, y)

  def colorAt(x: Int, y: Int): Option[Color] =
    if x < 0 || x >= buffer.width || y < 0 || y >= buffer.height then None
    else Some(getColor(x, y))

  def mergeWith(another: Bitmap, pixelMerger: (Color, Color) => Color): Bitmap =
    val resultWidth  = buffer.width.min(another.buffer.width)
    val resultHeight = buffer.height.min(another.buffer.height)

    val result       = new Bitmap(resultWidth, resultHeight)
    val resultBuffer = result.buffer
    resultBuffer.iterateLocations: (x, y) =>
      val firstColor  = this.getColor(x, y)
      val secondColor = another.getColor(x, y)

      val newColor = pixelMerger(firstColor, secondColor)
      resultBuffer.get.setRGB(x, y, newColor.toARGBInt)

    result
  end mergeWith

  def setColorsByLocation(generator: (Int, Int) => Color): Bitmap =
    val result = deepCopy()
    result.buffer.iterateLocations: (x, y) =>
      val color = generator(x, y)
      result.buffer.get.setRGB(x, y, color.toARGBInt)
    result

  def transformColorToColor(transformer: Color => Color): Bitmap =
    val result = new Bitmap(buffer.width, buffer.height)
    result.buffer.iterateLocations: (x, y) =>
      val color = this.getColor(x, y)
      result.buffer.get.setRGB(x, y, transformer(color).toARGBInt)

    result
  end transformColorToColor

  override def scaleBy(horizontalFactor: Double, verticalFactor: Double): PictureElement =
    scaleBy(horizontalFactor, verticalFactor, position)

  override def scaleBy(
      horizontalFactor: Double,
      verticalFactor: Double,
      relativityPoint: Pos
  ): PictureElement =
    scaleTo(
      horizontalFactor * buffer.width,
      verticalFactor * buffer.height,
      relativityPoint
    )

  override def scaleTo(targetWidth: Double, targetHeight: Double, relativityPoint: Pos): Bitmap =
    if targetWidth == 0 || targetHeight == 0 then return Bitmap()

    val newCenter =
      boundary.center.scaleBy(
        targetWidth / buffer.width,
        targetHeight / buffer.height,
        relativityPoint
      )

    val newBuffer = buffer.scaleTo(targetWidth, targetHeight, newCenter)
    val newUpperLeftCorner =
      newCenter - (targetWidth / 2.0, targetHeight / 2.0)

    val newLowerRightCorner =
      newCenter + (targetWidth / 2.0, targetHeight / 2.0)

    val newBounds = Bounds(newUpperLeftCorner, newLowerRightCorner)

    new Bitmap(newBuffer, newBounds)
  end scaleTo

  override def rotateBy(angle: Double, centerOfRotation: Pos): Bitmap =
    val newBuffer = transformContentUsing(
      AffineTransformation.forPointCentredRotation(
        angle,
        buffer.width / 2.0,
        buffer.height / 2.0
      )
    )

    val oldPositions = Seq(
      boundary.upperLeftCorner,
      boundary.upperRightCorner,
      boundary.lowerLeftCorner,
      boundary.lowerRightCorner
    )
    val newBounds =
      BoundaryCalculator.fromPositions(oldPositions.map(_.rotateBy(angle, centerOfRotation)))

    new Bitmap(newBuffer, newBounds)
  end rotateBy

  override def rotateByAroundOrigo(angle: Double): Bitmap =
    val newBuffer = transformContentUsing(
      AffineTransformation.forPointCentredRotation(angle, buffer.width / 2.0, buffer.height / 2.0)
    )

    val newCenter = boundary.center.rotateByAroundOrigo(angle)
    val newUpperLeftCorner =
      newCenter - (newBuffer.width / 2.0, newBuffer.height / 2.0)
    val newLowerRightCorner =
      newUpperLeftCorner + (newBuffer.width - 1.0, newBuffer.height - 1.0)

    val newBounds = Bounds(newUpperLeftCorner, newLowerRightCorner)

    new Bitmap(newBuffer, newBounds)
  end rotateByAroundOrigo

  /** An internal method to transform the content of this bitmap using a given
    * [[AffineTransformation]].
    *
    * @param transformation
    * @return
    */

  private def transformContentUsing(transformation: AffineTransformation): BufferAdapter =
    buffer.createTransformedVersionWith(
      transformation = transformation,
      resizeCanvasBasedOnTransformation = true
    )

  /** Flips the content of this bitmap horizontally.
    *
    * @return
    */
  def flipHorizontally: Bitmap =
    val newBuffer = transformContentUsing(
      AffineTransformation.forYAxisRelativeHorizontalFlipOf(buffer.width)
    )
    new Bitmap(newBuffer, boundary)

  /** Flips the content of this bitmap vertically.
    *
    * @return
    */
  def flipVertically: Bitmap =
    val newBuffer = transformContentUsing(
      AffineTransformation.forXAxisRelativeVerticalFlipOf(buffer.height)
    )
    new Bitmap(newBuffer, boundary)

  /** Flips the content of this bitmap diagonally.
    *
    * @return
    */
  def flipDiagonally: Bitmap =
    val newBuffer = transformContentUsing(
      AffineTransformation.forOrigoRelativeDiagonalFlipOf(buffer.width, buffer.height)
    )
    new Bitmap(newBuffer, boundary)

  override def crop(
      upperLeftXInPixels: Double,
      upperLeftYInPixels: Double,
      lowerRightXInPixels: Double,
      lowerRightYInPixels: Double
  ): Bitmap =

    val upperLeftX  = upperLeftXInPixels.floor
    val upperLeftY  = upperLeftYInPixels.floor
    val lowerRightX = lowerRightXInPixels.floor
    val lowerRightY = lowerRightYInPixels.floor

    val xMin = upperLeftX.min(lowerRightX)
    val xMax = upperLeftX.max(lowerRightX)
    val yMin = upperLeftY.min(lowerRightY)
    val yMax = upperLeftY.max(lowerRightY)

    require(xMin >= 0, s"X coordinate $xMin is negative")
    require(xMax <= buffer.width, s"X coordinate $xMax is out of bounds")
    require(yMin >= 0, s"Y coordinate $yMin is negative")
    require(yMax <= buffer.height, s"Y coordinate $yMax is out of bounds")

    val resultingWidth  = xMax - xMin + 1
    val resultingHeight = yMax - yMin + 1
    if resultingWidth < 1 || resultingHeight < 1 then return new Bitmap(0, 0)

    val newBuffer = buffer.copyPortionXYXY(xMin, yMin, xMax, yMax)

    val newUpperLeftCorner = Pos.Origo
    val newLowerRightCorner =
      newUpperLeftCorner + (newBuffer.width.doubleValue, newBuffer.height.doubleValue)

    val newBounds = Bounds(newUpperLeftCorner, newLowerRightCorner)

    // TODO: check if working
    new Bitmap(newBuffer, newBounds)

  private final val asciiLookupTable =
    "$@B%8&WM#*oahkbdpqwmZO0QLCJUYXzcvunxrjft/\\|()1{}[]?-_+~<>i!lI;:,\"^`'. ".toCharArray

  def toAsciiArt(height: Int = 15): String =
    val asciiArt    = new StringBuilder()
    val scaledImage = buffer.scaleToHeight(height)
    for y <- 0 until height do
      for x <- 0 until scaledImage.width do
        val pixel = scaledImage.pixelColor(x, y)
        val luminance =
          (pixel.red * 0.2126 + pixel.green * 0.7152 + pixel.blue * 0.0722).toInt
        val asciiIndex = (luminance * (asciiLookupTable.length - 1) / 255).toInt
        asciiArt.append(asciiLookupTable(asciiIndex))
        asciiArt.append("  ")
      asciiArt.append('\n')
    asciiArt.toString

  def toAsciiColorBlocks(height: Int = 15): String =
    val asciiArt    = new StringBuilder()
    val scaledImage = buffer.scaleToHeight(height)
    for y <- 0 until height do
      for x <- 0 until scaledImage.width do
        val pixel     = scaledImage.pixelColor(x, y)
        val asciiChar = if pixel.opacity == 0 then " " else "█"
        asciiArt.append(
          s"\u001b[38;2;${pixel.red};${pixel.green};${pixel.blue}m$asciiChar\u001b[0m"
        )
      asciiArt.append('\n')
    asciiArt.toString

  def saveAsPngTo(path: String): Boolean =
    ResourceFactory.saveBufferedImageToPath(buffer.get, path)
