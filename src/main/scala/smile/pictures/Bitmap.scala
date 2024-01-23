package smile.pictures

import smile.colors.Color
import smile.infrastructure.{BufferAdapter, ResourceFactory}
import smile.modeling.{AffineTransformation, BoundaryCalculator, Bounds, Pos}

/** Companion object for the `Bitmap` class, providing factory methods for creating `Bitmap`
  * instances.
  */
object Bitmap:
  /** A type alias for functions generating colors based on pixel locations. */
  type LocationToColorGenerator = (Int, Int) => Color

  /** Creates a `Bitmap` of specified dimensions using a function to generate pixel colors.
    *
    * @param width
    *   The width of the bitmap.
    * @param height
    *   The height of the bitmap.
    * @param contentGenerator
    *   A function to generate colors for each pixel based on its location.
    * @return
    *   A new `Bitmap` instance.
    */
  def apply(
      width: Int,
      height: Int,
      contentGenerator: LocationToColorGenerator
  ): Bitmap =
    new Bitmap(width, height).setColorsByLocation(contentGenerator)

  /** Creates a `Bitmap` from a sequence of colors, assuming row-wise population.
    *
    * @param width
    *   The width of the bitmap.
    * @param height
    *   The height of the bitmap.
    * @param colors
    *   A sequence of colors to populate the bitmap.
    * @return
    *   A new `Bitmap` instance.
    */
  def apply(width: Int, height: Int, colors: Seq[Color]): Bitmap =
    require(
      colors.length == (width * height),
      "The colors sequence must have exactly width * height items"
    )
    val bitmap = new Bitmap(width, height)
    bitmap.buffer.setColorsFromSeq(colors)
    bitmap

  /** Loads a `Bitmap` from a resource path.
    *
    * @param sourceResourcePath
    *   The path to the source image resource.
    * @return
    *   A new `Bitmap` instance loaded from the specified path.
    */
  def apply(sourceResourcePath: String): Bitmap =
    val image = ResourceFactory.bufferedImageFromPath(sourceResourcePath)
    new Bitmap(image, Bounds(Pos.Origin, image.width, image.height))

  /** Creates a `Bitmap` from a collection of picture elements.
    *
    * @param elements
    *   A collection of `PictureElement` instances to render into a bitmap.
    * @return
    *   A `Bitmap` representing the rendered picture elements.
    */
  def apply(elements: PictureElement*): Bitmap = Renderer.createBitmapFrom(elements*)

  /** Creates a `Bitmap` from a `Picture`.
    *
    * @param picture
    *   The `Picture` to render into a bitmap.
    * @return
    *   A `Bitmap` representing the rendered picture.
    */
  def apply(picture: Picture): Bitmap = Renderer.createBitmapFrom(picture)

/** Represents a bitmap image, capable of holding and manipulating pixel data.
  *
  * @param buffer
  *   The underlying buffer storing pixel data.
  * @param bounds
  *   The boundaries of the bitmap.
  */
class Bitmap(val buffer: BufferAdapter, bounds: Bounds) extends PictureElement:

  /** Primary constructor for creating a `Bitmap` with specified dimensions.
    *
    * @param width
    *   The width of the bitmap.
    * @param height
    *   The height of the bitmap.
    */
  def this(width: Int, height: Int) =
    this(new BufferAdapter(width, height), Bounds(Pos.Origin, width, height))
    require(width > 0 && height > 0, "Width and height must be positive")

  override lazy val boundary: Bounds = bounds

  /** Creates a copy of this `Bitmap` at a new position.
    *
    * @param newPosition
    *   The new position for the bitmap.
    * @return
    *   A new `Bitmap` instance at the specified position.
    */
  override def copy(newPosition: Pos): PictureElement =
    new Bitmap(
      buffer,
      boundary.moveBy(newPosition.x, newPosition.y)
    )

  /** Moves this `Bitmap` by specified offsets.
    *
    * @param xOffset
    *   The horizontal offset.
    * @param yOffset
    *   The vertical offset.
    * @return
    *   A new `Bitmap` instance moved by the specified offsets.
    */
  override def moveBy(xOffset: Double, yOffset: Double): PictureElement =
    new Bitmap(
      buffer,
      boundary.moveBy(xOffset, yOffset)
    )

  /** Creates a deep copy of this `Bitmap`. */
  def deepCopy(): Bitmap = new Bitmap(buffer.deepCopy, boundary)

  /** Gets the color of a pixel at specified coordinates. */
  def getColor(x: Int, y: Int): Color = buffer.pixelColor(x, y)

  /** Optionally gets the color at specified coordinates, returning `None` if out of bounds.
    *
    * @param x
    *   The x-coordinate.
    * @param y
    *   The y-coordinate.
    * @return
    *   An `Option[Color]` containing the color if within bounds, or `None`.
    */
  def colorAt(x: Int, y: Int): Option[Color] =
    if x < 0 || x >= buffer.width || y < 0 || y >= buffer.height then None
    else Some(getColor(x, y))

  /** Merges this `Bitmap` with another `Bitmap` using a specified pixel merging function.
    *
    * @param another
    *   The other bitmap to merge with.
    * @param pixelMerger
    *   A function that takes two colors and returns a merged color.
    * @return
    *   A new `Bitmap` instance representing the merged image.
    */
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

  /** Sets the colors of the bitmap based on a function that generates colors for each pixel
    * location.
    *
    * @param generator
    *   A function that generates a color for a given pixel location.
    * @return
    *   The `Bitmap` instance with updated colors.
    */
  def setColorsByLocation(generator: (Int, Int) => Color): Bitmap =
    val result = deepCopy()
    result.buffer.iterateLocations: (x, y) =>
      val color = generator(x, y)
      result.buffer.get.setRGB(x, y, color.toARGBInt)
    result

  /** Transforms each pixel color of this bitmap using a specified color transformer function.
    *
    * @param transformer
    *   A function that takes a color and returns a transformed color.
    * @return
    *   A new `Bitmap` instance with transformed colors.
    */
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

    val newBuffer = buffer.scaleTo(targetWidth, targetHeight)
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

  override def rotateByAroundOrigin(angle: Double): Bitmap =
    val newBuffer = transformContentUsing(
      AffineTransformation.forPointCentredRotation(angle, buffer.width / 2.0, buffer.height / 2.0)
    )

    val newCenter = boundary.center.rotateByAroundOrigin(angle)
    val newUpperLeftCorner =
      newCenter - (newBuffer.width / 2.0, newBuffer.height / 2.0)
    val newLowerRightCorner =
      newUpperLeftCorner + (newBuffer.width - 1.0, newBuffer.height - 1.0)

    val newBounds = Bounds(newUpperLeftCorner, newLowerRightCorner)

    new Bitmap(newBuffer, newBounds)
  end rotateByAroundOrigin

  /** Transforms the content of this bitmap using a specified [[AffineTransformation]]. This method
    * is internal and facilitates various geometric transformations.
    *
    * @param transformation
    *   An [[AffineTransformation]] instance to apply to the bitmap.
    * @return
    *   A new [[BufferAdapter]] containing the transformed pixel data.
    */
  private def transformContentUsing(transformation: AffineTransformation): BufferAdapter =
    buffer.createTransformedVersionWith(
      transformation = transformation,
      resizeCanvasBasedOnTransformation = true
    )

  /**
   * Flips the bitmap content horizontally. This is equivalent to reflecting the image
   * across a vertical axis running through the image's center.
   *
   * @return A new [[Bitmap]] instance with horizontally flipped content.
   */
  def flipHorizontally: Bitmap =
    val newBuffer = transformContentUsing(
      AffineTransformation.forYAxisRelativeHorizontalFlipOf(buffer.width)
    )
    new Bitmap(newBuffer, boundary)

  /**
   * Flips the bitmap content vertically. This is equivalent to reflecting the image
   * across a horizontal axis running through the image's center.
   *
   * @return A new [[Bitmap]] instance with vertically flipped content.
   */
  def flipVertically: Bitmap =
    val newBuffer = transformContentUsing(
      AffineTransformation.forXAxisRelativeVerticalFlipOf(buffer.height)
    )
    new Bitmap(newBuffer, boundary)

  /**
   * Flips the bitmap content diagonally. This is equivalent to rotating the image
   * by 180 degrees around the point at the center of its bounds.
   *
   * @return A new [[Bitmap]] instance with diagonally flipped content.
   */
  def flipDiagonally: Bitmap =
    val newBuffer = transformContentUsing(
      AffineTransformation.forOriginRelativeDiagonalFlipOf(buffer.width, buffer.height)
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

    val newUpperLeftCorner = Pos.Origin
    val newLowerRightCorner =
      newUpperLeftCorner + (newBuffer.width.doubleValue, newBuffer.height.doubleValue)

    val newBounds = Bounds(newUpperLeftCorner, newLowerRightCorner)

    // TODO: check if working
    new Bitmap(newBuffer, newBounds)

  /** Saves this bitmap as a PNG image to a specified path.
    *
    * @param path
    *   The file path where the bitmap should be saved.
    * @return
    *   `true` if the save operation was successful, `false` otherwise.
    */
  def saveAsPngTo(path: String): Boolean =
    ResourceFactory.saveBufferedImageToPath(buffer.get, path)
