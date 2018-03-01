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

package smcl.pictures


import smcl.colors.ColorValidator
import smcl.infrastructure.{BitmapBufferAdapter, Displayable, Identity, InjectablesRegistry, PRF}
import smcl.modeling.d2._
import smcl.modeling.{AffineTransformation, Angle, Len}




/**
 *
 *
 * @author Aleksi Lukkarinen
 */
object Bitmap
    extends InjectablesRegistry {

  /** The ColorValidator instance to be used by this object. */
  protected
  lazy val colorValidator: ColorValidator = {
    injectable(InjectablesRegistry.IIdColorValidator).asInstanceOf[ColorValidator]
  }

  /** The BitmapValidator instance to be used by this object. */
  protected
  lazy val bitmapValidator: BitmapValidator = {
    injectable(InjectablesRegistry.IIdBitmapValidator).asInstanceOf[BitmapValidator]
  }

  /**
   *
   *
   * @param elements
   *
   * @return
   */
  @inline
  def apply(elements: PictureElement*): Bitmap =
    RenderingController.createBitmapFrom(elements: _*)

  /**
   *
   *
   * @param widthInPixels
   * @param heightInPixels
   *
   * @return
   */
  @inline
  def apply(
      widthInPixels: Int,
      heightInPixels: Int): Bitmap = {

    apply(
      Len(widthInPixels),
      Len(heightInPixels))
  }

  /**
   *
   *
   * @param width
   * @param height
   *
   * @return
   */
  @inline
  def apply(
      width: Len,
      height: Len): Bitmap = {

    bitmapValidator.validateBitmapSize(width, height)

    val newIdentity: Identity = Identity()

    val buffer =
      if (width > 0 && height > 0)
        Some(PRF.createPlatformBitmapBuffer(width, height))
      else
        None

    apply(newIdentity, Pos.Origo, buffer)
  }

  /**
   *
   *
   * @param sourceResourcePath
   *
   * @return
   */
  @inline
  def apply(sourceResourcePath: String): Bitmap = {
    // The ImageProvider is trusted with validation of the source resource path.
    val loadedBufferTry = PRF.tryToLoadImageFromPath(sourceResourcePath)
    if (loadedBufferTry.isFailure)
      throw loadedBufferTry.failed.get

    apply(loadedBufferTry.get)
  }

  /**
   *
   *
   * @param buffer
   *
   * @return
   */
  @inline
  def apply(buffer: BitmapBufferAdapter): Bitmap = {
    val newIdentity: Identity = Identity()

    apply(newIdentity, Pos.Origo, Option(buffer))
  }

  /**
   *
   *
   * @param center
   * @param buffer
   *
   * @return
   */
  @inline
  private
  def apply(
      center: Pos,
      buffer: Option[BitmapBufferAdapter]): Bitmap = {

    val newIdentity: Identity = Identity()

    apply(newIdentity, center, buffer)
  }

  /**
   *
   *
   * @param identity
   * @param center
   * @param buffer
   *
   * @return
   */
  @inline
  private
  def apply(
      identity: Identity,
      center: Pos,
      buffer: Option[BitmapBufferAdapter]): Bitmap = {

    val isRenderable =
      buffer.isDefined &&
          buffer.get.widthInPixels > 0 &&
          buffer.get.heightInPixels > 0

    val boundary: Bounds =
      if (!isRenderable)
        Bounds.NotDefined
      else
        Bounds.apply(
          center,
          buffer.get.widthInPixels,
          buffer.get.heightInPixels)

    new Bitmap(identity, isRenderable, boundary, buffer)
  }

}




/**
 *
 *
 * @param identity
 * @param isRenderable
 * @param boundary
 * @param buffer
 *
 * @author Aleksi Lukkarinen
 */
class Bitmap private(
    override val identity: Identity,
    val isRenderable: Boolean,
    override val boundary: Bounds,
    private[smcl] val buffer: Option[BitmapBufferAdapter])
    extends PictureElement
        with Displayable {

  /**
   *
   *
   * @return
   */
  @inline
  override
  def isBitmap: Boolean = true

  /**
   *
   *
   * @return
   */
  @inline
  override
  def toBitmap: Bitmap = this

  /**
   *
   *
   * @return
   */
  @inline
  override
  def toBitmapCopy: Bitmap = Bitmap(buffer.map(_.copy).orNull)

  /**
   *
   *
   * @param offsetsInPixels
   *
   * @return
   */
  @inline
  override
  def moveBy(offsetsInPixels: Seq[Double]): PictureElement =
    this //copy(newPosition = boundary.moveBy(offsetsInPixels))

  /**
   *
   *
   * @param xOffsetInPixels
   * @param yOffsetInPixels
   *
   * @return
   */
  @inline
  override
  def moveBy(
      xOffsetInPixels: Double,
      yOffsetInPixels: Double): PictureElement = {

    this //copy(newPosition = boundary.moveBy(xOffsetInPixels, yOffsetInPixels))
  }

  /**
   *
   *
   * @param coordinatesInPixels
   *
   * @return
   */
  override
  def moveUpperLeftCornerTo(coordinatesInPixels: Seq[Double]): PictureElement = {
    require(
      coordinatesInPixels.length == NumberOfDimensions,
      s"Exactly $NumberOfDimensions coordinates must be given (found: ${coordinatesInPixels.length})")

    this //copy(newPosition = boundary.moveUpperLeftCornerTo(coordinatesInPixels))
  }

  /**
   *
   *
   * @param xCoordinateInPixels
   * @param yCoordinateInPixels
   *
   * @return
   */
  override
  def moveUpperLeftCornerTo(
      xCoordinateInPixels: Double,
      yCoordinateInPixels: Double): PictureElement = {

    this //copy(newPosition = boundary.moveUpperLeftCornerTo(xCoordinateInPixels, yCoordinateInPixels))
  }

  /**
   *
   *
   * @param coordinatesInPixels
   *
   * @return
   */
  @inline
  override
  def moveCenterTo(coordinatesInPixels: Seq[Double]): PictureElement = {
    require(
      coordinatesInPixels.length == NumberOfDimensions,
      s"Exactly $NumberOfDimensions coordinates must be given (found: ${coordinatesInPixels.length})")

    this //copy(newPosition = boundary.moveCenterTo(coordinatesInPixels))
  }

  /**
   *
   *
   * @param xCoordinateInPixels
   * @param yCoordinateInPixels
   *
   * @return
   */
  @inline
  override
  def moveCenterTo(
      xCoordinateInPixels: Double,
      yCoordinateInPixels: Double): PictureElement = {

    this //copy(newPosition = boundary.moveCenterTo(xCoordinateInPixels, yCoordinateInPixels))
  }

  /**
   *
   *
   * @param filename
   *
   * @return
   */
  @inline
  def saveAsPngTo(filename: String): String =
    buffer.fold("Error: No BitmapBufferAdapter to save.")(_.saveAsPngTo(filename))

  /**
   *
   * @return
   */
  //@inline
  //private
  //def internalCopy(newPosition: Pos = position): Bitmap =
  //  new Bitmap(identity, isRenderable, newPosition, buffer.map(_.copy))

  /**
   *
   */
  @inline
  override
  def display(): Bitmap = {
    super.display()

    this
  }

  /**
   * Transforms the content of this [[Bitmap]] using the specified affine
   * transformation. The center of this [[Bitmap]] remains unchanged.
   *
   * @param t
   *
   * @return
   */
  def transformContentWith(t: AffineTransformation): Bitmap = {
    if (buffer.isEmpty)
      return this

    val newBuffer = buffer.get.createTransformedVersionWith(
      transformation = t,
      resizeCanvasBasedOnTransformation = false)._1

    new Bitmap(
      identity = identity,
      isRenderable = isRenderable,
      boundary = boundary,
      buffer = Some(newBuffer))
  }

  /**
   * Rotates this object around origo (0,0) by 90 degrees clockwise.
   *
   * @return
   */
  @inline
  override
  def rotateBy90DegsCWAroundOrigo: Bitmap = {
    if (buffer.isEmpty)
      return this

    // TODO

    val (newBuffer, upperLeftOffsets) =
      buffer.get.createTransformedVersionWith(
        transformation = AffineTransformation.forOrigoCentredRotationOf90DegsCW,
        resizeCanvasBasedOnTransformation = true)

    //BoundaryCalculator.fromBoundaries()

    this

  }

  /**
   * Rotates this object around its center by 90 degrees clockwise.
   *
   * @return
   */
  @inline
  override
  def rotateBy90DegsCW: Bitmap = rotateBy90DegsCW(position)

  /**
   * Rotates this object around a given point by 90 degrees clockwise.
   *
   * @param centerOfRotation
   *
   * @return
   */
  @inline
  override
  def rotateBy90DegsCW(centerOfRotation: Pos): Bitmap = {
    if (buffer.isEmpty)
      return this

    this  // TODO
  }

  /**
   * Rotates this object around origo (0,0) by 90 degrees counterclockwise.
   *
   * @return
   */
  @inline
  override
  def rotateBy90DegsCCWAroundOrigo: Bitmap = {
    if (buffer.isEmpty)
      return this

    this  // TODO
  }

  /**
   * Rotates this object around the its center by 90 degrees counterclockwise.
   *
   * @return
   */
  @inline
  override
  def rotateBy90DegsCCW: Bitmap = rotateBy90DegsCCW(position)

  /**
   * Rotates this object around a given point by 90 degrees counterclockwise.
   *
   * @param centerOfRotation
   *
   * @return
   */
  @inline
  override
  def rotateBy90DegsCCW(centerOfRotation: Pos): Bitmap = {
    if (buffer.isEmpty)
      return this

    this  // TODO
  }

  /**
   * Rotates this object around origo (0,0) by 180 degrees.
   *
   * @return
   */
  @inline
  override
  def rotateBy180DegsAroundOrigo: Bitmap = {
    if (buffer.isEmpty)
      return this

    this  // TODO
  }

  /**
   * Rotates this object around its center by 180 degrees.
   *
   * @return
   */
  @inline
  override
  def rotateBy180Degs: Bitmap = rotateBy180Degs(position)

  /**
   * Rotates this object around a given point by 180 degrees.
   *
   * @param centerOfRotation
   *
   * @return
   */
  @inline
  override
  def rotateBy180Degs(centerOfRotation: Pos): Bitmap = {
    if (buffer.isEmpty)
      return this

    this  // TODO
  }

  /**
   * Rotates this object around its center by the specified angle.
   *
   * @param angle
   *
   * @return
   */
  @inline
  override
  def rotateByAroundOrigo(angle: Angle): Bitmap = rotateByAroundOrigo(angle)

  /**
   * Rotates this object around its center by the specified number of degrees.
   *
   * @param angleInDegrees
   *
   * @return
   */
  @inline
  override
  def rotateByAroundOrigo(angleInDegrees: Double): Bitmap = {
    if (buffer.isEmpty)
      return this

    this  // TODO
  }

  /**
   * Rotates this object around its center by the specified angle.
   *
   * @param angle
   *
   * @return
   */
  @inline
  override
  def rotateBy(angle: Angle): Bitmap = rotateBy(angle)

  /**
   * Rotates this object around its center by the specified number of degrees.
   *
   * @param angleInDegrees
   *
   * @return
   */
  @inline
  override
  def rotateBy(angleInDegrees: Double): Bitmap =
    rotateBy(angleInDegrees, position)

  /**
   * Rotates this object around a given point by the specified angle.
   *
   * @param angle
   * @param centerOfRotation
   *
   * @return
   */
  @inline
  override
  def rotateBy(
      angle: Angle,
      centerOfRotation: Pos): Bitmap = {

    rotateBy(angle, centerOfRotation)
  }

  /**
   * Rotates this object around a given point by the specified number of degrees.
   *
   * @param angleInDegrees
   * @param centerOfRotation
   *
   * @return
   */
  @inline
  override
  def rotateBy(
      angleInDegrees: Double,
      centerOfRotation: Pos): Bitmap = {

    if (buffer.isEmpty)
      return this

    val (newBuffer, _) =
      buffer.get.createTransformedVersionWith(
        AffineTransformation.forPointCentredRotation(
          angleInDegrees,
          centerOfRotation))

    new Bitmap(identity, isRenderable, boundary, Some(newBuffer))
  }

  /**
   *
   *
   * @param upperLeftX
   * @param upperLeftY
   * @param lowerRightX
   * @param lowerRightY
   *
   * @return
   */
  override
  def crop(
      upperLeftX: Double,
      upperLeftY: Double,
      lowerRightX: Double,
      lowerRightY: Double): Bitmap = {

    println("crop... to be implemented")
    //Pic(self.crop(topLeft.xInt, topLeft.yInt, (topLeft.x + width - 1).round.toInt, (topLeft.y + height - 1).round.toInt), Center) // XXX pwa // XXX anchor

    /*
    if (self.dimensions == background.dimensions)
      self
    else {
      val (cropX, cropY) = (from.x.floor.toInt, from.y.floor.toInt) // XXX pyöristys vai floor?
      val bgWithCroppedPicOnTop = self.crop(cropX, cropY, cropX + background.width - 1, cropY + background.height - 1)  // XXX -1 ei kiva
      PicWithAnchor(bgWithCroppedPicOnTop, background.anchor)    // XXX pwa
    }
    */

    this
  }

  /**
   *
   *
   * @param widthFactor
   * @param heightFactor
   *
   * @return
   */
  override
  def scaleBy(widthFactor: Double, heightFactor: Double): Bitmap = {
    this
  }

}
