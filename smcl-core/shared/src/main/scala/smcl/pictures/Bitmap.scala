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


import scala.util.{Failure, Success, Try}

import smcl.colors.ColorValidator
import smcl.colors.rgb.{Color, ColorComponentTranslationTable}
import smcl.infrastructure.{BitmapBufferAdapter, Displayable, Identity, InjectablesRegistry, PRF}
import smcl.modeling.d2._
import smcl.modeling.{AffineTransformation, Angle, Len}
import smcl.pictures.filters._
import smcl.settings
import smcl.settings.DefaultBackgroundColor




/**
 *
 *
 * @author Aleksi Lukkarinen
 */
object Bitmap
    extends InjectablesRegistry {

  /** */
  type LocationToColorGenerator = (Int, Int) => Color

  /** */
  val BackgroundColorGenerator: LocationToColorGenerator = (_, _) => DefaultBackgroundColor

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
  def apply(elements: PictureElement*): Bitmap =
    RenderingController.createBitmapFrom(elements: _*)

  /**
   *
   *
   * @param widthInPixels
   * @param heightInPixels
   * @param contentGenerator
   *
   * @return
   */
  def apply(
      widthInPixels: Int,
      heightInPixels: Int,
      contentGenerator: LocationToColorGenerator): Bitmap = {

    apply(widthInPixels, heightInPixels).setColorsByLocation(contentGenerator)
  }

  /**
   *
   *
   * @param widthInPixels
   * @param heightInPixels
   *
   * @return
   */
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
   * @param contentGenerator
   *
   * @return
   */
  def apply(
      width: Len,
      height: Len,
      contentGenerator: LocationToColorGenerator): Bitmap = {

    apply(width, height).setColorsByLocation(contentGenerator)
  }

  /**
   *
   *
   * @param width
   * @param height
   *
   * @return
   */
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
  def apply(sourceResourcePath: String): Bitmap = {
    processSingleLoadTry(PRF.tryToLoadImage(sourceResourcePath))
  }

  /**
   *
   *
   * @param sourceResourcePath
   *
   * @return
   */
  def loadImages(sourceResourcePath: String): Seq[Try[Bitmap]] = {
    processMultipleLoadTries(PRF.tryToLoadImages(sourceResourcePath))
  }

  /**
   *
   *
   * @param sourceResourcePath
   *
   * @return
   */
  def loadImageFromLocalPath(sourceResourcePath: String): Bitmap = {
    processSingleLoadTry(PRF.tryToLoadImageFromLocalPath(sourceResourcePath))
  }

  /**
   *
   *
   * @param sourceResourcePath
   *
   * @return
   */
  def loadImagesFromLocalPath(sourceResourcePath: String): Seq[Try[Bitmap]] = {
    processMultipleLoadTries(PRF.tryToLoadImagesFromLocalPath(sourceResourcePath))
  }

  /**
   *
   *
   * @param relativeSourceResourcePath
   *
   * @return
   */
  def loadImageFromResources(relativeSourceResourcePath: String): Bitmap = {
    processSingleLoadTry(PRF.tryToLoadImageFromResources(relativeSourceResourcePath))
  }

  /**
   *
   *
   * @param relativeSourceResourcePath
   *
   * @return
   */
  def loadImagesFromResources(relativeSourceResourcePath: String): Seq[Try[Bitmap]] = {
    processMultipleLoadTries(PRF.tryToLoadImagesFromResources(relativeSourceResourcePath))
  }

  /**
   *
   *
   * @param absoluteSourceResourcePath
   *
   * @return
   */
  def loadImageFromServer(absoluteSourceResourcePath: String): Bitmap = {
    processSingleLoadTry(PRF.tryToLoadImageFromServer(absoluteSourceResourcePath))
  }

  /**
   *
   *
   * @param absoluteSourceResourcePath
   *
   * @return
   */
  def loadImagesFromServer(absoluteSourceResourcePath: String): Seq[Try[Bitmap]] = {
    processMultipleLoadTries(PRF.tryToLoadImagesFromServer(absoluteSourceResourcePath))
  }

  /**
   *
   *
   * @param singleResult
   *
   * @return
   */
  private
  def processSingleLoadTry(singleResult: Try[BitmapBufferAdapter]): Bitmap = {
    if (singleResult.isFailure)
      throw singleResult.failed.get

    apply(singleResult.get)
  }

  /**
   *
   *
   * @param wholenessResult
   *
   * @return
   */
  private
  def processMultipleLoadTries(
      wholenessResult: Try[Seq[Try[BitmapBufferAdapter]]]): Seq[Try[Bitmap]] = {

    if (wholenessResult.isFailure)
      throw wholenessResult.failed.get

    wholenessResult.get.map{t =>
      if (t.isSuccess)
        Success(apply(t.get))
      else
        Failure(t.failed.get)
    }
  }

  /**
   *
   *
   * @param buffer
   *
   * @return
   */
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

    new Bitmap(
      identity,
      isRenderable,
      boundary,
      buffer)
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
  override
  def isBitmap: Boolean = true

  /**
   *
   *
   * @return
   */
  override
  def toBitmap: Bitmap = this

  /**
   *
   *
   * @return
   */
  override
  def toBitmapCopy: Bitmap = {
    val newBuffer = buffer.map(_.copy).orNull

    internalCopy(newBuffer = Option(newBuffer))
  }

  /**
   *
   *
   * @param another
   * @param pixelMerger
   *
   * @return
   */
  override
  def mergePixelsWith(
      another: PictureElement,
      pixelMerger: (Color, Color) => Color): Bitmap = {

    val mergedSnapshot = toPixelSnapshot.mergeWith(
      another.toPixelSnapshot,
      pixelMerger)

    mergedSnapshot.toBitmap
  }

  /**
   *
   *
   * @param translator
   *
   * @return
   */
  def translateColorsWith(translator: ColorComponentTranslationTable): Bitmap =
    toProvideModifiedCopyOfOldBuffer{oldBuffer =>
      oldBuffer.createFilteredVersionWith(translator)
    }

  /**
   *
   *
   * @param translator
   *
   * @return
   */
  private[pictures]
  def translateColorsWith(translator: (Int, Int, Int, Int) => (Int, Int, Int, Int)): Bitmap =
    toProvideModifiedCopyOfOldBuffer{oldBuffer =>
      val newBuffer = oldBuffer.copy

      val (reds, greens, blues, opacities) = newBuffer.colorComponentArrays

      for (i <- reds.indices) {
        val (newRed, newGreen, newBlue, newOpacity) =
          translator(reds(i), greens(i), blues(i), opacities(i))

        reds(i) = newRed
        greens(i) = newGreen
        blues(i) = newBlue
        opacities(i) = newOpacity
      }

      newBuffer.setColorComponentArrays(reds, greens, blues, opacities)
      newBuffer
    }

  /**
   *
   *
   * @param newCopyProvider
   *
   * @return
   */
  private
  def toProvideModifiedCopyOfOldBuffer(
      newCopyProvider: BitmapBufferAdapter => BitmapBufferAdapter): Bitmap = {

    if (buffer.isEmpty)
      return this

    val oldBuffer = buffer.get
    if (oldBuffer.widthInPixels <= 0 || oldBuffer.heightInPixels <= 0)
      return this

    val newBuffer = newCopyProvider(oldBuffer)

    Bitmap(identity, position, Some(newBuffer))
  }

  /**
   *
   *
   * @param xInPixels
   * @param yInPixels
   *
   * @return
   */
  def colorAt(
      xInPixels: Double,
      yInPixels: Double): Option[Color] = {

    buffer.map(_.colorAt(xInPixels, yInPixels))
  }

  /**
   *
   *
   * @param f
   *
   * @return
   */
  def applySimpleFilter(f: Filter): Bitmap = f(this).toBitmap

  /**
   *
   *
   * @return
   */
  def keepOnlyRedComponent: Bitmap =
    applySimpleFilter(KeepOnlyRedComponent)

  /**
   *
   *
   * @return
   */
  def keepOnlyRedAndGreenComponents: Bitmap =
    applySimpleFilter(KeepOnlyRedAndGreenComponents)

  /**
   *
   *
   * @return
   */
  def keepOnlyRedAndBlueComponents: Bitmap =
    applySimpleFilter(KeepOnlyRedAndBlueComponents)

  /**
   *
   *
   * @return
   */
  def keepOnlyGreenComponent: Bitmap =
    applySimpleFilter(KeepOnlyGreenComponent)

  /**
   *
   *
   * @return
   */
  def keepOnlyGreenAndBlueComponents: Bitmap =
    applySimpleFilter(KeepOnlyGreenAndBlueComponents)

  /**
   *
   *
   * @return
   */
  def keepOnlyBlueComponent: Bitmap =
    applySimpleFilter(KeepOnlyBlueComponent)

  /**
   *
   *
   * @return
   */
  def negate: Bitmap =
    applySimpleFilter(Negate)

  /**
   *
   *
   * @return
   */
  def negateRedComponent: Bitmap =
    applySimpleFilter(NegateRedComponent)

  /**
   *
   *
   * @return
   */
  def negateRedAndGreenComponents: Bitmap =
    applySimpleFilter(NegateRedAndGreenComponents)

  /**
   *
   *
   * @return
   */
  def negateRedAndBlueComponents: Bitmap =
    applySimpleFilter(NegateRedAndBlueComponents)

  /**
   *
   *
   * @return
   */
  def negateGreenComponent: Bitmap =
    applySimpleFilter(NegateGreenComponent)

  /**
   *
   *
   * @return
   */
  def negateGreenAndBlueComponents: Bitmap =
    applySimpleFilter(NegateGreenAndBlueComponents)

  /**
   *
   *
   * @return
   */
  def negateBlueComponent: Bitmap =
    applySimpleFilter(NegateBlueComponent)

  /**
   *
   *
   * @return
   */
  def toGrayscaleByLightness: Bitmap =
    applySimpleFilter(ToGrayscaleByLightness)

  /**
   *
   *
   * @return
   */
  def toGrayscaleByLuminocity: Bitmap =
    applySimpleFilter(ToGrayscaleByLuminocity)

  /**
   *
   *
   * @param redWeight
   * @param greenWeight
   * @param blueWeight
   *
   * @return
   */
  def toGrayscale(
      redWeight: Double,
      greenWeight: Double,
      blueWeight: Double): Bitmap = {

    val filter = ToWeightedGrayscale(redWeight, greenWeight, blueWeight)

    applySimpleFilter(filter)
  }

  /**
   *
   *
   * @param strengthAsPercentage
   *
   * @return
   */
  def posterize(strengthAsPercentage: Int): Bitmap =
    applySimpleFilter(Posterize(strengthAsPercentage))

  /**
   *
   *
   * @param generator
   *
   * @return
   */
  def setColorsByLocation(generator: (Int, Int) => Color): Bitmap =
    withPixelSnapshot(_.setColorsByLocation(generator))

  /**
   *
   *
   * @param transformers
   *
   * @return
   */
  def transformColorToColor(transformers: Seq[Color => Color]): Bitmap =
    withPixelSnapshot(_.transformColorToColor(transformers))

  /**
   *
   *
   * @param transformer
   *
   * @return
   */
  def transformColorToColor(transformer: Color => Color): Bitmap =
    withPixelSnapshot(_.transformColorToColor(transformer))

  /**
   *
   *
   * @param transformers
   *
   * @return
   */
  def transformLocationColorToColor(transformers: Seq[(Int, Int, Color) => Color]): Bitmap =
    withPixelSnapshot(_.transformLocationColorToColor(transformers))

  /**
   *
   *
   * @param transformer
   *
   * @return
   */
  def transformLocationColorToColor(transformer: (Int, Int, Color) => Color): Bitmap =
    withPixelSnapshot(_.transformLocationColorToColor(transformer))

  /**
   *
   *
   * @param transformers
   *
   * @return
   */
  def iteratePixels(transformers: Seq[Pixel => Unit]): Bitmap =
    withPixelSnapshot(_.iteratePixels(transformers))

  /**
   *
   *
   * @param transformer
   *
   * @return
   */
  def iteratePixels(transformer: Pixel => Unit): Bitmap =
    withPixelSnapshot(_.iteratePixels(transformer))

  /**
   *
   *
   * @param f
   *
   * @return
   */
  def withPixelSnapshot(f: PixelSnapshot => Unit): Bitmap = {
    val snapshot = PixelSnapshot(toBitmapCopy)
    f(snapshot)
    snapshot.toBitmap
  }

  /**
   *
   *
   * @param offsetsInPixels
   *
   * @return
   */
  override
  def moveBy(offsetsInPixels: Seq[Double]): Bitmap =
    internalBufferPreservingCopy(
      newBoundary = boundary.moveBy(offsetsInPixels))

  /**
   *
   *
   * @param xOffsetInPixels
   * @param yOffsetInPixels
   *
   * @return
   */
  override
  def moveBy(
      xOffsetInPixels: Double,
      yOffsetInPixels: Double): Bitmap = {

    internalBufferPreservingCopy(
      newBoundary = boundary.moveBy(xOffsetInPixels, yOffsetInPixels))
  }


  /**
   *
   *
   * @param position
   * @param positionType
   *
   * @return
   */
  override
  def moveTo(
      position: Pos,
      positionType: settings.PositionType): Bitmap = {

    super.moveTo(position, positionType).asInstanceOf[Bitmap]
  }

  /**
   *
   *
   * @param xCoordinateInPixels
   * @param yCoordinateInPixels
   * @param positionType
   *
   * @return
   */
  override
  def moveTo(
      xCoordinateInPixels: Double,
      yCoordinateInPixels: Double,
      positionType: settings.PositionType): Bitmap = {

    super.moveTo(xCoordinateInPixels, yCoordinateInPixels, positionType)
        .asInstanceOf[Bitmap]
  }

  /**
   *
   *
   * @param coordinatesInPixels
   *
   * @return
   */
  override
  def moveUpperLeftCornerTo(coordinatesInPixels: Seq[Double]): Bitmap = {
    require(
      coordinatesInPixels.length == NumberOfDimensions,
      s"Exactly $NumberOfDimensions coordinates must be given (found: ${coordinatesInPixels.length})")

    moveBy(
      coordinatesInPixels.head - boundary.upperLeftCorner.xInPixels,
      coordinatesInPixels.tail.head - boundary.upperLeftCorner.yInPixels)
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
      yCoordinateInPixels: Double): Bitmap = {

    moveBy(
      xCoordinateInPixels - boundary.upperLeftCorner.xInPixels,
      yCoordinateInPixels - boundary.upperLeftCorner.yInPixels)
  }

  /**
   *
   *
   * @param coordinatesInPixels
   *
   * @return
   */
  override
  def moveCenterTo(coordinatesInPixels: Seq[Double]): Bitmap = {
    require(
      coordinatesInPixels.length == NumberOfDimensions,
      s"Exactly $NumberOfDimensions coordinates must be given (found: ${coordinatesInPixels.length})")

    moveBy(
      coordinatesInPixels.head - boundary.center.xInPixels,
      coordinatesInPixels.tail.head - boundary.center.yInPixels)
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
  def moveCenterTo(
      xCoordinateInPixels: Double,
      yCoordinateInPixels: Double): Bitmap = {

    moveBy(
      xCoordinateInPixels - boundary.center.xInPixels,
      yCoordinateInPixels - boundary.center.yInPixels)
  }

  /**
   *
   *
   * @param filename
   *
   * @return
   */
  def saveAsPngTo(filename: String): String =
    buffer.fold("Error: No BitmapBufferAdapter to save.")(_.saveAsPngTo(filename))

  /**
   * Creates a copy of this bitmap with given arguments.
   *
   * This is an unsafe method, as it can be used to create [[Bitmap]] instances,
   * whose internal state is incoherent. As such, it is not for public use.
   *
   * @param newIdentity
   * @param newIsRenderable
   * @param newBoundary
   * @param newBuffer
   *
   * @return
   */
  private
  def internalCopy(
      newIdentity: Identity = identity,
      newIsRenderable: Boolean = isRenderable,
      newBoundary: Bounds = boundary,
      newBuffer: Option[BitmapBufferAdapter] = buffer): Bitmap = {

    new Bitmap(
      newIdentity,
      newIsRenderable,
      newBoundary,
      newBuffer) // e.g., buffer.map(_.copy) to make a new copy of the internal buffer
  }

  /**
   * Creates a copy of this bitmap with given arguments.
   *
   * This is an unsafe method, as it can be used to create [[Bitmap]] instances,
   * whose internal state is incoherent. As such, it is not for public use.
   *
   * @param newIdentity
   * @param newIsRenderable
   * @param newBoundary
   *
   * @return
   */
  private
  def internalBufferPreservingCopy(
      newIdentity: Identity = identity,
      newIsRenderable: Boolean = isRenderable,
      newBoundary: Bounds = boundary): Bitmap = {

    internalCopy(
      newIdentity,
      newIsRenderable,
      newBoundary,
      buffer)
  }

  /**
   *
   */
  override
  def display(): Bitmap = {
    super.display()

    this
  }

  /**
   * An internal method to transform the content of this
   * bitmap using a given [[AffineTransformation]].
   *
   * @param transformation
   *
   * @return
   */
  @inline
  private final
  def transformContentUsing(transformation: AffineTransformation): BitmapBufferAdapter = {
    val (newBuffer, _) =
      buffer.get.createTransformedVersionWith(
        transformation = transformation,
        resizeCanvasBasedOnTransformation = true)

    newBuffer
  }

  /**
   * Flips the content of this bitmap horizontally.
   *
   * @return
   */
  def flipHorizontally: Bitmap = {
    if (isNotRenderable)
      return this

    val newBuffer = transformContentUsing(
      AffineTransformation.forYAxisRelativeHorizontalFlipOf(width.inPixels))

    internalCopy(
      identity,
      isRenderable,
      boundary,
      Option(newBuffer))
  }

  /**
   * Flips the content of this bitmap vertically.
   *
   * @return
   */
  def flipVertically: Bitmap = {
    if (isNotRenderable)
      return this

    val newBuffer = transformContentUsing(
      AffineTransformation.forXAxisRelativeVerticalFlipOf(height.inPixels))

    internalCopy(
      identity,
      isRenderable,
      boundary,
      Option(newBuffer))
  }

  /**
   * Flips the content of this bitmap diagonally.
   *
   * @return
   */
  def flipDiagonally: Bitmap = {
    if (isNotRenderable)
      return this

    val newBuffer = transformContentUsing(
      AffineTransformation.forOrigoRelativeDiagonalFlipOf(
        width.inPixels,
        height.inPixels))

    internalCopy(
      identity,
      isRenderable,
      boundary,
      Option(newBuffer))
  }

  /**
   * Rotates this bitmap around origo (0,0) by 90 degrees clockwise.
   *
   * @return
   */
  override
  def rotateBy90DegsCWAroundOrigo: Bitmap = {
    if (isNotRenderable)
      return this

    val newBuffer = transformContentUsing(
      AffineTransformation.forPointCentredRotationOf90DegsCW(
        width.half.inPixels, height.half.inPixels))

    val newBounds = boundary.rotateBy90DegsCWAroundOrigo

    internalCopy(
      identity,
      isRenderable,
      newBounds,
      Option(newBuffer))
  }

  /**
   * Rotates this bitmap around its center by 90 degrees clockwise.
   *
   * @return
   */
  override
  def rotateBy90DegsCW: Bitmap = rotateBy90DegsCW(position)

  /**
   * Rotates this bitmap around a given point by 90 degrees clockwise.
   *
   * @param centerOfRotation
   *
   * @return
   */
  override
  def rotateBy90DegsCW(centerOfRotation: Pos): Bitmap = {
    if (isNotRenderable)
      return this

    val newBuffer = transformContentUsing(
      AffineTransformation.forPointCentredRotationOf90DegsCW(
        width.half.inPixels, height.half.inPixels))

    val newBounds = boundary.rotateBy90DegsCW(position)

    internalCopy(
      identity,
      isRenderable,
      newBounds,
      Option(newBuffer))
  }

  /**
   * Rotates this bitmap around origo (0,0) by 90 degrees counterclockwise.
   *
   * @return
   */
  override
  def rotateBy90DegsCCWAroundOrigo: Bitmap = {
    if (isNotRenderable)
      return this

    val newBuffer = transformContentUsing(
      AffineTransformation.forPointCentredRotationOf90DegsCCW(
        width.half.inPixels, height.half.inPixels))

    val newBounds = boundary.rotateBy90DegsCCWAroundOrigo

    internalCopy(
      identity,
      isRenderable,
      newBounds,
      Option(newBuffer))
  }

  /**
   * Rotates this bitmap around the its center by 90 degrees counterclockwise.
   *
   * @return
   */
  override
  def rotateBy90DegsCCW: Bitmap = rotateBy90DegsCCW(position)

  /**
   * Rotates this bitmap around a given point by 90 degrees counterclockwise.
   *
   * @param centerOfRotation
   *
   * @return
   */
  override
  def rotateBy90DegsCCW(centerOfRotation: Pos): Bitmap = {
    if (isNotRenderable)
      return this

    val newBuffer = transformContentUsing(
      AffineTransformation.forPointCentredRotationOf90DegsCCW(
        width.half.inPixels, height.half.inPixels))

    val newBounds = boundary.rotateBy90DegsCCW(position)

    internalCopy(
      identity,
      isRenderable,
      newBounds,
      Option(newBuffer))
  }

  /**
   * Rotates this bitmap around origo (0,0) by 180 degrees.
   *
   * @return
   */
  override
  def rotateBy180DegsAroundOrigo: Bitmap = {
    if (isNotRenderable)
      return this

    val newBuffer = transformContentUsing(
      AffineTransformation.forPointCentredRotationOf180Degs(
        width.half.inPixels, height.half.inPixels))

    val newBounds = boundary.rotateBy180DegsAroundOrigo

    internalCopy(
      identity,
      isRenderable,
      newBounds,
      Option(newBuffer))
  }

  /**
   * Rotates this bitmap around its center by 180 degrees.
   *
   * @return
   */
  override
  def rotateBy180Degs: Bitmap = rotateBy180Degs(position)

  /**
   * Rotates this bitmap around a given point by 180 degrees.
   *
   * @param centerOfRotation
   *
   * @return
   */
  override
  def rotateBy180Degs(centerOfRotation: Pos): Bitmap = {
    if (isNotRenderable)
      return this

    val newBuffer = transformContentUsing(
      AffineTransformation.forPointCentredRotationOf180Degs(
        width.half.inPixels, height.half.inPixels))

    val newBounds = boundary.rotateBy180Degs(position)

    internalCopy(
      identity,
      isRenderable,
      newBounds,
      Option(newBuffer))
  }

  /**
   * Rotates this bitmap around its center by the specified angle.
   *
   * @param angle
   *
   * @return
   */
  override
  def rotateByAroundOrigo(angle: Angle): Bitmap = rotateByAroundOrigo(angle)

  /**
   * Rotates this bitmap around its center by the specified number of degrees.
   *
   * @param angleInDegrees
   *
   * @return
   */
  override
  def rotateByAroundOrigo(angleInDegrees: Double): Bitmap = {
    if (isNotRenderable)
      return this

    val newBuffer = transformContentUsing(
      AffineTransformation.forPointCentredRotation(
        angleInDegrees,
        width.half.inPixels, height.half.inPixels))

    val newCenter = boundary.center.rotateByAroundOrigo(angleInDegrees)
    val newUpperLeftCorner = newCenter - (newBuffer.widthInPixels / 2.0, newBuffer.heightInPixels / 2.0)
    val newLowerRightCorner = newUpperLeftCorner + (newBuffer.widthInPixels - 1, newBuffer.heightInPixels - 1)

    val newBounds = Bounds(newUpperLeftCorner, newLowerRightCorner)

    internalCopy(
      identity,
      isRenderable,
      newBounds,
      Option(newBuffer))
  }

  /**
   * Rotates this bitmap around its center by the specified angle.
   *
   * @param angle
   *
   * @return
   */
  override
  def rotateBy(angle: Angle): Bitmap = rotateBy(angle)

  /**
   * Rotates this bitmap around its center by the specified number of degrees.
   *
   * @param angleInDegrees
   *
   * @return
   */
  override
  def rotateBy(angleInDegrees: Double): Bitmap =
    rotateBy(angleInDegrees, position)

  /**
   * Rotates this bitmap around a given point by the specified angle.
   *
   * @param angle
   * @param centerOfRotation
   *
   * @return
   */
  override
  def rotateBy(
      angle: Angle,
      centerOfRotation: Pos): Bitmap = {

    rotateBy(angle, centerOfRotation)
  }

  /**
   * Rotates this bitmap around a given point by the specified number of degrees.
   *
   * @param angleInDegrees
   * @param centerOfRotation
   *
   * @return
   */
  override
  def rotateBy(
      angleInDegrees: Double,
      centerOfRotation: Pos): Bitmap = {

    if (isNotRenderable)
      return this

    // TODO in all rotation methods: Check params, e.g., Pos has to be defined, Angle mustn't be null

    val newBuffer = transformContentUsing(
      AffineTransformation.forPointCentredRotation(
        angleInDegrees,
        width.half.inPixels, height.half.inPixels))

    val newCenter = boundary.center.rotateByAroundOrigo(angleInDegrees)
    val newUpperLeftCorner = newCenter - (newBuffer.widthInPixels / 2.0, newBuffer.heightInPixels / 2.0)
    val newLowerRightCorner = newUpperLeftCorner + (newBuffer.widthInPixels - 1, newBuffer.heightInPixels - 1)

    val newBounds = Bounds(newUpperLeftCorner, newLowerRightCorner)

    internalCopy(
      identity,
      isRenderable,
      newBounds,
      Option(newBuffer))
  }

  /**
   *
   *
   * @param upperLeftXInPixels
   * @param upperLeftYInPixels
   * @param lowerRightXInPixels
   * @param lowerRightYInPixels
   *
   * @return
   */
  override
  def crop(
      upperLeftXInPixels: Double,
      upperLeftYInPixels: Double,
      lowerRightXInPixels: Double,
      lowerRightYInPixels: Double): Bitmap = {

    if (buffer.isEmpty)
      return this

    // TODO: Check parameters: Have to be inside the bitmap

    val newBuffer = buffer.get.copyPortionXYXY(
      upperLeftXInPixels, upperLeftYInPixels,
      lowerRightXInPixels, lowerRightYInPixels)

    val newUpperLeftCorner = Pos.Origo

    val newLowerRightCorner =
      newUpperLeftCorner + (newBuffer.widthInPixels - 1, newBuffer.heightInPixels - 1)

    val newBounds = Bounds(newUpperLeftCorner, newLowerRightCorner)

    internalCopy(
      identity,
      isRenderable,
      newBounds,
      Option(newBuffer))
  }

  /**
   *
   *
   * @param upperLeftCorner
   * @param widthInPixels
   * @param heightInPixels
   *
   * @return
   */
  override
  def crop(
      upperLeftCorner: Pos,
      widthInPixels: Double,
      heightInPixels: Double): Bitmap = {

    if (buffer.isEmpty)
      return this

    // TODO: Check parameters: Have to be inside the bitmap

    val newBuffer = buffer.get.copyPortionXYWH(
      upperLeftCorner.xInPixels, upperLeftCorner.yInPixels,
      widthInPixels, heightInPixels)

    val newUpperLeftCorner = Pos.Origo
    val newLowerRightCorner =
      newUpperLeftCorner + (newBuffer.widthInPixels - 1, newBuffer.heightInPixels - 1)

    val newBounds = Bounds(newUpperLeftCorner, newLowerRightCorner)

    internalCopy(
      identity,
      isRenderable,
      newBounds,
      Option(newBuffer))
  }

  /**
   * Scales this object to a given width in relation to its center.
   *
   * @param targetWidth
   *
   * @return
   */
  override
  def scaleHorizontallyTo(targetWidth: Double): Bitmap =
    scaleHorizontallyTo(targetWidth, position)

  /**
   * Scales this object to a given width in relation to a given point.
   *
   * @param targetWidth
   * @param relativityPoint
   *
   * @return
   */
  override
  def scaleHorizontallyTo(
      targetWidth: Double,
      relativityPoint: Pos): Bitmap = {

    scaleTo(
      targetWidth,
      targetHeight = height.inPixels,
      relativityPoint = relativityPoint)
  }

  /**
   * Scales this object to a given width in relation to the origo.
   *
   * @param targetWidth
   *
   * @return
   */
  override
  def scaleHorizontallyToRelativeToOrigo(targetWidth: Double): Bitmap =
    scaleToRelativeToOrigo(
      targetWidth,
      targetHeight = height.inPixels)

  /**
   * Scales this object to a given height in relation to its center.
   *
   * @param targetHeight
   *
   * @return
   */
  override
  def scaleVerticallyTo(targetHeight: Double): Bitmap =
    scaleVerticallyTo(targetHeight, position)

  /**
   * Scales this object to a given height in relation to a given point.
   *
   * @param targetHeight
   * @param relativityPoint
   *
   * @return
   */
  override
  def scaleVerticallyTo(
      targetHeight: Double,
      relativityPoint: Pos): Bitmap = {

    scaleTo(
      targetWidth = width.inPixels,
      targetHeight = targetHeight,
      relativityPoint = relativityPoint)
  }

  /**
   * Scales this object to a given height in relation to the origo.
   *
   * @param targetHeight
   *
   * @return
   */
  override
  def scaleVerticallyToRelativeToOrigo(targetHeight: Double): Bitmap =
    scaleToRelativeToOrigo(
      targetWidth = width.inPixels,
      targetHeight = targetHeight)

  /**
   * Scales this object in relation to its center by
   * using a single length for both width and height.
   *
   * @param targetSideLength
   *
   * @return
   */
  override
  def scaleTo(targetSideLength: Double): Bitmap =
    scaleTo(targetSideLength, position)

  /**
   * Scales this object in relation to a given point by
   * using a single length for both width and height.
   *
   * @param targetSideLength
   * @param relativityPoint
   *
   * @return
   */
  override
  def scaleTo(
      targetSideLength: Double,
      relativityPoint: Pos): Bitmap = {

    scaleTo(
      targetWidth = targetSideLength,
      targetHeight = targetSideLength,
      relativityPoint = relativityPoint)
  }

  /**
   * Scales this object in relation to the origo by
   * using a single length for both width and height.
   *
   * @param targetSideLength
   *
   * @return
   */
  override
  def scaleToRelativeToOrigo(targetSideLength: Double): Bitmap =
    scaleToRelativeToOrigo(
      targetWidth = targetSideLength,
      targetHeight = targetSideLength)

  /**
   * Scales this object to given width and height in relation to its center.
   *
   * @param targetWidth
   * @param targetHeight
   *
   * @return
   */
  override
  def scaleTo(
      targetWidth: Double,
      targetHeight: Double): Bitmap = {

    scaleTo(targetWidth, targetHeight, position)
  }

  /**
   * Scales this object to given width and height in relation to a given point.
   *
   * @param targetWidth
   * @param targetHeight
   * @param relativityPoint
   *
   * @return
   */
  override
  def scaleTo(
      targetWidth: Double,
      targetHeight: Double,
      relativityPoint: Pos): Bitmap = {

    val (horizontalFactor, verticalFactor) =
      scalingFactorsFor(targetWidth, targetHeight)

    scaleBy(horizontalFactor, verticalFactor, relativityPoint)
  }

  /**
   * Scales this object to given width and height in relation to the origo.
   *
   * @param targetWidth
   * @param targetHeight
   *
   * @return
   */
  override
  def scaleToRelativeToOrigo(
      targetWidth: Double,
      targetHeight: Double): Bitmap = {

    val (horizontalFactor, verticalFactor) =
      scalingFactorsFor(targetWidth, targetHeight)

    scaleByRelativeToOrigo(horizontalFactor, verticalFactor)
  }

  /**
   *
   *
   * @param targetWidth
   * @param targetHeight
   *
   * @return
   */
  def scalingFactorsFor(
      targetWidth: Double,
      targetHeight: Double): (Double, Double) = {

    val horizontalFactor = targetWidth / width.inPixels
    val verticalFactor = targetHeight / height.inPixels

    (horizontalFactor, verticalFactor)
  }

  /**
   * Scales this object horizontally in relation to its center.
   *
   * @param factor
   *
   * @return
   */
  override
  def scaleHorizontallyBy(factor: Double): Bitmap =
    scaleHorizontallyBy(factor, position)

  /**
   * Scales this object horizontally in relation to a given point.
   *
   * @param factor
   * @param relativityPoint
   *
   * @return
   */
  override
  def scaleHorizontallyBy(
      factor: Double,
      relativityPoint: Pos): Bitmap = {

    scaleBy(
      horizontalFactor = factor,
      verticalFactor = Scalable.IdentityScalingFactor,
      relativityPoint = relativityPoint)
  }

  /**
   * Scales this object horizontally in relation to the origo.
   *
   * @param factor
   *
   * @return
   */
  override
  def scaleHorizontallyByRelativeToOrigo(factor: Double): Bitmap =
    scaleByRelativeToOrigo(
      horizontalFactor = factor,
      verticalFactor = Scalable.IdentityScalingFactor)

  /**
   * Scales this object vertically in relation to its center.
   *
   * @param factor
   *
   * @return
   */
  override
  def scaleVerticallyBy(factor: Double): Bitmap =
    scaleVerticallyBy(factor, position)

  /**
   * Scales this object vertically in relation to a given point.
   *
   * @param factor
   * @param relativityPoint
   *
   * @return
   */
  override
  def scaleVerticallyBy(
      factor: Double,
      relativityPoint: Pos): Bitmap = {

    scaleBy(
      horizontalFactor = Scalable.IdentityScalingFactor,
      verticalFactor = factor,
      relativityPoint = relativityPoint)
  }

  /**
   * Scales this object vertically in relation to the origo.
   *
   * @param factor
   *
   * @return
   */
  override
  def scaleVerticallyByRelativeToOrigo(factor: Double): Bitmap =
    scaleByRelativeToOrigo(
      horizontalFactor = Scalable.IdentityScalingFactor,
      verticalFactor = factor)

  /**
   * Scales this object in relation to its center by using a given factor
   * for both horizontal and vertical directions.
   *
   * @param factor
   *
   * @return
   */
  override
  def scaleBy(factor: Double): Bitmap =
    scaleBy(factor, position)

  /**
   * Scales this object in relation to a given point by using a given factor
   * for both horizontal and vertical directions.
   *
   * @param factor
   * @param relativityPoint
   *
   * @return
   */
  override
  def scaleBy(
      factor: Double,
      relativityPoint: Pos): Bitmap = {

    scaleBy(
      horizontalFactor = factor,
      verticalFactor = factor,
      relativityPoint = relativityPoint)
  }

  /**
   * Scales this object in relation to the origo by using a given factor for
   * both horizontal and vertical directions.
   *
   * @param factor
   *
   * @return
   */
  override
  def scaleByRelativeToOrigo(factor: Double): Bitmap =
    scaleByRelativeToOrigo(
      horizontalFactor = factor,
      verticalFactor = factor)

  /**
   * Scales this object by given horizontal and vertical factors in relation to its center.
   *
   * @param horizontalFactor
   * @param verticalFactor
   *
   * @return
   */
  override
  def scaleBy(
      horizontalFactor: Double,
      verticalFactor: Double): Bitmap = {

    scaleBy(horizontalFactor, verticalFactor, position)
  }

  /**
   * Scales this object by given horizontal and vertical factors in relation to a given point.
   *
   * @param horizontalFactor
   * @param verticalFactor
   * @param relativityPoint
   *
   * @return
   */
  override
  def scaleBy(
      horizontalFactor: Double,
      verticalFactor: Double,
      relativityPoint: Pos): Bitmap = {

    if (isNotRenderable)
      return this

    if (horizontalFactor == 0 || verticalFactor == 0)
      return Bitmap()

    val newCenter =
      boundary.center.scaleBy(
        horizontalFactor, verticalFactor, relativityPoint)

    scaleByInternal(horizontalFactor, verticalFactor, newCenter)
  }

  /**
   * Scales this object by given horizontal and vertical factors in relation to the origo.
   *
   * @param horizontalFactor
   * @param verticalFactor
   *
   * @return
   */
  override
  def scaleByRelativeToOrigo(
      horizontalFactor: Double,
      verticalFactor: Double): Bitmap = {

    if (isNotRenderable)
      return this

    if (horizontalFactor == 0 || verticalFactor == 0)
      return Bitmap()

    val newCenter =
      boundary.center.scaleByRelativeToOrigo(
        horizontalFactor, verticalFactor)

    scaleByInternal(horizontalFactor, verticalFactor, newCenter)
  }

  /**
   *
   *
   * @param horizontalFactor
   * @param verticalFactor
   * @param newCenter
   *
   * @return
   */
  private
  def scaleByInternal(
      horizontalFactor: Double,
      verticalFactor: Double,
      newCenter: Pos): Bitmap = {

    // TODO: Make content of the bitmap to flip when scaling factors are negative
    val newBuffer = transformContentUsing(
      AffineTransformation.forOrigoRelativeScalingOf(
        math.abs(horizontalFactor), math.abs(verticalFactor)))

    val newUpperLeftCorner =
      newCenter - (newBuffer.widthInPixels / 2.0, newBuffer.heightInPixels / 2.0)

    val newLowerRightCorner =
      newUpperLeftCorner + (newBuffer.widthInPixels - 1, newBuffer.heightInPixels - 1)

    val newBounds = Bounds(newUpperLeftCorner, newLowerRightCorner)

    internalCopy(
      identity,
      isRenderable,
      newBounds,
      Option(newBuffer))
  }

}
