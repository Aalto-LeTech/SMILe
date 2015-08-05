package aalto.smcl.bitmaps.immutable.primitives


import java.awt.geom.AffineTransform

import scala.collection.mutable
import scala.ref.WeakReference

import aalto.smcl.SMCL
import aalto.smcl.bitmaps.BitmapSettingKeys._
import aalto.smcl.bitmaps.immutable.primitives.Bitmap.ViewerUpdateStyle
import aalto.smcl.bitmaps.immutable.primitives.Bitmap.ViewerUpdateStyle.UpdateViewerPerDefaults
import aalto.smcl.bitmaps.immutable.{BitmapIdentity, PixelRectangle}
import aalto.smcl.bitmaps.operations._
import aalto.smcl.bitmaps.{display => displayInViewer, _}
import aalto.smcl.common.{RGBAColor, GS, HorizontalAlignment, TimestampedCreation, VerticalAlignment}
import aalto.smcl.platform.{PlatformBitmapBuffer, PlatformDrawingSurface, RenderableBitmap}




/**
 *
 *
 * @author Aleksi Lukkarinen
 */
object Bitmap {

  SMCL.performInitialization()


  /**
   *
   */
  object ViewerUpdateStyle {


    /**
     *
     */
    abstract sealed class Value


    /**
     *
     */
    case object UpdateViewerPerDefaults extends Value


    /**
     *
     */
    case object PreventViewerUpdates extends Value


  }


  /**
   * Creates a new empty [[Bitmap]] instance.
   */
  def apply(
      widthInPixels: Int = GS.intFor(DefaultBitmapWidthInPixels),
      heightInPixels: Int = GS.intFor(DefaultBitmapHeightInPixels),
      initialBackgroundColor: RGBAColor = GS.colorFor(DefaultBackground),
      viewerHandling: ViewerUpdateStyle.Value = UpdateViewerPerDefaults): Bitmap = {

    require(widthInPixels > 0, s"Width of the image must be at least 1 pixel (was $widthInPixels)")
    require(heightInPixels > 0, s"Height of the image must be at least 1 pixel (was $heightInPixels)")
    require(initialBackgroundColor != null, "The background color argument has to be a Color instance (was null).")

    val operationList =
      Clear(initialBackgroundColor) +:
          BitmapOperationList(CreateBitmap(widthInPixels, heightInPixels))

    val newBitmap = new Bitmap(operationList, BitmapIdentity())

    if (viewerHandling == UpdateViewerPerDefaults) {
      if (GS.isTrueThat(NewBitmapsAreDisplayedAutomatically))
        newBitmap.display()
    }

    newBitmap
  }


  /**
   *
   */
  //  def apply(sourceFilePath: String): Bitmap = {
  //
  //    // TODO: Load image from the given file and init the Bitmap accordingly
  //    val operationList = BitmapOperationList()
  //
  //    new Bitmap(operationList)
  //  }

}


/**
 *
 *
 * @param operations
 * @param uniqueIdentifier
 *
 * @author Aleksi Lukkarinen
 */
case class Bitmap private(
    private[bitmaps] val operations: BitmapOperationList,
    uniqueIdentifier: BitmapIdentity) extends {

  /** Width of this [[Bitmap]]. */
  val widthInPixels: Int = operations.widthInPixels

  /** Height of this [[Bitmap]]. */
  val heightInPixels: Int = operations.heightInPixels

  /** Rendering buffer for this image. */
  private[this] var _renderingBuffer: WeakReference[PlatformBitmapBuffer] =
    WeakReference[PlatformBitmapBuffer](null)

} with RenderableBitmap
       with PixelRectangle
       with OperableBitmap
       with Immutable
       with TimestampedCreation {

  /**
   * Returns the initial background color of this [[Bitmap]]
   * (may not be the actual background color at a later time).
   */
  val initialBackgroundColor: RGBAColor = operations.initialBackgroundColor()

  /**
   * Applies an [[AbstractSingleSourceOperation]] to this [[Bitmap]].
   *
   * @param newOperation
   * @param viewerHandling
   * @return
   */
  private[bitmaps] def apply(
      newOperation: AbstractSingleSourceOperation,
      viewerHandling: ViewerUpdateStyle.Value): Bitmap = {

    require(newOperation != null, "Operation argument cannot be null.")

    val newBitmap = copy(operations = newOperation +: operations)

    if (viewerHandling == UpdateViewerPerDefaults) {
      if (GS.isTrueThat(DisplayBitmapsAutomaticallyAfterOperations))
        newBitmap.display()
    }

    newBitmap
  }

  /**
   * Applies an [[AbstractBufferProviderOperation]] to this [[Bitmap]].
   *
   * @param newOperation
   * @param viewerHandling
   * @return
   */
  private[bitmaps] def apply(
      newOperation: AbstractBufferProviderOperation,
      viewerHandling: ViewerUpdateStyle.Value): Bitmap = {

    require(newOperation != null, "Operation argument cannot be null.")

    val newOperationList = BitmapOperationList(newOperation)
    val newBitmap = copy(operations = newOperationList)

    if (viewerHandling == UpdateViewerPerDefaults) {
      if (GS.isTrueThat(DisplayBitmapsAutomaticallyAfterOperations))
        newBitmap.display()
    }

    newBitmap
  }

  /**
   *
   *
   * @param color
   * @param viewerHandling
   * @return
   */
  def clear(
      color: RGBAColor = GS.colorFor(DefaultBackground),
      viewerHandling: ViewerUpdateStyle.Value = UpdateViewerPerDefaults): Bitmap = {

    apply(Clear(color), viewerHandling)
  }

  /**
   *
   *
   * @param fromXInPixels
   * @param fromYInPixels
   * @param toXInPixels
   * @param toYInPixels
   * @param color
   * @param viewerHandling
   * @return
   */
  def drawLine(
      fromXInPixels: Int,
      fromYInPixels: Int,
      toXInPixels: Int,
      toYInPixels: Int,
      color: RGBAColor = GS.colorFor(DefaultPrimary),
      viewerHandling: ViewerUpdateStyle.Value = UpdateViewerPerDefaults): Bitmap = {

    apply(DrawLine(
      fromXInPixels, fromYInPixels,
      toXInPixels, toYInPixels,
      color), viewerHandling)
  }

  /**
   *
   *
   * @param xCoordinates
   * @param yCoordinates
   * @param numberOfCoordinatesToDraw
   * @param color
   * @param viewerHandling
   * @return
   */
  def drawPolyline(
      xCoordinates: Seq[Int],
      yCoordinates: Seq[Int],
      numberOfCoordinatesToDraw: Int,
      color: RGBAColor = GS.colorFor(DefaultPrimary),
      viewerHandling: ViewerUpdateStyle.Value = UpdateViewerPerDefaults): Bitmap = {

    apply(DrawPolyline(
      xCoordinates, yCoordinates,
      numberOfCoordinatesToDraw,
      color), viewerHandling)
  }

  /**
   *
   *
   * @param xCoordinates
   * @param yCoordinates
   * @param numberOfCoordinatesToDraw
   * @param hasBorder
   * @param hasFilling
   * @param color
   * @param fillColor
   * @param viewerHandling
   * @return
   */
  def drawPolygon(
      xCoordinates: Seq[Int],
      yCoordinates: Seq[Int],
      numberOfCoordinatesToDraw: Int,
      hasBorder: Boolean = true,
      hasFilling: Boolean = false,
      color: RGBAColor = GS.colorFor(DefaultPrimary),
      fillColor: RGBAColor = GS.colorFor(DefaultSecondary),
      viewerHandling: ViewerUpdateStyle.Value = UpdateViewerPerDefaults): Bitmap = {

    apply(DrawPolygon(
      xCoordinates, yCoordinates,
      numberOfCoordinatesToDraw,
      hasBorder, hasFilling,
      color, fillColor), viewerHandling)
  }

  /**
   *
   *
   * @param upperLeftCornerXInPixels
   * @param upperLeftCornerYInPixels
   * @param sideLengthInPixels
   * @param hasBorder
   * @param hasFilling
   * @param color
   * @param fillColor
   * @param viewerHandling
   * @return
   */
  def drawSquare(
      upperLeftCornerXInPixels: Int,
      upperLeftCornerYInPixels: Int,
      sideLengthInPixels: Int = GS.intFor(DefaultBitmapWidthInPixels),
      hasBorder: Boolean = GS.isTrueThat(ShapesHaveBordersByDefault),
      hasFilling: Boolean = GS.isTrueThat(ShapesHaveFillingsByDefault),
      color: RGBAColor = GS.colorFor(DefaultPrimary),
      fillColor: RGBAColor = GS.colorFor(DefaultSecondary),
      viewerHandling: ViewerUpdateStyle.Value = UpdateViewerPerDefaults): Bitmap = {

    apply(DrawSquare(
      upperLeftCornerXInPixels, upperLeftCornerYInPixels,
      sideLengthInPixels,
      hasBorder, hasFilling,
      color, fillColor), viewerHandling)
  }

  /**
   *
   *
   * @param upperLeftCornerXInPixels
   * @param upperLeftCornerYInPixels
   * @param widthInPixels
   * @param heightInPixels
   * @param hasBorder
   * @param hasFilling
   * @param color
   * @param fillColor
   * @param viewerHandling
   * @return
   */
  def drawRectangle(
      upperLeftCornerXInPixels: Int,
      upperLeftCornerYInPixels: Int,
      widthInPixels: Int = GS.intFor(DefaultBitmapWidthInPixels),
      heightInPixels: Int = GS.intFor(DefaultBitmapHeightInPixels),
      hasBorder: Boolean = GS.isTrueThat(ShapesHaveBordersByDefault),
      hasFilling: Boolean = GS.isTrueThat(ShapesHaveFillingsByDefault),
      color: RGBAColor = GS.colorFor(DefaultPrimary),
      fillColor: RGBAColor = GS.colorFor(DefaultSecondary),
      viewerHandling: ViewerUpdateStyle.Value = UpdateViewerPerDefaults): Bitmap = {

    apply(DrawRectangle(
      upperLeftCornerXInPixels, upperLeftCornerYInPixels,
      widthInPixels, heightInPixels,
      hasBorder, hasFilling,
      color, fillColor), viewerHandling)
  }

  /**
   *
   *
   * @param upperLeftCornerXInPixels
   * @param upperLeftCornerYInPixels
   * @param sideLengthInPixels
   * @param roundingWidthInPixels
   * @param roundingHeightInPixels
   * @param hasBorder
   * @param hasFilling
   * @param color
   * @param fillColor
   * @param viewerHandling
   * @return
   */
  def drawRoundedSquare(
      upperLeftCornerXInPixels: Int,
      upperLeftCornerYInPixels: Int,
      sideLengthInPixels: Int = GS.intFor(DefaultBitmapWidthInPixels),
      roundingWidthInPixels: Int = GS.intFor(DefaultRoundingWidthInPixels),
      roundingHeightInPixels: Int = GS.intFor(DefaultRoundingHeightInPixels),
      hasBorder: Boolean = GS.isTrueThat(ShapesHaveBordersByDefault),
      hasFilling: Boolean = GS.isTrueThat(ShapesHaveFillingsByDefault),
      color: RGBAColor = GS.colorFor(DefaultPrimary),
      fillColor: RGBAColor = GS.colorFor(DefaultSecondary),
      viewerHandling: ViewerUpdateStyle.Value = UpdateViewerPerDefaults): Bitmap = {

    apply(DrawRoundedSquare(
      upperLeftCornerXInPixels, upperLeftCornerYInPixels,
      sideLengthInPixels,
      roundingWidthInPixels, roundingHeightInPixels,
      hasBorder, hasFilling,
      color, fillColor), viewerHandling)
  }

  /**
   *
   *
   * @param upperLeftCornerXInPixels
   * @param upperLeftCornerYInPixels
   * @param widthInPixels
   * @param heightInPixels
   * @param roundingWidthInPixels
   * @param roundingHeightInPixels
   * @param hasBorder
   * @param hasFilling
   * @param color
   * @param fillColor
   * @param viewerHandling
   * @return
   */
  def drawRoundedRectangle(
      upperLeftCornerXInPixels: Int,
      upperLeftCornerYInPixels: Int,
      widthInPixels: Int = GS.intFor(DefaultBitmapWidthInPixels),
      heightInPixels: Int = GS.intFor(DefaultBitmapHeightInPixels),
      roundingWidthInPixels: Int = GS.intFor(DefaultRoundingWidthInPixels),
      roundingHeightInPixels: Int = GS.intFor(DefaultRoundingHeightInPixels),
      hasBorder: Boolean = GS.isTrueThat(ShapesHaveBordersByDefault),
      hasFilling: Boolean = GS.isTrueThat(ShapesHaveFillingsByDefault),
      color: RGBAColor = GS.colorFor(DefaultPrimary),
      fillColor: RGBAColor = GS.colorFor(DefaultSecondary),
      viewerHandling: ViewerUpdateStyle.Value = UpdateViewerPerDefaults): Bitmap = {

    apply(DrawRoundedRectangle(
      upperLeftCornerXInPixels, upperLeftCornerYInPixels,
      widthInPixels, heightInPixels,
      roundingWidthInPixels, roundingHeightInPixels,
      hasBorder, hasFilling,
      color, fillColor), viewerHandling)
  }

  /**
   *
   *
   * @param centerXInPixels
   * @param centerYInPixels
   * @param radiusInPixels
   * @param hasBorder
   * @param hasFilling
   * @param color
   * @param fillColor
   * @param viewerHandling
   * @return
   */
  def drawCircle(
      centerXInPixels: Int,
      centerYInPixels: Int,
      radiusInPixels: Int = GS.intFor(DefaultCircleRadiusInPixels),
      hasBorder: Boolean = GS.isTrueThat(ShapesHaveBordersByDefault),
      hasFilling: Boolean = GS.isTrueThat(ShapesHaveFillingsByDefault),
      color: RGBAColor = GS.colorFor(DefaultPrimary),
      fillColor: RGBAColor = GS.colorFor(DefaultSecondary),
      viewerHandling: ViewerUpdateStyle.Value = UpdateViewerPerDefaults): Bitmap = {

    apply(DrawCircle(
      centerXInPixels, centerYInPixels,
      radiusInPixels,
      hasBorder, hasFilling,
      color, fillColor), viewerHandling)
  }

  /**
   *
   *
   * @param centerXInPixels
   * @param centerYInPixels
   * @param widthInPixels
   * @param heightInPixels
   * @param hasBorder
   * @param hasFilling
   * @param color
   * @param fillColor
   * @param viewerHandling
   * @return
   */
  def drawEllipse(
      centerXInPixels: Int,
      centerYInPixels: Int,
      widthInPixels: Int = GS.intFor(DefaultBitmapWidthInPixels),
      heightInPixels: Int = GS.intFor(DefaultBitmapHeightInPixels),
      hasBorder: Boolean = GS.isTrueThat(ShapesHaveBordersByDefault),
      hasFilling: Boolean = GS.isTrueThat(ShapesHaveFillingsByDefault),
      color: RGBAColor = GS.colorFor(DefaultPrimary),
      fillColor: RGBAColor = GS.colorFor(DefaultSecondary),
      viewerHandling: ViewerUpdateStyle.Value = UpdateViewerPerDefaults): Bitmap = {

    apply(DrawEllipse(
      centerXInPixels, centerYInPixels,
      widthInPixels, heightInPixels,
      hasBorder, hasFilling,
      color, fillColor), viewerHandling)
  }

  /**
   *
   *
   * @param upperLeftCornerXInPixels
   * @param upperLeftCornerYInPixels
   * @param widthInPixels
   * @param heightInPixels
   * @param startAngleInDegrees
   * @param arcAngleInDegrees
   * @param hasBorder
   * @param hasFilling
   * @param color
   * @param fillColor
   * @param viewerHandling
   * @return
   */
  def drawArc(
      upperLeftCornerXInPixels: Int,
      upperLeftCornerYInPixels: Int,
      widthInPixels: Int = GS.intFor(DefaultBitmapWidthInPixels),
      heightInPixels: Int = GS.intFor(DefaultBitmapHeightInPixels),
      startAngleInDegrees: Int = GS.intFor(DefaultArcStartAngleInDegrees),
      arcAngleInDegrees: Int = GS.intFor(DefaultArcAngleInDgrees),
      hasBorder: Boolean = GS.isTrueThat(ShapesHaveBordersByDefault),
      hasFilling: Boolean = GS.isTrueThat(ShapesHaveFillingsByDefault),
      color: RGBAColor = GS.colorFor(DefaultPrimary),
      fillColor: RGBAColor = GS.colorFor(DefaultSecondary),
      viewerHandling: ViewerUpdateStyle.Value = UpdateViewerPerDefaults): Bitmap = {

    apply(DrawArc(
      upperLeftCornerXInPixels, upperLeftCornerYInPixels,
      widthInPixels, heightInPixels,
      startAngleInDegrees, arcAngleInDegrees,
      hasBorder, hasFilling,
      color, fillColor), viewerHandling)
  }

  /**
   *
   *
   * @param bitmapsToCombineWith
   * @param verticalAlignment
   * @param paddingInPixels
   * @param backgroundColor
   * @return
   */
  def appendOnLeft(
      bitmapsToCombineWith: Bitmap*)(
      verticalAlignment: VerticalAlignment.Value = GS.optionFor(DefaultVerticalAlignment),
      paddingInPixels: Int = GS.intFor(DefaultPaddingInPixels),
      backgroundColor: RGBAColor = GS.colorFor(DefaultBackground)): Bitmap = {

    apply(
      AppendHorizontally(bitmapsToCombineWith :+ this)(
        verticalAlignment, paddingInPixels, backgroundColor),
      UpdateViewerPerDefaults)
  }

  /**
   *
   *
   * @param bitmapsToCombineWith
   * @param verticalAlignment
   * @param paddingInPixels
   * @param backgroundColor
   * @return
   */
  def appendOnRight(
      bitmapsToCombineWith: Bitmap*)(
      verticalAlignment: VerticalAlignment.Value = GS.optionFor(DefaultVerticalAlignment),
      paddingInPixels: Int = GS.intFor(DefaultPaddingInPixels),
      backgroundColor: RGBAColor = GS.colorFor(DefaultBackground)): Bitmap = {

    apply(
      AppendHorizontally(this +: bitmapsToCombineWith)(
        verticalAlignment, paddingInPixels, backgroundColor),
      UpdateViewerPerDefaults)
  }

  /**
   *
   *
   * @param bitmapsToCombineWith
   * @param horizontalAlignment
   * @param paddingInPixels
   * @param backgroundColor
   * @return
   */
  def appendOnTop(
      bitmapsToCombineWith: Bitmap*)(
      horizontalAlignment: HorizontalAlignment.Value = GS.optionFor(DefaultHorizontalAlignment),
      paddingInPixels: Int = GS.intFor(DefaultPaddingInPixels),
      backgroundColor: RGBAColor = GS.colorFor(DefaultBackground)): Bitmap = {

    apply(
      AppendVertically(bitmapsToCombineWith :+ this)(
        horizontalAlignment, paddingInPixels, backgroundColor),
      UpdateViewerPerDefaults)
  }

  /**
   *
   *
   * @param bitmapsToCombineWith
   * @param horizontalAlignment
   * @param paddingInPixels
   * @param backgroundColor
   * @return
   */
  def appendOnBottom(
      bitmapsToCombineWith: Bitmap*)(
      horizontalAlignment: HorizontalAlignment.Value = GS.optionFor(DefaultHorizontalAlignment),
      paddingInPixels: Int = GS.intFor(DefaultPaddingInPixels),
      backgroundColor: RGBAColor = GS.colorFor(DefaultBackground)): Bitmap = {

    apply(
      AppendVertically(this +: bitmapsToCombineWith)(
        horizontalAlignment, paddingInPixels, backgroundColor),
      UpdateViewerPerDefaults)
  }


  // ----------------------------------------------------------------------------------------------

  //-------------------------------
  //
  //  :/\
  //
  //-------------------------------

  /**
   *
   *
   * @param other
   * @return
   */
  def :/\ (other: Bitmap): Bitmap = appendOnTop(other)()

  /**
   *
   *
   * @param other
   * @return
   */
  def :/\ (other: scala.collection.Seq[Bitmap]): Bitmap = appendOnTop(other: _*)()

  /**
   *
   *
   * @param other
   * @return
   */
  def :/\ (other: scala.collection.Traversable[Bitmap]): Bitmap = :/\(other.toSeq)

  //-------------------------------
  //
  //  /\:
  //
  //-------------------------------

  /**
   *
   *
   * @param other
   * @return
   */
  def /\: (other: Bitmap): Bitmap = appendOnTop(other)()

  /**
   *
   *
   * @param other
   * @return
   */
  def /\: (other: scala.collection.Seq[Bitmap]): Bitmap = appendOnTop(other: _*)()

  /**
   *
   *
   * @param other
   * @return
   */
  def /\: (other: scala.collection.Traversable[Bitmap]): Bitmap = /\:(other.toSeq)

  //-------------------------------
  //
  //  :\/
  //
  //-------------------------------

  /**
   *
   *
   * @param other
   * @return
   */
  def :\/ (other: Bitmap): Bitmap = appendOnBottom(other)()

  /**
   *
   *
   * @param other
   * @return
   */
  def :\/ (other: scala.collection.Seq[Bitmap]): Bitmap = appendOnBottom(other: _*)()

  /**
   *
   *
   * @param other
   * @return
   */
  def :\/ (other: scala.collection.Traversable[Bitmap]): Bitmap = :\/(other.toSeq)

  //-------------------------------
  //
  //  \/:
  //
  //-------------------------------

  /**
   *
   *
   * @param other
   * @return
   */
  def \/: (other: Bitmap): Bitmap = appendOnBottom(other)()

  /**
   *
   *
   * @param other
   * @return
   */
  def \/: (other: scala.collection.Seq[Bitmap]): Bitmap = appendOnBottom(other: _*)()

  /**
   *
   *
   * @param other
   * @return
   */
  def \/: (other: scala.collection.Traversable[Bitmap]): Bitmap = \/:(other.toSeq)

  //-------------------------------
  //
  //  :>>
  //
  //-------------------------------

  /**
   *
   *
   * @param other
   * @return
   */
  def :>> (other: Bitmap): Bitmap = appendOnRight(other)()

  /**
   *
   *
   * @param other
   * @return
   */
  def :>> (other: scala.collection.Seq[Bitmap]): Bitmap = appendOnRight(other: _*)()

  /**
   *
   *
   * @param other
   * @return
   */
  def :>> (other: scala.collection.Traversable[Bitmap]): Bitmap = :>>(other.toSeq)

  //-------------------------------
  //
  //  >>:
  //
  //-------------------------------

  /**
   *
   *
   * @param other
   * @return
   */
  def >>: (other: Bitmap): Bitmap = appendOnRight(other)()

  /**
   *
   *
   * @param other
   * @return
   */
  def >>: (other: scala.collection.Seq[Bitmap]): Bitmap = appendOnRight(other: _*)()

  /**
   *
   *
   * @param other
   * @return
   */
  def >>: (other: scala.collection.Traversable[Bitmap]): Bitmap = >>:(other.toSeq)

  //-------------------------------
  //
  //  :<<
  //
  //-------------------------------

  /**
   *
   *
   * @param other
   * @return
   */
  def :<< (other: Bitmap): Bitmap = appendOnLeft(other)()

  /**
   *
   *
   * @param other
   * @return
   */
  def :<< (other: scala.collection.Seq[Bitmap]): Bitmap = appendOnLeft(other: _*)()

  /**
   *
   *
   * @param other
   * @return
   */
  def :<< (other: scala.collection.Traversable[Bitmap]): Bitmap = :<<(other.toSeq)

  //-------------------------------
  //
  //  <<:
  //
  //-------------------------------

  /**
   *
   *
   * @param other
   * @return
   */
  def <<: (other: Bitmap): Bitmap = appendOnLeft(other)()

  /**
   *
   *
   * @param other
   * @return
   */
  def <<: (other: scala.collection.Seq[Bitmap]): Bitmap = appendOnLeft(other: _*)()

  /**
   *
   *
   * @param other
   * @return
   */
  def <<: (other: scala.collection.Traversable[Bitmap]): Bitmap = <<:(other.toSeq)

  // ----------------------------------------------------------------------------------------------


  /**
   * Renders this [[Bitmap]] onto a drawing surface using specified coordinates.
   *
   * @param drawingSurface
   * @param x
   * @param y
   */
  def renderOnto(drawingSurface: PlatformDrawingSurface, x: Int, y: Int): Unit = {
    require(drawingSurface != null, "Drawing surface argument cannot be null.")

    val rendition = toRenderedRepresentation
    drawingSurface.drawBitmap(rendition, x, y)
  }

  /**
   * Renders this [[Bitmap]] onto a drawing surface using specified affine transformation.
   *
   * @param drawingSurface
   * @param affineTransformation
   */
  def renderOnto(drawingSurface: PlatformDrawingSurface, affineTransformation: AffineTransform): Unit = {
    require(drawingSurface != null, "Drawing surface argument cannot be null.")

    val rendition = toRenderedRepresentation
    drawingSurface.drawBitmap(_renderingBuffer.apply(), affineTransformation)
  }

  /**
   * Returns a `BufferedImage` instance representing this [[Bitmap]].
   */
  def toRenderedRepresentation: PlatformBitmapBuffer =
    _renderingBuffer.get getOrElse {
      val rendition = operations.render()

      _renderingBuffer = WeakReference[PlatformBitmapBuffer](rendition)

      return rendition
    }

  /**
   * Returns a mutable `ArrayBuffer` containing a given number of copies of this [[Bitmap]] instance.
   *
   * @param size
   * @return
   */
  private def propagateToArrayBuffer(size: Int): mutable.ArrayBuffer[Bitmap] = {
    require(size >= 0, s"Size of the collection cannot be negative (was $size)")

    mutable.ArrayBuffer.fill[Bitmap](size)(this)
  }

  /**
   * Returns an `Array` containing a given number of copies of this [[Bitmap]] instance.
   *
   * @param size
   * @return
   */
  def propagateToArray(size: Int): Array[Bitmap] = propagateToArrayBuffer(size).toArray

  /**
   * Returns an `List` containing a given number of copies of this [[Bitmap]] instance.
   *
   * @param size
   * @return
   */
  def propagateToList(size: Int): List[Bitmap] = propagateToArrayBuffer(size).toList

  /**
   * Returns an `Seq` containing a given number of copies of this [[Bitmap]] instance.
   *
   * @param size
   * @return
   */
  def propagateToSeq(size: Int): Seq[Bitmap] = propagateToArrayBuffer(size).toSeq

  /**
   *
   */
  def display(): Bitmap = {
    displayInViewer(this)

    this
  }

}
