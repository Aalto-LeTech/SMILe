package aalto.smcl.bitmaps.viewer


import java.awt.{Graphics2D => JGraphics2D, RenderingHints}

import scala.swing._
import scala.util.Try

import aalto.smcl.bitmaps.Bitmap
import aalto.smcl.common.AffineTransformation




/**
 *
 *
 * @author Aleksi Lukkarinen
 */
private[bitmaps]
class ImageDisplayPanel
  extends Panel {

  /** The bitmap to be displayed. */
  private var _bitmapOption: Option[Bitmap] = None

  /** */
  private var _zoomFactor: ZoomFactor = ZoomFactor.Identity

  /** */
  private var _zoomingTransformation: AffineTransformation = AffineTransformation()

  /**
   *
   *
   * @param lowLevelGraphics2D
   */
  override def paintComponent(lowLevelGraphics2D: JGraphics2D): Unit = {
    super.paintComponent(lowLevelGraphics2D)

    val bufferRetrievalTry =
      Try(_bitmapOption.get.toRenderedRepresentation.awtBufferedImage)
    if (bufferRetrievalTry.isFailure)
      return

    val drawingSurface = lowLevelGraphics2D.create().asInstanceOf[Graphics2D]
    try {
      drawingSurface.setRenderingHint(
        RenderingHints.KEY_INTERPOLATION,
        RenderingHints.VALUE_INTERPOLATION_BICUBIC)

      drawingSurface.drawImage(
        bufferRetrievalTry.get,
        _zoomingTransformation.platformAffineTransform.awtAffineTransformation,
        null)
    }
    finally {
      drawingSurface.dispose()
    }
  }

  /**
   *
   *
   * @return
   */
  def zoomFactor: ZoomFactor = _zoomFactor

  /**
   *
   *
   * @param value
   */
  def zoomFactor_=(value: ZoomFactor): Unit = {
    require(value != null, "Internal error: Zoom factor cannot be null.")


    cursor = Application.WaitCursor
    _zoomFactor = value
    val result = Try(updateView()) // TODO: Error processing, here as well as elsewhere...
    if (result.isFailure)
      println(result)
    cursor = Application.DefaultCursor
  }

  /**
   *
   *
   * @param adjuster
   * @return
   */
  def adjustZoomWith(adjuster: ZoomFactor => ZoomFactor): ZoomFactor = {
    require(adjuster != null, "Internal error: Function cannot be null.")

    val newZoomFactor = adjuster(_zoomFactor)
    require(newZoomFactor != null, "Internal error: Zoom factor returned by adjuster function cannot be null.")

    zoomFactor = newZoomFactor

    newZoomFactor
  }

  /**
   *
   *
   * @param bitmap
   */
  def updateImageBuffer(bitmap: Bitmap) = {
    require(bitmap != null, "Internal error: Bitmap to be used for update cannot be null.")

    _bitmapOption = Option(bitmap)

    cursor = Application.WaitCursor
    val result = Try(updateView()) // TODO: Error processing, here as well as elsewhere...
    if (result.isFailure)
      println(result)
    cursor = Application.DefaultCursor
  }

  /**
   *
   */
  def updateView(): Unit = {
    if (_bitmapOption.isEmpty)
      throw new IllegalStateException(
        "Internal error: Bitmap to be displayed has to be set before calling this method.")

    val newSize = _zoomFactor.scaleToDimension(_bitmapOption.get.sizeInPixels)

    if (minimumSize != newSize) {
      minimumSize = newSize
      preferredSize = newSize
      maximumSize = newSize
    }

    _zoomingTransformation = _zoomFactor.asAffineTransformation

    revalidate()
    repaint()
  }

}
