package aalto.smcl.bitmaps.operations


import aalto.smcl.common._
import aalto.smcl.platform.{PlatformAffineTransform, PlatformBitmapBuffer}




/**
 * Operation to flip a bitmap vertically.
 *
 * @author Aleksi Lukkarinen
 */
private[bitmaps] case class FlipVertically()
    extends AbstractOperation with Renderable with Buffered with Immutable {

  /** Information about this [[Renderable]] instance */
  lazy val metaInformation = MetaInformationMap(Map())


  /**
   * Creates the buffer which contains the results of applying this operation
   * and which is used as a background for a new buffers provided by this
   * [[Buffered]].
   *
   * @param sources     possible [[PlatformBitmapBuffer]] instances used as sources
   * @return
   */
  override protected def createStaticBuffer(sources: PlatformBitmapBuffer*): PlatformBitmapBuffer = {
    require(sources.length == 1, s"Flip required exactly one source image (provided: ${sources.length}).")

    sources(0).createTransfomedVersionWith(
      PlatformAffineTransform.forVerticalFlipOf(sources(0).heightInPixels))
  }

  /**
   * Flips the given bitmap vertically.
   *
   * @param destination
   */
  override def render(destination: PlatformBitmapBuffer): Unit =
    destination.drawingSurface().drawBitmap(getOrCreateStaticBuffer(destination))

}
