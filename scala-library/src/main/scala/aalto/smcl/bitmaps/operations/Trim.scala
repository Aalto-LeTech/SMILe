package aalto.smcl.bitmaps.operations


import aalto.smcl.GS
import aalto.smcl.bitmaps.{Bitmap, DefaultBackground}
import aalto.smcl.colors.{RGBAColor, _}
import aalto.smcl.infrastructure.MetaInformationMap
import aalto.smcl.platform.PlatformBitmapBuffer




/**
 * Operation to trim a bitmap, i.e. to remove "empty" horizontal
 * and vertical slices of pixels from its edges.
 *
 * @param sourceBitmap
 * @param colorToTrim
 *
 * @author Aleksi Lukkarinen
 */
private[bitmaps] case class Trim(
    sourceBitmap: Bitmap,
    colorToTrim: RGBAColor = GS.colorFor(DefaultBackground))
    extends AbstractOperation with BufferProvider with Immutable {

  require(sourceBitmap != null, s"Trimming requires exactly one source image (was null).")
  require(colorToTrim != null, "The background color argument has to be a Color instance (was null).")

  /** Information about this [[Renderable]] instance */
  lazy val metaInformation = MetaInformationMap(Map(
    "colorToTrim" -> Option(s"0x${colorToTrim.toArgbInt.toArgbHexColorString}")
  ))

  /** The [[BitmapOperationList]] instance resulting the bitmap to be trimmed. */
  val childOperationListsOption: Option[Seq[BitmapOperationList]] =
    Option(Seq(sourceBitmap.operations))

  /**
   * Creates the buffer which contains the results of applying this operation
   * and which is used as a background for a new buffers provided by this
   * [[Buffered]].
   *
   * @param sources     possible [[PlatformBitmapBuffer]] instances used as sources
   * @return
   */
  override protected def createStaticBuffer(sources: PlatformBitmapBuffer*): PlatformBitmapBuffer = {
    sources(0).trim(colorToTrim)
  }

  /** Width of the provided buffer in pixels. */
  lazy val widthInPixels: Int =
    getOrCreateStaticBuffer(sourceBitmap.toRenderedRepresentation).widthInPixels

  /** Height of the provided buffer in pixels. */
  lazy val heightInPixels: Int =
    getOrCreateStaticBuffer(sourceBitmap.toRenderedRepresentation).heightInPixels

  /**
   * Returns the buffer from which the provided buffer copies are made.
   * Users of this trait must provide an implementation, which returns
   * a [[PlatformBitmapBuffer]] instance always after instantiation of
   * the class claiming to provide the buffer.
   *
   * @return    bitmap buffer to be made copies of for providees
   */
  override protected def provideNewBufferToBeCopiedForProvidees(): PlatformBitmapBuffer =
    getOrCreateStaticBuffer(sourceBitmap.toRenderedRepresentation)

}
