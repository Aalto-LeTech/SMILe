package aalto.smcl.bitmaps.operations


import aalto.smcl.colors.RGBAComponentTranslationTable
import aalto.smcl.infrastructure.{MetaInformationMap, BitmapBufferAdapter}




/**
 * Operation to keep only the red and green components of a bitmap.
 *
 * @author Aleksi Lukkarinen
 */
private[bitmaps]
case class KeepOnlyRedAndGreenComponents()
  extends AbstractOperation
  with OneSourceFilter
  with Immutable {

  /** Information about this [[Renderable]] instance */
  lazy val metaInformation = MetaInformationMap("KeepOnlyRedAndGreenComponents", Map())

  /**
   * Creates the buffer which contains the results of applying this operation
   * and which is used as a background for a new buffers provided by this
   * [[Buffered]].
   *
   * @param sources possible [[BitmapBufferAdapter]] instances used as sources
   * @return
   */
  override protected def createStaticBuffer(sources: BitmapBufferAdapter*): BitmapBufferAdapter = {
    require(sources.length == 1,
      s"Negative creation requires exactly one source image (provided: ${sources.length}).")

    sources(0).createFilteredVersionWith(RGBAComponentTranslationTable.forKeepingOnlyRedAndGreen)
  }

}
