package aalto.smcl.bitmaps.operations

import aalto.smcl.common._
import aalto.smcl.platform.PlatformBitmapBuffer




/**
 * Operation to negate the blue component of a bitmap.
 *
 * @author Aleksi Lukkarinen
 */
private[bitmaps] case class NegateBlueComponent()
  extends AbstractOperation with OneSourceFilter with Immutable {

  /** Information about this [[Renderable]] instance */
  lazy val metaInformation = MetaInformationMap(Map(
  ))


  /**
   * Creates the buffer which contains the results of applying this operation
   * and which is used as a background for a new buffers provided by this
   * [[Buffered]].
   *
   * @param sources     possible [[PlatformBitmapBuffer]] instances used as sources
   * @return
   */
  override protected def createStaticBuffer(sources: PlatformBitmapBuffer*): PlatformBitmapBuffer = {
    require(sources.length == 1,
      s"Negative creation requires exactly one source image (provided: ${sources.length}).")

    sources(0).createFilteredVersionWith(RGBAComponentTranslationTable.forNegatingBlue)
  }

}