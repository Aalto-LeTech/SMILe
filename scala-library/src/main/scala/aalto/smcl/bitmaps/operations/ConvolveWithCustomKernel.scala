package aalto.smcl.bitmaps.operations


import aalto.smcl.bitmaps.immutable.ConvolutionKernel
import aalto.smcl.common._
import aalto.smcl.platform.PlatformBitmapBuffer




/**
 * Operation to apply a custom convolution filter to a bitmap.
 *
 * @author Aleksi Lukkarinen
 */
private[bitmaps] case class ConvolveWithCustomKernel(kernel: ConvolutionKernel)
  extends AbstractOperation with OneSourceFilter with Immutable {

  /** Information about this [[Renderable]] instance */
  lazy val metaInformation = MetaInformationMap(Map(
    "kernel" -> Option(kernel.toString)
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
    require(sources.length == 1, s"Convolution requires exactly one source image (provided: ${sources.length}).")

    sources(0).createFilteredVersionWith(kernel)
  }

}
