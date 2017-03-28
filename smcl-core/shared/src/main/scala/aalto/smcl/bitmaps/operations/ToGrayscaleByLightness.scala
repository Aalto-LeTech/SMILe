package aalto.smcl.bitmaps.operations


import aalto.smcl.infrastructure.{MetaInformationMap, BitmapBufferAdapter}




/**
 * Operation to convert bitmap's colors into grayscale by lightness.
 *
 * @author Aleksi Lukkarinen
 */
private[bitmaps]
case class ToGrayscaleByLightness()
  extends AbstractOperation
  with OneSourceFilter
  with Immutable {

  /** Information about this [[Renderable]] instance */
  lazy val metaInformation = MetaInformationMap("ToGrayscaleByLightness", Map())

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
      s"Grayscale conversion requires exactly one source image (provided: ${sources.length}).")

    sources(0).iteratePixelsWith {(red, green, blue, opacity) =>
      val intensity = ((Math.max(red, green).max(blue) + Math.min(red, green).min(blue)) / 2.0).toInt

      (intensity, intensity, intensity, opacity)
    }
  }

}
