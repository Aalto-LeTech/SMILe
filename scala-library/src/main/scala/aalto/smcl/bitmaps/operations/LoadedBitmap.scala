package aalto.smcl.bitmaps.operations


import aalto.smcl.common.MetaInformationMap
import aalto.smcl.platform.PlatformBitmapBuffer




/**
 *
 *
 * @author Aleksi Lukkarinen
 */
private[bitmaps] case class LoadedBitmap(
    buffer: PlatformBitmapBuffer,
    resourcePathOption: Option[String],
    bitmapIndexInResourceOption: Option[Int])
    extends AbstractBufferProviderOperation with Immutable {

  require(resourcePathOption != null, "The resource path argument has to be a String or None (was null).")
  require(bitmapIndexInResourceOption != null, "The bitmap index argument has to be an Int or None (was null).")

  /** Width of the provided buffer in pixels. */
  override def widthInPixels: Int = buffer.widthInPixels

  /** Height of the provided buffer in pixels. */
  override def heightInPixels: Int = buffer.heightInPixels

  /** Information about this [[AbstractSingleSourceOperation]] instance */
  lazy override val metaInformation = MetaInformationMap(Map(
    "resourcePath" -> Option(resourcePathOption.getOrElse("<unknown>")),
    "imageIndexInFile" -> Option(bitmapIndexInResourceOption.fold("<undefined>") {_.toString})))

  /** Operation streams needed to construct  */
  override def childOperationListsOption: Option[Seq[BitmapOperationList]] = None

}
