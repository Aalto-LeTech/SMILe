package aalto.smcl.colors


/**
 *
 *
 * @author Aleksi Lukkarinen
 */
final class SMCLInvalidColorWeightCombinationError private[smcl](private val detailMessage: String, cause: Throwable)
    extends RuntimeException(detailMessage, cause) {

  /**
   *
   *
   * @param detailMessage
   * @return
   */
  def this(detailMessage: String) = this(detailMessage, null)

}
