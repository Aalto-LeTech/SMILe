package aalto.smcl.infrastructure

/**
 *
 *
 * @author Aleksi Lukkarinen
 */
final class SMCLFunctionExecutionError private[smcl](private val detailMessage: String, cause: Throwable)
  extends RuntimeException(detailMessage, cause) {

  /**
   *
   *
   * @param detailMessage
   * @return
   */
  def this(detailMessage: String) = this(detailMessage, null)

}
