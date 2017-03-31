package aalto.smcl.infrastructure.jvmawt


import scala.swing.Dialog
import scala.swing.Dialog.{Message, Options}

import aalto.smcl.infrastructure.exceptions.SMCLUnexpectedInternalError




/**
 *
 *
 * @author Aleksi Lukkarinen
 */
private[smcl]
class SwingUtils() {

  /**
   *
   *
   * @return
   */
  def yesNoDialogResultAsBoolean(result: Dialog.Result.Value): Boolean = result match {
    case Dialog.Result.Yes    => true
    case Dialog.Result.No     => false
    case Dialog.Result.Closed => false
    case _                    => throw new SMCLUnexpectedInternalError("Invalid dialog return value.")
  }

  /**
   *
   *
   * @return
   */
  val showParentlessYesNoQuestionDialog: (String, String) => Dialog.Result.Value =
    Dialog.showConfirmation(parent = null, _: String, _: String, Options.YesNo, Message.Question)

}
