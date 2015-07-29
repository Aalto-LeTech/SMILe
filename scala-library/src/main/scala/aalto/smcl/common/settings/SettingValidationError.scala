package aalto.smcl.common.settings


/**
 *
 *
 * @author Aleksi Lukkarinen
 */
final class SettingValidationError(setting: SettingKeys.Value, cause: Throwable)
    extends RuntimeException(
      s"""Validation of setting "${setting.toString}" failed (see the upstream exception).""",
      cause) {

}
