package aalto.smcl.colors.exceptions


import aalto.smcl.colors.ColorValidator




/**
 *
 *
 * @author Aleksi Lukkarinen
 */
final class SMCLRgbRedComponentFromValueProviderOutOfRangeError private[smcl](invalidValue: Int)
  extends RuntimeException(
    s"An RGB red component value returned by a value provider function was out of its range " +
      s"${ColorValidator.MinimumRgbRed} - ${ColorValidator.MaximumRgbRed} (was $invalidValue)") {

}
