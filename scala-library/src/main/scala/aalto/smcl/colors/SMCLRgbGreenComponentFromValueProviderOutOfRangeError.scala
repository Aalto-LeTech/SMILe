package aalto.smcl.colors


/**
 *
 *
 * @author Aleksi Lukkarinen
 */
final class SMCLRgbGreenComponentFromValueProviderOutOfRangeError private[smcl](invalidValue: Int)
    extends RuntimeException(
      s"An RGB green component value returned by a value provider function was out of its range " +
          s"${ColorValidator.MinimumRgbGreen} - ${ColorValidator.MaximumRgbGreen} (was $invalidValue)") {

}
