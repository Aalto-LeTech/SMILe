package aalto.smcl.colors


import aalto.smcl.infrastructure.SMCLInitializationInvoker




/**
 *
 *
 * @author Aleksi Lukkarinen
 */
final class SMCLInvalidHsiValueCombinationError private[smcl](
  invalidHueValue: Double,
  invalidSaturationValue: Double,
  invalidIntensityValue: Double)
  extends RuntimeException(
    s"The given HSI component combination " +
      "($invalidHueValue, $invalidSaturationValue, $invalidIntensityValue) is invalid.")
  with SMCLInitializationInvoker {

}
