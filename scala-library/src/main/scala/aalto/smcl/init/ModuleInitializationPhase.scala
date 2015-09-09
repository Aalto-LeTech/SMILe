package aalto.smcl.init


/**
 * Initialization phases.
 *
 * @author Aleksi Lukkarinen
 */
private[smcl] object ModuleInitializationPhase extends Enumeration {

  /** Type alias for this enumeration. */
  type ModuleInitializationPhase = Value

  /** Early initialization. */
  val Early = Value

  /** Late initialization. */
  val Late = Value

}
