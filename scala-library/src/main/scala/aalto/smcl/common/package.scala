package aalto.smcl


/**
 *
 *
 * @author Aleksi Lukkarinen
 */
package object common {

  SMCL.performInitialization(ModuleInitializationPhase.Early)


  /** */
  lazy val Fonts: Fonts = new Fonts()

}
