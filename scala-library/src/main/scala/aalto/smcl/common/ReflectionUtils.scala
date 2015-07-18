package aalto.smcl.common


import scala.reflect.runtime.universe.{Symbol, TypeTag, typeOf}




/**
 * Miscellaneous reflection-related utility operations.
 *
 * @author Aleksi Lukkarinen
 */
private[smcl] object ReflectionUtils {

  /**
   * Retrieves the `Symbol` object for the type of the object referenced by a given variable.
   *
   * @param variable    reference to the object which the `Symbol` is to be retrieved for
   */
  def symbolOf[T](variable: T)(implicit tt: TypeTag[T]): Symbol = typeOf(tt).typeSymbol

}
