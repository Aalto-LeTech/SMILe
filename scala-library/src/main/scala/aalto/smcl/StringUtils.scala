package aalto.smcl

import scala.reflect.runtime.universe.{ Literal, Constant }

/**
 * Miscellaneous string utility operations.
 *
 * @author Aleksi Lukkarinen
 */
object StringUtils {

  /**
   * Returns Scala's standard escaped representation of a given string.
   *
   * @param s   the string to be escaped
   */
  def escapeString(s: String): String = Literal(Constant(s)).toString()

}
