package aalto.smcl.infrastructure.tests;


import org.scalatest.prop.PropertyChecks
import org.scalatest.{FreeSpec, Matchers};




/**
 * Base class for integration tests.
 *
 * @author Aleksi Lukkarinen
 */
abstract class IntegrationSpecBase extends FreeSpec with Matchers with PropertyChecks {

}
