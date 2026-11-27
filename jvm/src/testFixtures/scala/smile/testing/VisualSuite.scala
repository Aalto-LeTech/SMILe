package smile.testing

import smile.pictures.Bitmap

trait VisualAssertions:
  self: munit.FunSuite =>

  protected def assertBitmapEquals(
      name: String,
      expected: Bitmap,
      actual: Bitmap
  )(using munit.Location): Unit =
    val result = VisualArtifacts.compare(name, expected, actual)
    assert(result.matches, result.message)

/** Base for suites that produce visual artifacts. Clears previous results before the run and writes
  * the HTML index afterwards.
  */
trait VisualSuite extends munit.FunSuite with VisualAssertions:
  override def beforeAll(): Unit = VisualArtifacts.reset()
  override def afterAll(): Unit  =
    val _ = VisualArtifacts.writeIndex()
