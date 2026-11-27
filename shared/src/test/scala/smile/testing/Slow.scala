package smile.testing

/** Marks a test that takes seconds on its own, so that the quick tier (`sbt quick`) can leave it
  * out. `sbt test` and CI run it.
  */
val Slow = new munit.Tag("Slow")
