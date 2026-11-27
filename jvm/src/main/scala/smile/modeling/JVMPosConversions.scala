package smile.modeling

import java.awt.Point

/** JVM-only ways of making a [[smile.modeling.Pos]] from the AWT types.
  *
  * `Pos(point)` makes a position of a `java.awt.Point`, such as a Swing mouse event's. `Pos` is
  * shared with Scala.js, which has no AWT, so here it is an extension on the companion object.
  * `import smile.modeling.*` brings it into scope.
  */
extension (companion: Pos.type)
  /** Constructs a [[smile.modeling.Pos]] instance from the coordinates of the given `Point` object.
    * JVM only.
    */
  def apply(point: Point): Pos = new Pos(point.getX, point.getY)
