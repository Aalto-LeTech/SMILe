package smile.infrastructure

import scala.collection.mutable

/** Plans of any depth compared without recursion, since the equality of two deep plans recurses
  * once per level.
  */
object PlanListing:

  /** `ops` and every operation inside them, outermost first, each with the operations inside it
    * left out and counted, walked with a stack of its own. Two plans are equal exactly when their
    * listings are, and comparing listings never recurses.
    */
  def listed(ops: Vector[DrawOp]): Vector[(DrawOp, Vector[Int])] =
    def emptied(layer: DrawOp.Layer): DrawOp.Layer =
      DrawOp.withParts(layer, DrawOp.parts(layer).map(_ => Vector()))
    def inside(layer: DrawOp.Layer): Vector[Vector[DrawOp]] = DrawOp.parts(layer)
    val listing = Vector.newBuilder[(DrawOp, Vector[Int])]
    val pending = mutable.Stack.from(ops)
    while pending.nonEmpty do
      val (shallow, inner) = pending.pop() match
        case layer: DrawOp.Layer                => (emptied(layer), inside(layer))
        case DrawOp.HardClip(box, ops)          => (DrawOp.HardClip(box, Vector()), Vector(ops))
        case DrawOp.Tiles(tile, copies, vector) =>
          (DrawOp.Tiles(emptied(tile), copies, vector), inside(tile))
        case op => (op, Vector())
      listing += shallow -> inner.map(_.size)
      pending.pushAll(inner.flatten.reverseIterator)
    listing.result()
end PlanListing
