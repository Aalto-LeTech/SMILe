package smile.pictures

import smile.modeling.{Affine, Bounds}

import scala.collection.mutable

/** Walks of element trees with stacks of their own, so that a tree of any depth is hashed,
  * compared, printed and measured without overflowing the call stack.
  */
private[smile] object Trees:

  /** The elements directly under `e`. They are a picture's children front first, a clip's or wrap's
    * child, a mask group's masks in order and then its masked content, or an animation's frames. A
    * leaf has none.
    */
  def parts(e: Element): Iterator[Element] = e match
    case p: Picture         => p.children.iterator
    case c: Clip            => Iterator(c.child)
    case m: MaskGroup       => m.masks.iterator.map(_._1) ++ Iterator(m.masked)
    case w: Wrap            => Iterator(w.child)
    case a: AnimatedPicture => a.frames.iterator
    case _                  => Iterator.empty

  /** Calls `fill` on every element under `root`, `root` included, for which `known` is false,
    * children before parents, where the elements under each are its `under`. It never descends
    * below a known element.
    */
  def fillBottomUp(
      root: Element,
      known: Element => Boolean,
      fill: Element => Unit,
      under: Element => Iterator[Element] = parts
  ): Unit =
    // Each element is pushed once to be expanded and once more, under its parts, to be filled.
    val stack = mutable.Stack[(Element, Boolean)]()
    if !known(root) then stack.push((root, false))
    while stack.nonEmpty do
      val (e, expanded) = stack.pop()
      if expanded then fill(e)
      else if !known(e) then
        stack.push((e, true))
        under(e).filterNot(known).foreach(part => stack.push((part, false)))

  /** Caches the hash of every element under `e`, `e` excluded, from the bottom up. The formula of
    * `e`'s own hash then reads the hashes of its parts without recursing.
    */
  def hashParts(e: Element): Unit =
    parts(e).foreach(part => fillBottomUp(part, _.hashKnown, element => element.hashCode: Unit))

  /** The elements whose bounds make up the bounds of `e`. They are its parts, except for a mask
    * group, which gives only its masks that are not inverted and its masked content.
    */
  private def boundsParts(e: Element): Iterator[Element] = e match
    case m: MaskGroup => m.keepingMasks ++ Iterator(m.masked)
    case _            => parts(e)

  /** Caches the world bounds of every group whose bounds make up those of `e`, `e` excluded, from
    * the bottom up. The formula of `e`'s own bounds then reads the bounds of its parts without
    * recursing.
    */
  def measureParts(e: Element): Unit = measure(boundsParts(e))

  /** Caches the world bounds of `elements` and of every group under them that their bounds walk. */
  private def measure(elements: Iterator[Element]): Unit =
    // Only groups are ever not known.
    def cache(element: Element): Unit = element match
      case group: CachedBounds =>
        val _ = group.measured()
      case _ => ()
    elements.foreach(fillBottomUp(_, _.boundsKnown, cache, boundsParts))

  /** A step of `turnedBounds`. */
  private enum Step:
    /** Measure `e` in the frame that `outer` places. */
    case Visit(e: Element, outer: Affine)

    /** Close `group`'s frame. Cut the union of what its bounding part gave through `through`, as
      * `group.within` does, and add it to the frame around.
      */
    case Close(group: MaskGroup, through: Affine)

  /** The union of `part.boundsIn(m)` over `parts`, for an `m` that does not keep the axes, walked
    * with a stack of steps.
    *
    * The parts are measured first, so every group the walk meets has cached its bounds and those of
    * the groups under it. A group under a composed affine that keeps the axes then gives its box
    * mapped, as its `boundsThrough` does, from the cached bounds of its parts. A group under any
    * other affine gives its parts in its place, unless it was last measured through that affine,
    * when it gives the bounds it measured there. A mask group gives only its bounding part, in a
    * frame of its own. What that part gives is collected apart and, when the frame closes, cut to
    * the group's kept box mapped (see `smile.pictures.MaskGroup.within`), and added to the frame
    * around it. So the walk does the arithmetic that measuring each part through `boundsIn` does,
    * in the same order of composition, whichever levels were measured before.
    */
  def turnedBounds(parts: Iterable[Element], m: Affine): Option[Bounds] =
    measure(parts.iterator)
    // The boxes of each open frame, the innermost on top.
    val frames                            = mutable.Stack(mutable.ArrayBuffer[Option[Bounds]]())
    def add(bounds: Option[Bounds]): Unit = frames.top += bounds: Unit
    val stack = mutable.Stack.from[Step](parts.iterator.map(Step.Visit(_, m)))
    while stack.nonEmpty do
      stack.pop() match
        case Step.Close(group, through) =>
          add(group.within(through, Element.union(frames.pop().iterator)))
        case Step.Visit(e, outer) =>
          // The map outer never keeps the axes, so it is not the identity, and boundsIn(outer)
          // measures through the composition.
          val composed = e.affine.andThen(outer)
          if composed.keepsAxes then add(e.boundsThrough(composed))
          else
            e match
              case p: Picture =>
                p.children.foreach(child => stack.push(Step.Visit(child, composed)))
              case g: MaskGroup =>
                // A group last measured through this composed map keeps the bounds it measured
                // there.
                g.measuredThrough(composed) match
                  case Some(bounds) => add(bounds)
                  case None         =>
                    frames.push(mutable.ArrayBuffer())
                    stack.push(Step.Close(g, composed))
                    stack.push(Step.Visit(g.bounding, composed))
              case a: AnimatedPicture =>
                a.frames.foreach(frame => stack.push(Step.Visit(frame, composed)))
              case other => add(other.boundsThrough(composed))
    Element.union(frames.pop().iterator)

  /** Whether `a` and `b` are equal, as each class's `equals` defines it. */
  def sameTree(a: Element, b: Element): Boolean =
    val pending = mutable.Stack((a, b))
    var same    = true
    while same && pending.nonEmpty do
      val (x, y) = pending.pop()
      if !(x eq y) then
        same = (x.getClass eq y.getClass) && x.hashCode == y.hashCode && fieldsMatch(x, y)
        if same && !sharesParts(x, y) then pending.pushAll(parts(x).zip(parts(y)))
    same

  /** Whether `x` and `y`, of the same class, have equal fields apart from the elements under them,
    * of which they have as many. Leaves are compared whole.
    */
  private def fieldsMatch(x: Element, y: Element): Boolean = (x, y) match
    case (x: Picture, y: Picture) =>
      x.affine == y.affine && x.localAnchor == y.localAnchor &&
      x.children.size == y.children.size
    case (x: Clip, y: Clip) =>
      x.affine == y.affine && x.localAnchor == y.localAnchor && x.rect == y.rect
    case (x: MaskGroup, y: MaskGroup) =>
      x.affine == y.affine && x.localAnchor == y.localAnchor &&
      x.masks.corresponds(y.masks)(_._2 == _._2)
    case (x: AnimatedPicture, y: AnimatedPicture) =>
      x.affine == y.affine && x.localAnchor == y.localAnchor &&
      x.frameDelays == y.frameDelays && x.startTime == y.startTime &&
      x.frames.size == y.frames.size
    case (x: Wrap, y: Wrap) =>
      x.affine == y.affine && x.localAnchor == y.localAnchor && x.dx == y.dx && x.dy == y.dy &&
      x.period == y.period
    case _ => x == y

  /** Whether `x` and `y` are pictures that share their children. */
  private def sharesParts(x: Element, y: Element): Boolean = (x, y) match
    case (x: Picture, y: Picture) => x.parts eq y.parts
    case _                        => false

  /** The text of `e`, as each class's `toString` writes it. */
  def text(e: Element): String =
    val out = new StringBuilder
    // The pieces still to write, the next on top. Each is literal text or an element to write out.
    val pending = mutable.Stack[String | Element](e)

    def expand(pieces: (String | Element)*): Unit = pending.pushAll(pieces.reverseIterator)
    def separated(elements: Vector[Element]): Seq[String | Element] =
      elements.flatMap(element => Seq(", ", element)).drop(1)
    while pending.nonEmpty do
      pending.pop() match
        case s: String  => out ++= s
        case p: Picture =>
          val rest = s", ${p.affine}, anchor = ${p.localAnchor})"
          expand("Picture(" +: separated(p.children) :+ rest*)
        case c: Clip =>
          expand(s"Clip(${c.rect}, ", c.child, s", ${c.affine}, anchor = ${c.localAnchor})")
        case m: MaskGroup =>
          // A group of one mask writes its one flag, and a group of several the flags in order.
          val flags    = m.masks.map(_._2)
          val inverted =
            if flags.size == 1 then flags.head.toString else flags.mkString("(", ", ", ")")
          val rest = s", inverted = $inverted, ${m.affine}, anchor = ${m.localAnchor})"
          expand("MaskGroup(" +: separated(m.masks.map(_._1) :+ m.masked) :+ rest*)
        case a: AnimatedPicture =>
          val rest =
            s", ${a.frameDelays}, startTime = ${a.startTime}, ${a.affine}, anchor = ${a.localAnchor})"
          expand("AnimatedPicture(" +: separated(a.frames) :+ rest*)
        case w: Wrap =>
          val rest = s", dx = ${w.dx}, dy = ${w.dy}, ${w.affine}, anchor = ${w.localAnchor})"
          expand(s"Wrap(${w.period}, ", w.child, rest)
        case leaf: Element => out ++= leaf.toString
    out.toString
end Trees
