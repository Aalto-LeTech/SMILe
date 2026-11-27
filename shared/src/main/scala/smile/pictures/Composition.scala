package smile.pictures

import smile.modeling.{Affine, Align, Bounds, Pos, PositionType, Side}

extension (e: Element)

  /** This element with `content` added in front of it, moved so that the point of its bounds that
    * `positionType` names is at `(x, y)`. That point is the middle of its bounds by default, or
    * their upper left corner.
    */
  def addAt(
      content: Element,
      x: Double,
      y: Double,
      positionType: PositionType = PositionType.Center
  ): Element =
    e.addToFront(Composition.placedAt(content, x, y, positionType))

  /** This element with every element of `contentsAndCoordinates` added in front of it, the first in
    * front, each moved so that the point of its bounds that `positionType` names is at its
    * coordinates.
    */
  def addAt(
      contentsAndCoordinates: Seq[(Element, Double, Double)],
      positionType: PositionType
  ): Element =
    val placed = contentsAndCoordinates.map((content, x, y) =>
      Composition.placedAt(content, x, y, positionType)
    )
    Composition.combined(placed :+ e)

  /** `content` placed onto this element as `addAt` adds it, and the result cropped to this
    * element's bounds as `crop` crops it, so nothing reaches outside this background.
    *
    * Placing the empty picture gives this element unchanged, and placing anything onto the empty
    * picture gives the empty picture, since its lack of bounds clips everything away.
    *
    * Successive placements share one crop when the intervening transform preserves the axes.
    * Placing once per animation tick then keeps the crop depth constant.
    *
    * A background turned by an angle that is not a quarter turn has bounds that are the box around
    * the turned rectangle, and a place crops to that box. Turning the result again takes the box
    * around that box, so a fold that places and then turns grows without bound. For a square
    * background turned by `a`, the side grows by `abs(cos a) + abs(sin a)` per step, about 1.08 at
    * 5 degrees. To avoid it, crop back to a fixed window after each step.
    */
  def place(
      content: Element,
      x: Double,
      y: Double,
      positionType: PositionType = PositionType.Center
  ): Element =
    if !Element.isSomething(content) then e
    else Composition.placed(e, _.addAt(content, x, y, positionType))

  /** Every element of `contentsAndCoordinates` placed onto this element as `addAt` adds them, and
    * the result cropped to this element's bounds as `crop` crops it. Placing nothing but empty
    * pictures gives this element unchanged, and placing anything onto the empty picture gives the
    * empty picture. Places in a row stay as shallow as one, as with a single content.
    */
  def place(
      contentsAndCoordinates: Seq[(Element, Double, Double)],
      positionType: PositionType
  ): Element =
    if !contentsAndCoordinates.exists((content, _, _) => Element.isSomething(content)) then e
    else Composition.placed(e, _.addAt(contentsAndCoordinates, positionType))

  /** This element with `content` in front of it.
    *
    * ```scala
    * import smile.colors.PresetColor.*
    * import smile.modeling.*
    *
    * val disc   = Circle(Pos.Origin, 40, fillWith(Crimson), None)
    * val target = disc.addToFront(Circle(Pos.Origin, 20, fillWith(White), None))
    * ```
    *
    * ![A white disc in front of a larger red one](pictures/target.png)
    */
  def addToFront(content: Element): Element = Composition.combined(Seq(content, e))

  /** This element with `content` behind it. */
  def addToBack(content: Element): Element = Composition.combined(Seq(e, content))

  /** This element with `content` added on `targetSide`, `gap` pixels away and aligned along that
    * side by `align`, in front of this element.
    *
    * ```scala
    * import smile.colors.PresetColor.*
    * import smile.modeling.*
    *
    * val square = Rectangle(60, fillWith(Teal), None)
    * val pair   = square.addToRight(Circle(Pos.Origin, 20, fillWith(Gold), None), gap = 10)
    * ```
    *
    * ![A teal square with a golden circle to its right](pictures/pair.png)
    */
  def addTo(targetSide: Side, content: Element, gap: Double = 0, align: Align.Generic): Element =
    Composition.addAllTo(e, targetSide, Iterator.single(content), gap, align)

  /** This element with `content` added above it, `gap` pixels away and aligned by `align`. */
  def addToTop(
      content: Element,
      gap: Double = 0,
      align: Align.Horizontal = Align.Horizontal.Left
  ): Element =
    Composition.addAllTo(e, Side.Top, Iterator.single(content), gap, align.toGeneric)

  /** This element with `content` added to its right, `gap` pixels away and aligned by `align`. */
  def addToRight(
      content: Element,
      gap: Double = 0,
      align: Align.Vertical = Align.Vertical.Middle
  ): Element =
    Composition.addAllTo(e, Side.Right, Iterator.single(content), gap, align.toGeneric)

  /** This element with `content` added below it, `gap` pixels away and aligned by `align`. */
  def addToBottom(
      content: Element,
      gap: Double = 0,
      align: Align.Horizontal = Align.Horizontal.Left
  ): Element =
    Composition.addAllTo(e, Side.Bottom, Iterator.single(content), gap, align.toGeneric)

  /** This element with `content` added to its left, `gap` pixels away and aligned by `align`. */
  def addToLeft(
      content: Element,
      gap: Double = 0,
      align: Align.Vertical = Align.Vertical.Middle
  ): Element =
    Composition.addAllTo(e, Side.Left, Iterator.single(content), gap, align.toGeneric)

  /** `alternateWith` downwards. */
  def alternateDownwardsWith(
      alternatives: Seq[Element],
      numberOfAlternations: Int,
      gap: Double = 0,
      align: Align.Horizontal = Align.Horizontal.Left
  ): Element =
    e.alternateWith(alternatives, Side.Bottom, numberOfAlternations, gap, align.toGeneric)

  /** `alternateWith` rightwards. */
  def alternateRightwardsWith(
      alternatives: Seq[Element],
      numberOfAlternations: Int,
      gap: Double = 0,
      align: Align.Vertical = Align.Vertical.Middle
  ): Element =
    e.alternateWith(alternatives, Side.Right, numberOfAlternations, gap, align.toGeneric)

  /** This element with `numberOfAlternations` elements added on `side`, one after another, each
    * next to all before it. First come the alternatives in order, then this element, then the
    * alternatives again, and so on.
    *
    * ```scala
    * import smile.colors.PresetColor.*
    * import smile.modeling.*
    *
    * val red   = Circle(Pos.Origin, 12, fillWith(Crimson), None)
    * val blue  = Circle(Pos.Origin, 12, fillWith(Navy), None)
    * val beads = red.alternateRightwardsWith(Seq(blue), 6, gap = 4)
    * ```
    *
    * ![A row of beads, red and navy by turns](pictures/beads.png)
    *
    * @throws IllegalArgumentException
    *   if the number of alternations is negative
    */
  def alternateWith(
      alternatives: Seq[Element],
      side: Side,
      numberOfAlternations: Int,
      gap: Double = 0,
      align: Align.Generic
  ): Element =
    require(
      numberOfAlternations >= 0,
      s"Number of alternations cannot be negative (was $numberOfAlternations)"
    )
    val cycle = alternatives.length + 1
    val added = Iterator
      .range(1, numberOfAlternations + 1)
      .map(n => if n % cycle == 0 then e else alternatives(n % cycle - 1))
    Composition.addAllTo(e, side, added, gap, align)

  /** `replicateTo` downwards. */
  def replicateDownwards(
      numberOfReplicas: Int,
      gap: Double = 0,
      align: Align.Horizontal = Align.Horizontal.Left,
      transformer: Element => Element = identity
  ): Element =
    e.replicateTo(Side.Bottom, numberOfReplicas, gap, align.toGeneric, transformer)

  /** `replicateTo` rightwards. */
  def replicateRightwards(
      numberOfReplicas: Int,
      gap: Double = 0,
      align: Align.Vertical = Align.Vertical.Middle,
      transformer: Element => Element = identity
  ): Element =
    e.replicateTo(Side.Right, numberOfReplicas, gap, align.toGeneric, transformer)

  /** This element with `numberOfReplicas` copies added on `side`, one after another, each next to
    * all before it. Each copy is `transformer` applied to the copy before it, the first to this
    * element.
    *
    * ```scala
    * import smile.colors.PresetColor.*
    * import smile.modeling.*
    *
    * val dot = Circle(Pos.Origin, 20, fillWith(SteelBlue), None)
    * val row = dot.replicateRightwards(4, gap = 8)
    * ```
    *
    * ![Five blue dots in a row](pictures/row.png)
    *
    * @throws IllegalArgumentException
    *   if the number of replicas is negative
    */
  def replicateTo(
      side: Side,
      numberOfReplicas: Int,
      gap: Double = 0,
      align: Align.Generic,
      transformer: Element => Element = identity
  ): Element =
    require(numberOfReplicas >= 0, s"Number of replicas cannot be negative (was $numberOfReplicas)")
    val replicas = Iterator.iterate(e)(transformer).slice(1, numberOfReplicas + 1)
    Composition.addAllTo(e, side, replicas, gap, align)
end extension

/** How composition places and combines elements. */
private[pictures] object Composition:

  /** `content` moved so that the point of its bounds that `positionType` names is at `(x, y)`. */
  def placedAt(content: Element, x: Double, y: Double, positionType: PositionType): Element =
    content.bounds match
      case None      => content
      case Some(box) =>
        val at = positionType match
          case PositionType.Center          => box.center
          case PositionType.UpperLeftCorner => box.upperLeftCorner
        content.moveBy(x - at.x, y - at.y)

  /** `background` with `add` adding to it, cropped to the background's bounds, as `place` places. A
    * background that is itself a clip that keeps the axes, as an earlier `place` gives, has the
    * content added to its child, placed as the clip places it, under the same crop. The clip's
    * rectangle there is its bounds. It draws the same, since the content is clipped to the same
    * rectangle either way. Places in a row then keep one clip. Nesting one clip per place would
    * make a long fold of them too deep to draw.
    */
  def placed(background: Element, add: Element => Element): Element =
    background match
      case clip: Clip if clip.affine.keepsAxes =>
        add(Element.placedBy(clip.child, clip.affine)).crop(Element.mapBox(clip.affine, clip.rect))
      case _ => background.bounds.fold(Empty)(box => add(background).crop(box))

  /** `parts`, the first in front, as one element. The empty pictures among them are left out. A
    * single remaining part is given as it is, and several as a picture anchored at the middle of
    * its bounds (see `grouped`).
    */
  def combined(parts: Seq[Element]): Element =
    parts.filter(Element.isSomething) match
      case Seq()     => Empty
      case Seq(only) => only
      case kept      => grouped(kept)

  /** `parts`, the first in front, none of them the empty picture and at least one, as one picture
    * anchored at the middle of its bounds, kept flat so that composing step by step does not nest
    * one picture per step, however the steps move, turn or anchor what they build.
    *
    * A part that is a picture gives its children in its place. Its own anchor is lost, but no one
    * sees it. A picture's parts are not public, and a transform of the whole composes into the
    * whole's affine, so a picture placed by the identity gives its children as they are.
    *
    * A picture placed by another affine that has more children than all the other parts give lends
    * the result its affine and keeps its children as they are, and every other part is placed into
    * its frame by the inverse of that affine, so that a picture that was moved or turned before
    * something was added to it stays one level deep, and adding to it costs what is added. The
    * inverse of a move is exact, since moves lie on the grid of 2^-20 pixel. A part placed by the
    * inverse of a turn or a scaling lands within a few steps of that grid, far below a device pixel
    * but not exact, so a turned or scaled picture lends its frame only when it already holds a
    * turned or scaled picture, that is, when keeping it as one part would nest a second level. A
    * single composition keeps the exact bounds of its parts, and a fold of them stays two levels
    * deep. A smaller picture is kept as one part, too.
    */
  def grouped(parts: Seq[Element]): Picture =
    val worldBox = Element.union(parts.iterator.map(_.worldBounds))
    // How many elements each part gives. A picture placed by the identity gives its children.
    def count(part: Element): Int = part match
      case p: Picture if p.affine == Affine.Identity => p.children.size
      case _                                         => 1
    val host = parts.iterator
      .collect { case p: Picture if hosts(p) => p }
      .maxByOption(_.children.size)
      .filter(p => p.children.size > parts.iterator.filterNot(_ eq p).map(count).sum)
    host match
      case None =>
        // Every picture that is placed by the identity gives its children as they are.
        val children = parts.iterator
          .map {
            case p: Picture if p.affine == Affine.Identity => p.children
            case other                                     => Vector(other)
          }
          .reduceLeft(_ ++ _)
        Picture.of(children, Some(worldBox))
      case Some(frame) =>
        val toFrame = frame.affine.inverse
        val others  = parts.iterator.map { part =>
          if part eq frame then None
          else
            Some(part match
              case p: Picture if p.affine == Affine.Identity =>
                p.children.map(Element.placedBy(_, toFrame))
              case other => Vector(Element.placedBy(other, toFrame)))
        }.toVector
        // Concatenating vectors shares the longer one's structure, so a scene built by adding one
        // element at a time does not copy all its children on every addition.
        val children = others
          .map(_.getOrElse(frame.children))
          .reduceLeft(_ ++ _)
        val box = Element.union(
          Iterator(frame.parts.box) ++ others.iterator.flatten.flatten.map(_.worldBounds)
        )
        val anchor = worldBox.fold(Pos.Origin)(_.center)
        new Picture(
          new Picture.Parts(children, Some(box)),
          frame.affine,
          Element.localAnchorAt(frame.affine, anchor)
        )
    end match
  end grouped

  /** Whether `p` can have other parts placed into its frame by the inverse of its affine. Its
    * affine is other than the identity, which needs no frame, and neither shrinks nor stretches so
    * much that the inverse could overflow. Unless it only moves, which inverts exactly, `p` also
    * already holds a picture that is not only moved, so that keeping `p` as one part would nest a
    * second level.
    */
  private def hosts(p: Picture): Boolean =
    val m = p.affine
    m != Affine.Identity && {
      val scale = math.sqrt(m.determinant.abs)
      scale >= MinimumHostScale && scale <= 1 / MinimumHostScale
    } && (Clipping.isTranslation(m) || p.children.exists {
      case child: Picture => !Clipping.isTranslation(child.affine)
      case _              => false
    })

  private val MinimumHostScale = 1.0 / 1048576 // 2^-20

  /** `existing` with every element of `contents` added on `side`, each placed next to the bounds of
    * everything before it as `addTo` places one, and in front of it. The contents are consumed in
    * order, and the result is built once at the end.
    *
    * @throws IllegalArgumentException
    *   if `gap` is not finite
    */
  def addAllTo(
      existing: Element,
      side: Side,
      contents: Iterator[Element],
      gap: Double,
      align: Align.Generic
  ): Element =
    require(gap.isFinite, s"A gap must be finite, not $gap")
    var parts = List(existing)
    var box   = existing.bounds
    for content <- contents; contentBox <- content.bounds do
      val placed = box match
        case None        => content
        case Some(sofar) =>
          val corner = upperLeftCornerWhenAddingTo(side, sofar, contentBox, gap, align)
          content.moveBy(
            corner.x - contentBox.upperLeftCorner.x,
            corner.y - contentBox.upperLeftCorner.y
          )
      parts = placed :: parts
      box = Element.union(Iterator(box, placed.bounds))
    combined(parts)

  /** The upper left corner at which content bounded by `added` lands when it is added on `side` of
    * `existing`.
    */
  private def upperLeftCornerWhenAddingTo(
      side: Side,
      existing: Bounds,
      added: Bounds,
      gap: Double,
      align: Align.Generic
  ): Pos =
    side match
      case Side.Top =>
        Pos(
          existing.horizontalPositionFor(align.toHorizontal, added),
          existing.upperLeftCorner.y - gap - added.height
        )
      case Side.Bottom =>
        Pos(
          existing.horizontalPositionFor(align.toHorizontal, added),
          existing.lowerRightCorner.y + gap
        )
      case Side.Left =>
        Pos(
          existing.upperLeftCorner.x - gap - added.width,
          existing.verticalPositionFor(align.toVertical, added)
        )
      case Side.Right =>
        Pos(
          existing.lowerRightCorner.x + gap,
          existing.verticalPositionFor(align.toVertical, added)
        )
end Composition
