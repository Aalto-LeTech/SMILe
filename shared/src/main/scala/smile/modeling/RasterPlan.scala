package smile.modeling

import smile.{RenderSettings, TransformMethod}

/** One drawing of a raster's source. It holds the map from the source's pixels to world
  * coordinates, and the convex polygon in world coordinates outside which nothing of it is drawn.
  *
  * @param toWorld
  *   Takes a point of the source, in its pixels, to where it is drawn in the world.
  * @param clip
  *   The corners of a convex polygon in world coordinates, in order, or `None` when the whole
  *   source is drawn.
  */
private[smile] final case class RasterTile(toWorld: Affine, clip: Option[Vector[Pos]]):

  /** This tile drawn after `map`, which moves, turns, scales or mirrors the world. */
  def followedBy(map: Affine): RasterTile =
    RasterTile(toWorld.andThen(map), clip.map(_.map(map(_))))

  /** Whether the tile draws nothing, because its clip has no area. */
  def isEmpty: Boolean = clip.exists(corners => RasterPlan.area(corners) < RasterPlan.Negligible)

  /** This tile with nothing drawn outside the rectangle `bounds`. */
  def clippedTo(bounds: Bounds): RasterTile =
    val rectangle = Vector(
      bounds.upperLeftCorner,
      bounds.upperRightCorner,
      bounds.lowerRightCorner,
      bounds.lowerLeftCorner
    )
    RasterTile(toWorld, Some(clip.fold(rectangle)(RasterPlan.intersect(_, bounds))))
end RasterTile

/** How an output draws a raster's source pixels. The source is resampled once, when it is drawn,
  * however many operations were applied to the raster. Each tile is the source drawn with its map,
  * under the output's own scale, inside its clip.
  *
  * @param tiles
  *   The drawings that make up the raster, in drawing order.
  */
private[smile] final case class RasterPlan(tiles: Vector[RasterTile]):

  /** This plan with every tile followed by `map`. */
  def followedBy(map: Affine): RasterPlan = RasterPlan(tiles.map(_.followedBy(map)))

  /** This plan with every tile clipped to `bounds`, and without the tiles that leaves empty. */
  def clippedTo(bounds: Bounds): RasterPlan =
    RasterPlan(tiles.map(_.clippedTo(bounds)).filterNot(_.isEmpty))

  /** This plan, which lies inside `bounds`, with what it draws moved right by `dx` and down by `dy`
    * and wrapped around inside `bounds`. What leaves it on one side comes back on the other. Each
    * offset is at least zero and less than the size of `bounds` along its axis. Each tile becomes
    * up to four, the parts that move and the parts that wrap around.
    */
  def wrapShifted(bounds: Bounds, dx: Double, dy: Double): RasterPlan =
    val (left, top)     = (bounds.upperLeftCorner.x, bounds.upperLeftCorner.y)
    val (right, bottom) = (bounds.lowerRightCorner.x, bounds.lowerRightCorner.y)
    val (width, height) = (right - left, bottom - top)

    // The part of every tile up to the line at the given offset from the far side moves by the
    // offset, and the rest moves back by the size less the offset, to the near side.
    def along(
        plan: RasterPlan,
        offset: Double,
        size: Double,
        part: (Double, Double) => Bounds,
        move: Double => Affine
    ): RasterPlan =
      if offset == 0 then plan
      else
        val far  = part(0, size - offset)
        val near = part(size - offset, size)
        RasterPlan(
          plan.tiles
            .flatMap(tile =>
              Vector(
                tile.clippedTo(far).followedBy(move(offset)),
                tile.clippedTo(near).followedBy(move(offset - size))
              )
            )
            .filterNot(_.isEmpty)
        )

    val horizontally = along(
      this,
      dx,
      width,
      (from, to) => Bounds(Pos(left + from, top), Pos(left + to, bottom)),
      Affine.translation(_, 0)
    )
    along(
      horizontally,
      dy,
      height,
      (from, to) => Bounds(Pos(left, top + from), Pos(right, top + to)),
      Affine.translation(0, _)
    )
  end wrapShifted

  /** This plan moved by `(dx, dy)`. */
  def movedBy(dx: Double, dy: Double): RasterPlan =
    if dx == 0 && dy == 0 then this else followedBy(Affine.translation(dx, dy))

  /** Whether every tile lays the source's pixels on a pixel grid, so that drawing the pixels as
    * they are is exact.
    */
  def keepsPixelGrid: Boolean = tiles.forall(_.toWorld.keepsPixelGrid)

  /** Whether every number in the plan is finite. */
  def isFinite: Boolean =
    tiles.forall(tile =>
      tile.toWorld.isFinite && tile.clip.forall(
        _.forall(point => point.x.isFinite && point.y.isFinite)
      )
    )

  /** The interpolation an output with `settings` draws this plan with. A plan that turns the axes
    * takes `bufferTransformMethod`, as a turned buffer is resampled, and any other plan takes
    * `bufferScalingMethod`, as a scaled buffer is.
    */
  def interpolation(settings: RenderSettings): RasterInterpolation =
    if tiles.exists(_.toWorld.turnsAxes) then
      settings.bufferTransformMethod match
        case TransformMethod.Nearest  => RasterInterpolation.Nearest
        case TransformMethod.Bilinear => RasterInterpolation.Bilinear
        case TransformMethod.Bicubic  => RasterInterpolation.Bicubic
    else if settings.bufferScalingMethod.copiesNearestPixel then RasterInterpolation.Nearest
    else RasterInterpolation.Bilinear
end RasterPlan

private[smile] object RasterPlan:

  /** An area so small, in square pixels, that a polygon of it draws nothing. */
  private[modeling] val Negligible = 1e-9

  /** The area of the polygon with the corners `corners` in order, either way around. */
  def area(corners: Vector[Pos]): Double =
    if corners.length < 3 then 0.0
    else
      val doubled = corners.indices
        .map: index =>
          val (current, next) = (corners(index), corners((index + 1) % corners.length))
          current.x * next.y - next.x * current.y
        .sum
      doubled.abs / 2

  /** The part of the convex polygon `polygon` inside the convex polygon `clip`, by clipping it
    * against each of the clip's sides in turn, as `intersect` does against a rectangle's.
    */
  def intersectConvex(polygon: Vector[Pos], clip: Vector[Pos]): Vector[Pos] =
    if clip.length < 3 then Vector.empty
    else
      // Which side of each edge is inside depends on which way around the clip goes.
      val orientation = clip.indices
        .map: index =>
          val (current, next) = (clip(index), clip((index + 1) % clip.length))
          current.x * next.y - next.x * current.y
        .sum
        .sign
      clip.indices.foldLeft(polygon): (points, index) =>
        val (from, to) = (clip(index), clip((index + 1) % clip.length))
        // Positive on the inner side of the clip's edge from its start point to its end point, and
        // zero on the edge.
        def side(point: Pos): Double =
          orientation * ((to.x - from.x) * (point.y - from.y) - (to.y - from.y) * (point.x - from.x))
        if points.isEmpty then points
        else
          points.indices.toVector.flatMap: pointIndex =>
            val current  = points(pointIndex)
            val previous = points((pointIndex + points.length - 1) % points.length)
            val (previousSide, currentSide) = (side(previous), side(current))
            // On an edge along an axis, the crossing takes the edge's own coordinate, which
            // interpolation would round, so that clipping rectangles to rectangles gives their
            // exact corners.
            def crossing: Pos =
              val t = previousSide / (previousSide - currentSide)
              Pos(
                if from.x == to.x then from.x else previous.x + t * (current.x - previous.x),
                if from.y == to.y then from.y else previous.y + t * (current.y - previous.y)
              )
            (previousSide >= 0, currentSide >= 0) match
              case (true, true)   => Vector(current)
              case (true, false)  => Vector(crossing)
              case (false, true)  => Vector(crossing, current)
              case (false, false) => Vector.empty
  end intersectConvex

  /** A source of `width` by `height` pixels laid on the rectangle `bounds`. */
  def of(bounds: Bounds, width: Int, height: Int): RasterPlan =
    val toWorld = Affine
      .uncheckedScaling(bounds.width / width, bounds.height / height)
      .translatedBy(bounds.upperLeftCorner.x, bounds.upperLeftCorner.y)
    RasterPlan(Vector(RasterTile(toWorld, None)))

  /** The part of the convex polygon `polygon` inside `bounds`, by clipping it against each of the
    * rectangle's sides in turn (the Sutherland and Hodgman algorithm).
    */
  def intersect(polygon: Vector[Pos], bounds: Bounds): Vector[Pos] =
    val left   = bounds.upperLeftCorner.x
    val top    = bounds.upperLeftCorner.y
    val right  = bounds.lowerRightCorner.x
    val bottom = bounds.lowerRightCorner.y

    // Each side keeps the points on its inner side, which the first function tests, and puts a
    // point where an edge crosses it, which the second function finds from the edge's two ends.
    def clip(
        points: Vector[Pos],
        inside: Pos => Boolean,
        crossing: (Pos, Pos) => Pos
    ): Vector[Pos] =
      if points.isEmpty then points
      else
        points.indices.toVector.flatMap: index =>
          val current  = points(index)
          val previous = points((index + points.length - 1) % points.length)
          (inside(previous), inside(current)) match
            case (true, true)   => Vector(current)
            case (true, false)  => Vector(crossing(previous, current))
            case (false, true)  => Vector(crossing(previous, current), current)
            case (false, false) => Vector.empty

    def atX(x: Double)(from: Pos, to: Pos): Pos =
      val t = (x - from.x) / (to.x - from.x)
      Pos(x, from.y + t * (to.y - from.y))

    def atY(y: Double)(from: Pos, to: Pos): Pos =
      val t = (y - from.y) / (to.y - from.y)
      Pos(from.x + t * (to.x - from.x), y)

    val clippedLeft   = clip(polygon, _.x >= left, atX(left))
    val clippedRight  = clip(clippedLeft, _.x <= right, atX(right))
    val clippedTop    = clip(clippedRight, _.y >= top, atY(top))
    val clippedBottom = clip(clippedTop, _.y <= bottom, atY(bottom))
    clippedBottom
  end intersect
end RasterPlan

/** How an output resamples a raster's source as it draws it. */
private[smile] enum RasterInterpolation derives CanEqual:
  case Nearest, Bilinear, Bicubic
