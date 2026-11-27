package smile.pictures

import smile.modeling.Pos

/** The points of a stroked path's outline that can extend its bounding box, where the stroke's ink
  * ends. A stroke paints half its width to each side of its path, and at the path's ends and
  * corners it paints whatever the cap and the join add. The stroked support function of a path,
  * which gives the bounds of polygons and lines (see `smile.modeling.PathShape`), is measured over
  * these points, so the bounds hold exactly what the renderers paint. Miter tips and cap corners
  * are not clipped, and no margin is reserved for ink that never comes.
  *
  * Directions are unit vectors, and every result is in the coordinates of the points passed in.
  */
private[smile] object StrokeOutline:

  /** The unit vector pointing the way `vector` does, or `None` for a vector without a direction. */
  def direction(vector: Pos): Option[Pos] =
    val length = math.hypot(vector.x, vector.y)
    Option.when(length > 0 && !length.isInfinite)(Pos(vector.x / length, vector.y / length))

  /** The two points where the stroke's sides pass `point`, for a path running through it in
    * `direction`.
    */
  def sides(point: Pos, direction: Pos, radius: Double): Seq[Pos] =
    val offset = normalOf(direction) * radius
    Seq(point + offset, point - offset)

  /** The points a cap adds at the end of an open path, beyond the `sides` there.
    *
    * @param outward
    *   The direction the path would continue in past this end.
    */
  def cap(end: Pos, outward: Pos, radius: Double, style: StrokeStyle.Cap): Seq[Pos] =
    style match
      // A butt cap cuts the stroke off square at the end itself.
      case StrokeStyle.Cap.Butt => Seq.empty
      // A round cap is half a disc, and the stroke's body covers the disc's other half.
      case StrokeStyle.Cap.Round => disc(end, radius)
      // A square cap carries the stroke's sides on by the radius.
      case StrokeStyle.Cap.Square => sides(end + outward * radius, outward, radius)

  /** The points a join adds at a corner of a path, beyond the `sides` of the two segments meeting
    * there.
    *
    * @param incoming
    *   The direction the path arrives at the corner in.
    * @param outgoing
    *   The direction it leaves in.
    */
  def join(
      corner: Pos,
      incoming: Pos,
      outgoing: Pos,
      radius: Double,
      style: StrokeStyle.Join
  ): Seq[Pos] =
    style match
      // A bevel join connects the two segments' sides with a straight edge.
      case StrokeStyle.Join.Bevel => Seq.empty
      case StrokeStyle.Join.Round => disc(corner, radius)
      case StrokeStyle.Join.Miter => miterTip(corner, incoming, outgoing, radius).toSeq

  /** The point of a miter join, where the outer sides of the two segments meet when they are
    * carried on past the corner. `None` where the path runs straight on, and where the corner is so
    * sharp that the renderers cut the miter off into a bevel (see
    * [[smile.pictures.StrokeStyle.MiterLimit]]).
    */
  private def miterTip(corner: Pos, incoming: Pos, outgoing: Pos, radius: Double): Option[Pos] =
    val turn = incoming.x * outgoing.y - incoming.y * outgoing.x
    if turn == 0 then None
    else
      // The normals of the two segments on the outer side of the turn.
      val side          = if turn > 0 then -1.0 else 1.0
      val incomingSide  = normalOf(incoming) * side
      val outgoingSide  = normalOf(outgoing) * side
      val alignment     = 1 + incomingSide.x * outgoingSide.x + incomingSide.y * outgoingSide.y
      val miterPerWidth = 1 / math.sqrt(alignment / 2)

      // Written this way, a right-angled corner between axis-parallel segments gets its tip
      // without a rounding error. The tip is the sum of the normals divided by exactly one.
      Option.when(alignment > 0 && miterPerWidth <= StrokeStyle.MiterLimit)(
        corner + (incomingSide + outgoingSide) * (radius / alignment)
      )

  /** The leftmost, topmost, rightmost, and bottommost points of the disc around `center`. */
  private def disc(center: Pos, radius: Double): Seq[Pos] =
    Seq(
      center + (radius, 0.0),
      center - (radius, 0.0),
      center + (0.0, radius),
      center - (0.0, radius)
    )

  private def normalOf(direction: Pos): Pos = Pos(-direction.y, direction.x)
