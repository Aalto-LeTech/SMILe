package smile.pictures

// Test helpers that look inside compositions, for the suites that check what layout and
// composition build. Only the tests see them.

extension (e: Element)

  /** The parts of a composition, front first: a picture's children, nothing for the empty picture,
    * and otherwise the element itself.
    */
  def elements: Vector[Element] = e match
    case picture: Picture => picture.children
    case Empty            => Vector()
    case other            => Vector(other)

  /** This element, which an operation of a bitmap's own gives, as the bitmap it is. */
  def asBitmap: Bitmap = e match
    case bitmap: Bitmap => bitmap
    case other          => throw new AssertionError(s"Expected a bitmap, was $other")
end extension

extension (e: Element)

  /** The vertices of a polygon or a line in the world, in the order they were given. */
  def vertices: Vector[smile.modeling.Pos] = e match
    case shape: Shape =>
      shape.shape match
        case path: smile.modeling.PathShape =>
          Vector.tabulate(path.xy.length / 2)(i => shape.affine(path.xy(2 * i), path.xy(2 * i + 1)))
        case other => throw new AssertionError(s"$other has no vertices")
    case other => throw new AssertionError(s"$other has no vertices")
end extension
