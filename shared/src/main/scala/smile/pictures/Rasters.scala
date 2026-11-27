package smile.pictures

import smile.RenderSettings
import smile.infrastructure.{DeviceGrid, Renderer}
import smile.modeling.{Affine, Angles, Bounds, Pos}

import scala.concurrent.{ExecutionContext, Future}

/** This element's pixels, frozen into a bitmap. They are rendered at once with `settings` at
  * `scale` buffer pixels per logical pixel, as [[smile.infrastructure.Renderer.createBitmapFrom]]
  * renders them. The bitmap covers the whole pixels the element's bounds touch, and its anchor is
  * where the element's anchor is.
  *
  * Pixel reads stay at one buffer pixel per logical pixel by default. A larger `scale` is for
  * display on a dense screen, and its bitmap's buffer is that much denser than its bounds. A bitmap
  * of pixels that only its own operations have placed is frozen already, and at the default scale
  * it is its own bitmap. A bitmap of a picture is rendered with `settings`.
  *
  * Something that covers no whole pixel, such as the empty picture or blank space with no width,
  * gives a bitmap with no bounds whose pixels are one transparent pixel, since neither a Java image
  * nor a canvas's pixel data can have none.
  *
  * @throws IllegalArgumentException
  *   if `scale` is not positive and finite, or is not 1 for a `Stepwise` rendering
  */
extension (e: Element)
  def toBitmap(settings: RenderSettings = RenderSettings.Default, scale: Double = 1.0): Bitmap =
    e match
      case bitmap: Bitmap
          if scale == 1.0 && Clipping.isTranslation(bitmap.affine) && isPixels(bitmap) =>
        bitmap
      case _ => Renderer().createBitmapFrom(e, scale, settings)

/** Whether `bitmap`'s source is pixels that exist already. */
private def isPixels(bitmap: Bitmap): Boolean = bitmap.source.source match
  case BitmapSource.Pixels(_) => true
  case _                      => false

/** How bitmaps are built, measured and transformed by their own operations, which record steps on
  * their source as a `smile.pictures.PixelSource` records them.
  *
  * A bitmap's own operation records a step only while its affine only moves, so that the source's
  * frame and the world differ by a translation and a step of the source means the same in both. A
  * pivot or a rectangle given in the world is then moved into the source's frame, and the step
  * moves what the source draws in that frame. Otherwise the operation only composes into the
  * affine, as for any element.
  */
private[smile] object Rasters:

  /** A bitmap of packed ARGB `pixels` in rows from the top. The pixels are copied, so later changes
    * to the array do not affect the bitmap.
    *
    * @throws IllegalArgumentException
    *   if a dimension is not positive, their product does not fit in an `Int`, or there are not
    *   exactly `width * height` pixels
    */
  def fromARGB(width: Int, height: Int, pixels: Array[Int]): Bitmap =
    of(PixelSource.fromARGB(width, height, pixels))

  /** A bitmap of packed ARGB pixels that `generator` gives by column and row, called once per
    * pixel, from left to right in each row, top row first. No `Color` is made per pixel.
    *
    * @throws IllegalArgumentException
    *   if a dimension is not positive or their product does not fit in an `Int`
    */
  def generateARGB(width: Int, height: Int, generator: (Int, Int) => Int): Bitmap =
    of(PixelSource.generateARGB(width, height, generator))

  /** Loads the image at `path` asynchronously, on every platform. In a browser it is the only way
    * to load an image the first time, because decoding there is asynchronous, and afterwards the
    * synchronous `apply` serves the same path.
    */
  def load(path: String): Future[Bitmap] =
    PixelSource.load(path).map(of)(using ExecutionContext.parasitic)

  /** A bitmap of `picture`, rendered only when its pixels are asked for, with the settings of the
    * output that asks. A `Direct` output renders the picture at its own scale, so it stays sharp.
    * The bitmap covers the whole pixels the picture's bounds touch, and its anchor is where the
    * picture's anchor is.
    */
  def of(picture: Element): Bitmap = anchoredAs(of(PixelSource.of(picture)), picture)

  /** A bitmap of `source` where `source` lies, anchored in the middle of its pixels. */
  def of(source: PixelSource): Bitmap =
    new Bitmap(source, Affine.Identity, source.boundary.center)

  /** `bitmap` with its anchor where the anchor of `element`, which it has the pixels of, is. */
  def anchoredAs(bitmap: Bitmap, element: Element): Bitmap =
    if Element.isSomething(element) then bitmap.withAnchor(element.anchor) else bitmap

  /** The box around where `source` lays its pixels once `m` places its frame, or `None` when it
    * lays none. Each tile of the source's plan draws the source's whole pixel rectangle, cut by its
    * clip, so the box goes around the turned corners however the steps turn the pixels.
    */
  def boundsThrough(source: PixelSource, m: Affine): Option[Bounds] =
    if source.isEmpty then None
    else boundsOfPlan(source, m)

  /** The box around the polygons `source`'s plan draws (see
    * `smile.pictures.PixelSource.drawnPolygons`) once `m` places them. A map that keeps the axes
    * takes their box to exactly the box around them mapped, so it maps the box the source keeps.
    */
  private def boundsOfPlan(source: PixelSource, m: Affine): Option[Bounds] =
    if m.keepsAxes then source.drawnBox.map(Element.mapBox(m, _))
    else
      Element.union(
        source.drawnPolygons.iterator.map(polygon => Some(boxAround(polygon.map(m(_)))))
      )

  /** The box around `points`, of which there is at least one. */
  def boxAround(points: Vector[Pos]): Bounds =
    // A loop, since a collection's min would box every coordinate.
    var left   = points.head.x
    var top    = points.head.y
    var right  = left
    var bottom = top
    for p <- points do
      left = math.min(left, p.x)
      top = math.min(top, p.y)
      right = math.max(right, p.x)
      bottom = math.max(bottom, p.y)
    Bounds(left, top, right, bottom)

  /** The world point `p` in the frame of `b`'s source, which `b`'s affine only moves. */
  private def local(b: Bitmap, p: Pos): Pos = Pos(p.x - b.affine.e, p.y - b.affine.f)

  /** `b` with `newSource` in place of its source and its local anchor moved by `move`, which is
    * what the new source's step does to the source's frame.
    */
  private def stepped(b: Bitmap, newSource: PixelSource, move: Affine): Bitmap =
    new Bitmap(newSource, b.affine, move(b.localAnchor))

  /** `b` turned by `degrees` around the world point `pivot`, with the turn recorded on its source
    * when its affine only moves.
    */
  def turned(b: Bitmap, degrees: Double, pivot: Pos): Bitmap =
    val rotation = Affine.rotation(degrees, pivot)
    if !Clipping.isTranslation(b.affine) then b.withAffine(b.affine.andThen(rotation))
    else if degrees % Angles.FullTurn == 0 then b
    else
      val center = local(b, pivot)
      stepped(b, b.source.turnedAround(degrees, center), Affine.rotation(degrees, center))

  /** `b` scaled by `scaling`, which scales by `horizontal` and `vertical` around the world point
    * `pivot`, with the scaling recorded on its source when its affine only moves and the scaled
    * pixels are at least one pixel on each axis.
    */
  def scaled(
      b: Bitmap,
      horizontal: Double,
      vertical: Double,
      pivot: Pos,
      scaling: Affine
  ): Bitmap =
    val box        = b.source.boundary
    val (w, h)     = ((horizontal * box.width).abs, (vertical * box.height).abs)
    val expressive = Clipping.isTranslation(b.affine) && w >= 1 && h >= 1
    if !expressive then b.withAffine(b.affine.andThen(scaling))
    else
      val center = local(b, pivot)
      stepped(
        b,
        b.source.scaleBy(horizontal, vertical, center),
        Affine(
          horizontal,
          0,
          0,
          vertical,
          center.x - horizontal * center.x,
          center.y - vertical * center.y
        )
      )

  /** `b` mirrored along the world axes through its anchor, with the flip recorded on its source
    * when its affine only moves. The source flips across the middle of its pixels, and moving it by
    * twice the way from there to the anchor makes that a flip through the anchor.
    */
  def mirrored(b: Bitmap, horizontally: Boolean, vertically: Boolean): Bitmap =
    val mirroring = Affine.mirroring(horizontally, vertically, b.anchor)
    if !Clipping.isTranslation(b.affine) then b.withAffine(b.affine.andThen(mirroring))
    else
      val (anchor, middle) = (b.localAnchor, b.source.pixelArea.center)
      val flipped          = (horizontally, vertically) match
        case (true, false) => b.source.flipHorizontally
        case (false, true) => b.source.flipVertically
        case _             => b.source.flipDiagonally
      val dx = if horizontally then 2 * (anchor.x - middle.x) else 0.0
      val dy = if vertically then 2 * (anchor.y - middle.y) else 0.0
      new Bitmap(
        if dx == 0 && dy == 0 then flipped else flipped.moveBy(dx, dy),
        b.affine,
        b.localAnchor
      )

  /** `b` cropped to the world rectangle `world`. As for any element's crop, its bounds are exactly
    * the rectangle and its anchor is the rectangle's middle. Where the rectangle lies on whole
    * pixels of `b`'s source inside it, which `b`'s affine only moves, the crop is recorded on the
    * source as a step, so that its pixels are exactly the ones cropped and a crop of a crop is one
    * crop. Otherwise it is the generic crop, a clip of the rectangle over `b`, which keeps `b`'s
    * source.
    */
  def crop(b: Bitmap, world: Bounds): Element =
    val rect             = Element.normalizedBox(world)
    def generic: Element = (b: Element).crop(rect)
    val source           = b.source
    val pixels           = source.boundary
    val (w, h)           = source.pixelSize
    val oneToOne         =
      Clipping.isTranslation(b.affine) && pixels.width == w.toDouble &&
        pixels.height == h.toDouble
    if !oneToOne || rect.width < 1 || rect.height < 1 then generic
    else
      val inside                            = rect.moveBy(-b.affine.e, -b.affine.f)
      val corner                            = pixels.upperLeftCorner
      def onGrid(x: Double, origin: Double) = DeviceGrid.isWhole(x - origin)
      val whole                             = onGrid(inside.upperLeftCorner.x, corner.x) &&
        onGrid(inside.upperLeftCorner.y, corner.y) &&
        onGrid(inside.lowerRightCorner.x, corner.x) &&
        onGrid(inside.lowerRightCorner.y, corner.y)
      val within = inside.upperLeftCorner.x >= corner.x && inside.upperLeftCorner.y >= corner.y &&
        inside.lowerRightCorner.x <= pixels.lowerRightCorner.x &&
        inside.lowerRightCorner.y <= pixels.lowerRightCorner.y
      if !whole || !within then generic
      else
        // Half a pixel in from each corner names that pixel whatever the rounding of the corner.
        val cropped = source.crop(
          inside.upperLeftCorner.x + 0.5,
          inside.upperLeftCorner.y + 0.5,
          inside.lowerRightCorner.x + 0.5,
          inside.lowerRightCorner.y + 0.5
        )
        val exact = new Bitmap(cropped, b.affine, inside.center)
        // A turned source does not cover every pixel of its buffer, and there the generic crop
        // keeps the requested bounds.
        exact.bounds match
          case Some(box) if sameBox(box, rect) => exact.withAnchor(rect.center)
          case _                               => generic

  /** Whether `a` and `b` are the same box, up to the float error of placing them. */
  private def sameBox(a: Bounds, b: Bounds): Boolean =
    def near(p: Pos, q: Pos) = (p.x - q.x).abs <= 1e-9 && (p.y - q.y).abs <= 1e-9
    near(a.upperLeftCorner, b.upperLeftCorner) && near(a.lowerRightCorner, b.lowerRightCorner)

  /** `b` shifted with wraparound by `dx` and `dy`, as any element's `wrapShift` shifts it. The
    * bounds and the anchor stay where they are. Where `b`'s affine only moves it, its source has
    * one pixel per logical pixel and those pixels lie on whole logical pixels, the shift is
    * recorded on the source as a step, so that shifts in a row fold into one and the pixels are
    * exactly the ones shifted. Otherwise it is the generic wrap shift.
    */
  def wrapShifted(b: Bitmap, dx: Int, dy: Int): Element =
    val pixels = b.source.boundary
    val (w, h) = b.source.pixelSize
    // The generic shift clips its tiles to the bounds, so the own one agrees with it only where the
    // pixels lie on whole logical pixels.
    val corner   = b.affine(pixels.upperLeftCorner)
    val oneToOne =
      Clipping.isTranslation(b.affine) && pixels.width == w.toDouble &&
        pixels.height == h.toDouble && b.source.exactPlan.keepsPixelGrid &&
        DeviceGrid.isWhole(corner.x) && DeviceGrid.isWhole(corner.y)
    if !oneToOne then (b: Element).wrapShift(dx.toDouble, dy.toDouble)
    else
      val shifted = b.source.wrapShift(dx, dy)
      if shifted eq b.source then b else new Bitmap(shifted, b.affine, b.localAnchor)
end Rasters
