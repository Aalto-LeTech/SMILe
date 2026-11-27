package smile.infrastructure

import smile.RenderSettings
import smile.colors.Color
import smile.modeling.{Affine, RasterInterpolation}

import java.awt.*
import java.awt.geom.AffineTransform
import java.awt.image.{AffineTransformOp, BufferedImage}
import java.io.File
import javax.imageio.ImageIO
import scala.util.Using
import scala.util.Using.Releasable

private[smile] object JVMBufferAdapter:
  /** Lets `Using` dispose of a `Graphics2D`, which is not `AutoCloseable`. */
  private given Releasable[Graphics2D] = _.dispose()

  /** How many bordered copies `bordered` has made, which tests read to see the caches in use. */
  private[smile] val builtBorders = new java.util.concurrent.atomic.AtomicLong()

  /** How many premultiplied copies `premultipliedImage` has made, which tests read to see the cache
    * in use.
    */
  private[smile] val builtPremultiplied = new java.util.concurrent.atomic.AtomicLong()

  /** The `width` by `height` pixels of `source` from column `left` and row `top`, with a border of
    * one pixel around them that repeats their edge pixels, as
    * `smile.infrastructure.JSBufferAdapter.drawPadded` draws them on a canvas. A renderer resamples
    * this so that the pixels cover every device pixel their clip keeps. The rows are copied as they
    * are where `source` is packed ARGB, and converted from its color model elsewhere, and the copy
    * is premultiplied ARGB. A `transparent` border has no alpha. Resampled with it, the pixels'
    * edges fade out over a source pixel, which antialiases them.
    */
  private[smile] def bordered(
      source: BufferedImage,
      left: Int,
      top: Int,
      width: Int,
      height: Int,
      transparent: Boolean = false
  ): BufferedImage =
    val _                     = builtBorders.incrementAndGet()
    val (outWidth, outHeight) = (width + 2, height + 2)
    val packed                =
      source.getType == BufferedImage.TYPE_INT_ARGB && !source.isAlphaPremultiplied
    val inner =
      if packed then
        source.getRaster.getDataElements(left, top, width, height, null).asInstanceOf[Array[Int]]
      else source.getRGB(left, top, width, height, null, 0, width)
    val data = new Array[Int](outWidth * outHeight)
    var y    = 0
    while y < outHeight do
      val from = (y - 1).max(0).min(height - 1) * width
      val to   = y * outWidth
      System.arraycopy(inner, from, data, to + 1, width)
      data(to) = inner(from)
      data(to + outWidth - 1) = inner(from + width - 1)
      y += 1
    if transparent then
      // The border keeps its colors with no alpha, so that a filter that blends them in does not
      // darken the edge.
      var i = 0
      while i < outWidth do
        data(i) &= 0xffffff
        data(data.length - 1 - i) &= 0xffffff
        i += 1
      var row = 1
      while row < outHeight - 1 do
        data(row * outWidth) &= 0xffffff
        data(row * outWidth + outWidth - 1) &= 0xffffff
        row += 1
    val result = new BufferedImage(outWidth, outHeight, BufferedImage.TYPE_INT_ARGB)
    result.getRaster.setDataElements(0, 0, outWidth, outHeight, data)
    premultiplied(result)

  /** `image` as premultiplied ARGB, as Java2D converts it, which is the form its filters weigh
    * pixels in. A copy kept in this form for an image resampled on every frame saves converting its
    * samples on every frame, and resampled, it gives exactly the pixels the image gives.
    */
  private[smile] def premultiplied(image: BufferedImage): BufferedImage =
    if image.getType == BufferedImage.TYPE_INT_ARGB_PRE then image
    else
      val copy =
        new BufferedImage(image.getWidth, image.getHeight, BufferedImage.TYPE_INT_ARGB_PRE)
      val g = copy.createGraphics()
      try
        g.setComposite(AlphaComposite.Src)
        val _ = g.drawImage(image, 0, 0, null)
      finally g.dispose()
      copy

  /** A fresh 1x1 buffer representing "no content". See `smile.infrastructure.BufferAdapter.Empty`.
    */
  def Empty: JVMBufferAdapter = JVMBufferAdapter(1, 1)

  /** Sets up `g` from `settings` as every graphics context SMILe draws with is set up, with its
    * antialiasing for shapes and for text and with how strokes are placed.
    */
  private[smile] def configure(g: Graphics2D, settings: RenderSettings): Unit =
    val antialiasingState =
      if settings.drawingIsAntiAliased then RenderingHints.VALUE_ANTIALIAS_ON
      else RenderingHints.VALUE_ANTIALIAS_OFF

    g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, antialiasingState)

    val textAntialiasingState =
      if settings.drawingIsAntiAliased then RenderingHints.VALUE_TEXT_ANTIALIAS_ON
      else RenderingHints.VALUE_TEXT_ANTIALIAS_OFF

    g.setRenderingHint(
      RenderingHints.KEY_TEXT_ANTIALIASING,
      textAntialiasingState
    )

    // Strokes are drawn where their geometry says. Java2D's default, VALUE_STROKE_NORMALIZE,
    // nudges outlines up to half a pixel down and to the right, toward pixel centers. That
    // carries ink out of the boundaries the elements compute, where a buffer sized from the
    // boundary cuts it off, and makes the JVM disagree with the canvas about where a stroke is.
    // Settings that ask for the normalization anyway get it. See RenderSettings.
    val strokeControl =
      if settings.normalizesStrokes then RenderingHints.VALUE_STROKE_NORMALIZE
      else RenderingHints.VALUE_STROKE_PURE
    g.setRenderingHint(RenderingHints.KEY_STROKE_CONTROL, strokeControl)
  end configure

  private def transformationToAWT(transformation: Affine): AffineTransform =
    val awtTransform = new AffineTransform()

    awtTransform.setTransform(
      transformation.a,
      transformation.b,
      transformation.c,
      transformation.d,
      transformation.e,
      transformation.f
    )

    awtTransform

/** The pixels of a bitmap on the JVM, held in a `BufferedImage`, with methods for common image
  * processing tasks such as copying, scaling, and transforming.
  */
private[smile] class JVMBufferAdapter(private val buffer: BufferedImage) extends ImageOperations:

  /** A buffer of `width` by `height` transparent pixels. */
  def this(width: Int, height: Int) =
    this(new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB))

  val width: Int  = buffer.getWidth
  val height: Int = buffer.getHeight

  /* The bordered copy the renderer resamples (see `borderedImage`) is cached against a revision of
   * the pixels, as JSBufferAdapter caches its padded canvas. Every path through which this class
   * changes its pixels bumps the revision. The image escapes through `get` to code that can draw
   * on it unseen, so after that the cache is never trusted again. An image handed to the
   * constructor is taken to be this buffer's from then on.
   */
  private var revision: Long                                                      = 0L
  private var escaped: Boolean                                                    = false
  private var bordered: Option[(Long, BufferedImage)]                             = None
  private var borderedWindow: Option[(Long, (Int, Int, Int, Int), BufferedImage)] = None
  private var faded: Option[(Long, BufferedImage)]                                = None
  private var premultipliedCopy: Option[(Long, BufferedImage)]                    = None

  private def changed(): Unit = revision += 1

  /** A token of this buffer, which a key of `smile.infrastructure.RasterCache` names it by, so that
    * the key does not keep its pixels.
    */
  private val identity: AnyRef = new Object

  /** The key of these pixels, as they are now, scaled by `density` with `interpolation` and
    * `settings`, or `None` once the image has escaped and its pixels may change unseen.
    */
  private[smile] def scaledKey(
      density: Double,
      interpolation: RasterInterpolation,
      settings: RenderSettings
  ): Option[RasterKey] =
    Option.when(!escaped)(RasterKey.Scaled(identity, revision, density, interpolation, settings))

  /** The scaled key these pixels were last asked for under, which a raster is not kept for yet. */
  private var askedOnce: Option[RasterKey] = None

  /** Whether these pixels were asked for under `key` the last time a raster of them was asked for,
    * noting `key` for the next ask. The rule for when their scaled raster is kept is in
    * `JVMPlanRenderer`'s `scaledAtDensity`. A note lost to another thread only keeps the raster one
    * ask later.
    */
  private[smile] def askedBefore(key: RasterKey): Boolean =
    val before = askedOnce.contains(key)
    askedOnce = Some(key)
    before

  /** The underlying `BufferedImage`. Its holder can draw on it unseen, so nothing made from the
    * pixels is cached after this call.
    */
  def get: BufferedImage =
    escaped = true
    buffer

  /** The underlying image, for the renderer to draw from and never to draw on. */
  private[smile] def readOnlyImage: BufferedImage = buffer

  /** The underlying image, to draw on at once. Whatever was kept of its pixels is dropped now and
    * not again, so nothing may draw on it later through this reference.
    */
  private[smile] def imageToDrawOn: BufferedImage =
    changed()
    buffer

  /** The pixels with a border of one pixel around them that repeats their edge pixels (see
    * `smile.infrastructure.JVMBufferAdapter.bordered`), made once for each revision of the pixels.
    * A sprite turned a little every frame is drawn from it without copying its pixels each time.
    */
  private[smile] def borderedImage: BufferedImage =
    bordered match
      case Some((at, image)) if !escaped && at == revision => image
      case _                                               =>
        val image = JVMBufferAdapter.bordered(buffer, 0, 0, width, height)
        if !escaped then bordered = Some((revision, image))
        image

  /** The pixels with a border of one transparent pixel around them (see
    * `smile.infrastructure.JVMBufferAdapter.bordered`), made once for each revision of the pixels.
    * A turned bitmap is resampled from it, so that its edges come out antialiased in one draw.
    */
  private[smile] def fadedImage: BufferedImage =
    faded match
      case Some((at, image)) if !escaped && at == revision => image
      case _                                               =>
        val image = JVMBufferAdapter.bordered(buffer, 0, 0, width, height, transparent = true)
        if !escaped then faded = Some((revision, image))
        image

  /** The pixels as premultiplied ARGB (see `smile.infrastructure.JVMBufferAdapter.premultiplied`),
    * made once for each revision of the pixels. The renderer resamples them from it, which it does
    * faster than from the pixels themselves and to exactly the same result. Once the image has
    * escaped, nothing is kept and the pixels themselves are given, so as not to copy them on every
    * frame.
    */
  private[smile] def premultipliedImage: BufferedImage =
    premultipliedCopy match
      case Some((at, image)) if !escaped && at == revision => image
      case _ if escaped                                    => buffer
      case _                                               =>
        val _     = JVMBufferAdapter.builtPremultiplied.incrementAndGet()
        val image = JVMBufferAdapter.premultiplied(buffer)
        premultipliedCopy = Some((revision, image))
        image

  /** The `width` by `height` pixels from column `left` and row `top` with a border of one pixel
    * that repeats their edge pixels, as `borderedImage` has the whole, kept for the last window
    * asked for. A cropped bitmap drawn every frame asks for the same one.
    */
  private[smile] def borderedImage(left: Int, top: Int, width: Int, height: Int): BufferedImage =
    val window = (left, top, width, height)
    borderedWindow match
      case Some((at, kept, image)) if !escaped && at == revision && kept == window => image
      case _                                                                       =>
        val image = JVMBufferAdapter.bordered(buffer, left, top, width, height)
        if !escaped then borderedWindow = Some((revision, window, image))
        image

  def imageData: Seq[Color] =
    // One bulk raster read, which avoids allocating a 4-element array per pixel.
    val samples =
      buffer.getRaster.getPixels(0, 0, width, height, new Array[Int](width * height * 4))
    (0 until width * height).map: i =>
      val base = i * 4
      Color(samples(base), samples(base + 1), samples(base + 2), samples(base + 3))

  def argbData: Array[Int] = argbData(width, height)

  override def argbData(regionWidth: Int, regionHeight: Int): Array[Int] =
    requireARGBRegion(regionWidth, regionHeight)
    if regionWidth == 0 || regionHeight == 0 then Array.emptyIntArray
    else if buffer.getType == BufferedImage.TYPE_INT_ARGB && !buffer.isAlphaPremultiplied then
      buffer.getRaster
        .getDataElements(0, 0, regionWidth, regionHeight, null)
        .asInstanceOf[Array[Int]]
    else buffer.getRGB(0, 0, regionWidth, regionHeight, null, 0, regionWidth)

  private[smile] def setARGBData(data: Array[Int]): Unit =
    require(data.length == width * height, "The pixel array must have exactly width * height items")
    changed()
    // Native transfer elements are non-premultiplied ARGB only for this format. Other loaded
    // images still need setRGB's color-model conversion. Raster writes keep managed-image
    // tracking, which exposing the DataBufferInt backing array would lose.
    if buffer.getType == BufferedImage.TYPE_INT_ARGB && !buffer.isAlphaPremultiplied then
      buffer.getRaster.setDataElements(0, 0, width, height, data)
    else buffer.setRGB(0, 0, width, height, data, 0, width)

  /** A new `TYPE_INT_ARGB` image of these pixels, which nothing else shares, copied in one pass
    * where this buffer has that format and through packed ARGB values otherwise. The packed values
    * keep the color of fully transparent pixels, which drawing this buffer would drop.
    */
  private[smile] def copiedImage: BufferedImage =
    val image = new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB)
    if buffer.getType == BufferedImage.TYPE_INT_ARGB && !buffer.isAlphaPremultiplied then
      image.getRaster.setDataElements(0, 0, buffer.getRaster)
    else image.setRGB(0, 0, width, height, argbData, 0, width)
    image

  def pixelColor(x: Int, y: Int): Color =
    val argb = buffer.getRGB(x, y)
    new Color(argb)

  private[smile] def setRGBA(x: Int, y: Int, color: Color): Unit =
    changed()
    buffer.setRGB(x, y, color.toARGBInt)

  def scaleTo(
      targetWidth: Double,
      targetHeight: Double,
      settings: RenderSettings
  ): JVMBufferAdapter =
    val method = settings.bufferScalingMethod

    val newWidth  = targetWidth.toInt.abs
    val newHeight = targetHeight.toInt.abs

    val newBuffer = JVMBufferAdapter(newWidth, newHeight)

    val g = newBuffer.buffer.createGraphics()

    if targetWidth < 0 || targetHeight < 0 then
      val tx = AffineTransform.getScaleInstance(
        if targetWidth < 0 then -1 else 1,
        if targetHeight < 0 then -1 else 1
      )
      if targetWidth < 0 then tx.translate(-newWidth, 0)
      if targetHeight < 0 then tx.translate(0, -newHeight)
      g.transform(tx)

    if method == smile.ScalingMethod.AreaAveraging then
      val image = this.buffer.getScaledInstance(newWidth, newHeight, method.value)
      g.drawImage(image, 0, 0, null)
    else
      // Smooth is bilinear interpolation, as on a canvas. AWT's own Image.SCALE_SMOOTH
      // averages areas, which enlarges a bitmap into blocks.
      g.setRenderingHint(
        RenderingHints.KEY_INTERPOLATION,
        if method.copiesNearestPixel then RenderingHints.VALUE_INTERPOLATION_NEAREST_NEIGHBOR
        else RenderingHints.VALUE_INTERPOLATION_BILINEAR
      )
      g.drawImage(this.buffer, 0, 0, newWidth, newHeight, null)
    g.dispose()

    newBuffer
  end scaleTo

  private[smile] def setColorsFromSeq(
      colors: Seq[Color]
  ): Unit =
    require(
      colors.length == width * height,
      "The colors sequence must have exactly width * height items"
    )
    val data = new Array[Int](colors.length)

    var i = 0
    for c <- colors do
      if c != null then data(i) = c.toARGBInt
      i += 1
    setARGBData(data)
  end setColorsFromSeq

  def copyPortionXYWH(
      topLeftX: Double,
      topLeftY: Double,
      width: Double,
      height: Double
  ): JVMBufferAdapter =
    val flooredWidth: Int  = width.floor.toInt
    val flooredHeight: Int = height.floor.toInt
    val flooredX: Int      = topLeftX.floor.toInt
    val flooredY: Int      = topLeftY.floor.toInt

    if flooredWidth < 1 || flooredHeight < 1 then return JVMBufferAdapter.Empty

    // Validate here so both platforms reject out-of-bounds copies with the same error.
    require(
      flooredX >= 0 && flooredY >= 0 &&
        flooredX + flooredWidth <= this.width && flooredY + flooredHeight <= this.height,
      s"Requested portion ($flooredX,$flooredY " +
        s"${flooredWidth}x$flooredHeight) is outside the ${this.width}x${this.height} buffer"
    )

    val sourceBufferArea =
      buffer.getSubimage(
        flooredX,
        flooredY,
        flooredWidth,
        flooredHeight
      )

    // getSubimage shares the parent's pixel storage. Copying the raster keeps the result
    // from retaining the full source image or writing through to it.
    val copiedImage =
      new BufferedImage(flooredWidth, flooredHeight, BufferedImage.TYPE_INT_ARGB)
    sourceBufferArea.copyData(copiedImage.getRaster)

    new JVMBufferAdapter(copiedImage)
  end copyPortionXYWH

  /** Runs `workUnit` on a fresh `Graphics2D` for this buffer, set up from `settings`, and disposes
    * of the graphics afterwards. `Using.resource` does the disposal. When both the work and the
    * disposal throw, the work's error is the one that propagates, with the other added to it as
    * suppressed.
    */
  private[smile] def withGraphics2D[ResultType](settings: RenderSettings)(
      workUnit: Graphics2D => ResultType
  ): ResultType =
    import JVMBufferAdapter.given
    changed()
    Using.resource(buffer.createGraphics()): graphics =>
      JVMBufferAdapter.configure(graphics, settings)
      workUnit(graphics)

  /** A new buffer of these pixels under `transformation`, sized and placed by
    * `smile.infrastructure.ImageOperations.transformedPlacement`.
    *
    * @param settings
    *   Supplies the interpolation method (see [[smile.RenderSettings]]). The area the
    *   transformation leaves uncovered is transparent.
    */
  def transform(
      transformation: Affine,
      settings: RenderSettings
  ): JVMBufferAdapter =
    val (newWidth, newHeight, placed) =
      ImageOperations.transformedPlacement(width, height, transformation)
    val operation = new AffineTransformOp(
      JVMBufferAdapter.transformationToAWT(placed),
      settings.bufferTransformMethod.value
    )
    val resultingBuffer = JVMBufferAdapter(newWidth, newHeight)
    val _               = operation.filter(buffer, resultingBuffer.imageToDrawOn)
    resultingBuffer
  end transform

  /** Saves the pixels as a PNG file at the filesystem path `path`.
    *
    * @return
    *   `true` if the image was saved successfully, `false` otherwise.
    */
  override def saveToPath(path: String): Boolean =
    ImageIO.write(buffer, "png", new File(path))
end JVMBufferAdapter
