package smile.infrastructure

import org.scalajs.dom
import org.scalajs.dom.html
import smile.{RenderSettings, TransformMethod}
import smile.colors.Color
import smile.modeling.Affine

import scala.scalajs.js
import scala.scalajs.js.typedarray.{Uint8Array, Uint32Array}

private[smile] object JSBufferAdapter:
  /** A buffer of `width` by `height` transparent pixels, which its setters fill. */
  def apply(width: Int, height: Int): JSBufferAdapter =
    new JSBufferAdapter(width, height, Some(new Array[Int](width * height)), None)

  /** A buffer of the `width` by `height` pixels that `draw` draws onto a transparent canvas of that
    * size. Only `draw` sees the canvas's context, and only now, so the pixels never change after
    * this.
    */
  def drawn(width: Int, height: Int)(draw: dom.CanvasRenderingContext2D => Unit): JSBufferAdapter =
    val target = blankCanvas(width, height)
    draw(contextOf(target))
    new JSBufferAdapter(width, height, None, Some(target))

  /** A fresh 1x1 buffer representing "no content", matching the JVM platform's size. See
    * `smile.infrastructure.BufferAdapter.Empty`.
    */
  def Empty: JSBufferAdapter = JSBufferAdapter(1, 1)

  private def blankCanvas(width: Int, height: Int): html.Canvas =
    val result = scalatags.JsDom.tags.canvas().render
    result.width = width
    result.height = height
    result

  /** The context of a canvas of the library's own. It is the browser's ordinary kind, which it may
    * draw with its graphics hardware, wherever that kind draws true, and otherwise the kind whose
    * pixels are read back often, which a browser keeps in ordinary memory and draws on by itself.
    *
    * The ordinary kind is the faster one. A frame of a game scene with 50 sprites at 2000 by 1400
    * pixels took 5.8 ms on it and 12.3 ms on the other in a Chromium with a GPU, 33 and 52 ms in
    * Firefox, and 2.7 and 12.6 ms in Safari. But Safari's does not draw true (see
    * `ordinaryCanvasDrawsTrue`), and it takes a surface of the system for every canvas, of which
    * macOS has a few thousand for all its windows.
    */
  private[infrastructure] def contextOf(canvas: html.Canvas): dom.CanvasRenderingContext2D =
    val context =
      if ordinaryCanvasDrawsTrue then canvas.getContext("2d")
      else canvas.getContext("2d", js.Dynamic.literal(willReadFrequently = true))
    context.asInstanceOf[dom.CanvasRenderingContext2D]

  /** Whether the browser's ordinary canvas draws a turned ellipse within its outline, asked by
    * drawing one. It is 18 by 7, turned 35 degrees and stretched by (2, 0.5), which is 30.56 pixels
    * wide of its center, so that the column 32 pixels from it stays empty.
    *
    * WebKit's ordinary canvas inks that column, in Safari 27 as in Playwright's build of 26.4, and
    * what else it draws there is off as well. A bevel join reaches a pixel past its corner, and
    * there is stray ink around a turned text that is filled with a gradient. Its read-back canvas
    * does none of that, and Chromium's and Firefox's ordinary canvases neither.
    */
  private lazy val ordinaryCanvasDrawsTrue: Boolean =
    val canvas  = blankCanvas(80, 40)
    val context = canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]
    val turn    = math.toRadians(35)
    context.setTransform(
      2 * math.cos(turn),
      -0.5 * math.sin(turn),
      2 * math.sin(turn),
      0.5 * math.cos(turn),
      40,
      20
    )
    context.beginPath()
    context.ellipse(0, 0, 18, 7, 0, 0, 2 * math.Pi)
    context.fill()
    val column = context.getImageData(8, 0, 1, 40).data
    (3 until column.length by 4).forall(column(_) == 0)

  /** Draws the pixels of `source` from column `left` and row `top`, `width` by `height` of them,
    * onto `g` from its pixel `(1, 1)`, with a border of one pixel around them that repeats their
    * edge pixels (see `smile.infrastructure.JSBufferAdapter.paddedCanvas`). The `width + 2` by
    * `height + 2` pixels drawn must be transparent before.
    */
  private[smile] def drawPadded(
      g: dom.CanvasRenderingContext2D,
      source: html.Canvas,
      left: Int,
      top: Int,
      width: Int,
      height: Int
  ): Unit =
    // Each strip is copied at its own size, so nothing is resampled.
    def copy(sx: Int, sy: Int, sw: Int, sh: Int, dx: Int, dy: Int): Unit =
      g.drawImage(source, left + sx, top + sy, sw, sh, dx, dy, sw, sh)
    copy(0, 0, width, height, 1, 1)
    copy(0, 0, width, 1, 1, 0)
    copy(0, height - 1, width, 1, 1, height + 1)
    copy(0, 0, 1, height, 0, 1)
    copy(width - 1, 0, 1, height, width + 1, 1)
    copy(0, 0, 1, 1, 0, 0)
    copy(width - 1, 0, 1, 1, width + 1, 0)
    copy(0, height - 1, 1, 1, 0, height + 1)
    copy(width - 1, height - 1, 1, 1, width + 1, height + 1)

  private[infrastructure] val littleEndian: Boolean =
    val word = new Uint32Array(1)
    word(0) = 0x01020304.toDouble
    new Uint8Array(word.buffer)(0) == 4

/** The pixels of a bitmap in the browser, with methods for common image processing tasks such as
  * copying, scaling, and transforming.
  *
  * A buffer is filled once, in one of two ways, and whichever filled it holds the pixels:
  *   - A buffer made with `smile.infrastructure.JSBufferAdapter.apply` and filled with its setters
  *     holds them as packed ARGB `words`, exactly, translucent ones included. Its `canvas` is made
  *     from them when something first draws it, and kept.
  *   - A buffer made with `smile.infrastructure.JSBufferAdapter.drawn` holds them on the `canvas`
  *     they were drawn on. The first read of its pixels reads the canvas back once and keeps the
  *     words. Translucent pixels read so are the canvas's own, which stores them multiplied by
  *     their alpha. Its copies are drawn from the canvas too, so that they keep its pixels exactly.
  *     Written back to a canvas, the words would come back with their translucent colors rounded
  *     again.
  *
  * Since nothing draws on a buffer after it is made, no canvas is read back twice, and every canvas
  * has the default context, which a browser may keep on the GPU. At least one of `words` and
  * `canvas` is always there.
  */
private[smile] class JSBufferAdapter private (
    val width: Int,
    val height: Int,
    private var words: Option[Array[Int]],
    private var canvas: Option[html.Canvas]
) extends ImageOperations:
  require(words.isDefined || canvas.isDefined, "a buffer holds its pixels in words or on a canvas")

  /** Whether the canvas holds the pixels, which were drawn on it. */
  private var drawn: Boolean = words.isEmpty

  /* The PNG an SVG embeds is cached, because encoding one costs milliseconds and an animation
   * re-serializes the same static bitmap every frame. The setters drop it with the canvas.
   */
  private var encodedPng: Option[String]       = None
  private var padded: Option[html.Canvas]      = None
  private var encodedPaddedPng: Option[String] = None

  /** How many times this buffer has been PNG-encoded. Tests read it to see the cache in use. */
  private[infrastructure] var pngEncodingCount: Int = 0

  /** How many times this buffer's canvas has been read back. Tests read it. */
  private[infrastructure] var readBacks: Int = 0

  /** The canvas, for library code that only reads it, such as drawing it onto another canvas. */
  private[smile] def readOnlyCanvas: html.Canvas =
    canvas.getOrElse:
      val made = newCanvas
      canvas = Some(made)
      made

  /** A new canvas of these pixels, made on every call, which its holder may draw on freely. */
  private[smile] def newCanvas: html.Canvas =
    val made = JSBufferAdapter.blankCanvas(width, height)
    if width > 0 && height > 0 then
      val g = JSBufferAdapter.contextOf(made)
      // Drawn at its own size onto a transparent canvas, a canvas keeps its pixels exactly.
      if drawn then g.drawImage(canvas.get, 0, 0)
      else g.putImageData(imageDataOf(pixelWords, g), 0, 0)
    made

  /** `argb` as image data, its colors not multiplied by their alpha. */
  private def imageDataOf(argb: Array[Int], g: dom.CanvasRenderingContext2D): dom.ImageData =
    val imageData    = g.createImageData(width, height)
    val bytes        = imageData.data
    val target       = new Uint32Array(bytes.buffer, bytes.byteOffset, argb.length)
    val littleEndian = JSBufferAdapter.littleEndian
    var i            = 0
    while i < argb.length do
      target(i) = rgbaWord(argb(i), littleEndian).toDouble
      i += 1
    imageData

  /** The pixels as a PNG data URL, encoded once. */
  private[smile] def pngDataUrl: String =
    encodedPng.getOrElse:
      pngEncodingCount += 1
      val png = readOnlyCanvas.toDataURL("image/png")
      encodedPng = Some(png)
      png

  /** The pixels with a border of one pixel around them that repeats their edge pixels, made once.
    *
    * A canvas that resamples an image samples outside it near its edges, finds transparency there
    * and fades the edges. Drawn with its border and clipped to the pixels' own extent, the image
    * keeps its edge pixels whole, as Java2D and the SVG output draw them.
    */
  private[smile] def paddedCanvas: html.Canvas =
    padded.getOrElse:
      val made = JSBufferAdapter.blankCanvas(width + 2, height + 2)
      JSBufferAdapter.drawPadded(
        JSBufferAdapter.contextOf(made),
        readOnlyCanvas,
        0,
        0,
        width,
        height
      )
      padded = Some(made)
      made

  /** `smile.infrastructure.JSBufferAdapter.paddedCanvas` as a PNG data URL, encoded once and
    * counted with the encodings of `smile.infrastructure.JSBufferAdapter.pngDataUrl`.
    */
  private[smile] def paddedPngDataUrl: String =
    encodedPaddedPng.getOrElse:
      pngEncodingCount += 1
      val png = paddedCanvas.toDataURL("image/png")
      encodedPaddedPng = Some(png)
      png

  /** The pixels as packed ARGB words, read back from the canvas the first time if they were drawn.
    * The array is this buffer's own, so callers copy what they hand out or change.
    */
  private def pixelWords: Array[Int] = words.getOrElse(readBack())

  private def readBack(): Array[Int] =
    val result = new Array[Int](width * height)
    if result.nonEmpty then
      val data = JSBufferAdapter.contextOf(canvas.get).getImageData(0, 0, width, height).data
      var i    = 0
      while i < result.length do
        val base = i * 4
        result(i) =
          (data(base + 3) << 24) | (data(base) << 16) | (data(base + 1) << 8) | data(base + 2)
        i += 1
      readBacks += 1
    words = Some(result)
    result

  /** Makes `argb`, which nothing else holds, these pixels, and drops what was made from the old
    * ones.
    */
  private def filled(argb: Array[Int]): Unit =
    words = Some(argb)
    drawn = false
    canvas = None
    encodedPng = None
    padded = None
    encodedPaddedPng = None

  def imageData: Seq[Color] = pixelWords.toIndexedSeq.map(new Color(_))

  /** Where the pixel `(x, y)` is in the words, which it has to be inside. */
  private def indexOf(x: Int, y: Int): Int =
    require(
      x >= 0 && x < width && y >= 0 && y < height,
      s"Pixel ($x, $y) is outside the ${width}x$height buffer"
    )
    y * width + x

  def pixelColor(x: Int, y: Int): Color = new Color(pixelWords(indexOf(x, y)))

  def argbData: Array[Int] = pixelWords.clone()

  private[smile] def setARGBData(argb: Array[Int]): Unit =
    require(argb.length == width * height, s"Expected ${width * height} pixels, got ${argb.length}")
    filled(argb.clone())

  private[smile] def setRGBA(x: Int, y: Int, color: Color): Unit =
    val argb = argbData
    argb(indexOf(x, y)) = color.toARGBInt
    filled(argb)

  /** A native word whose bytes in memory are R, G, B, A. */
  @inline private[infrastructure] def rgbaWord(argb: Int, littleEndian: Boolean): Int =
    if littleEndian then (argb & 0xff00ff00) | ((argb >>> 16) & 255) | ((argb & 255) << 16)
    else (argb << 8) | (argb >>> 24)

  private[smile] def setColorsFromSeq(colors: Seq[Color]): Unit =
    filled(colors.iterator.map(_.toARGBInt).toArray)

  def scaleTo(
      targetWidth: Double,
      targetHeight: Double,
      settings: RenderSettings
  ): JSBufferAdapter =
    JSBufferAdapter.drawn(targetWidth.toInt.abs, targetHeight.toInt.abs): g =>
      // A canvas has no area averaging, so every method but nearest-pixel copying smooths.
      g.imageSmoothingEnabled = !settings.bufferScalingMethod.copiesNearestPixel
      g.scale(targetWidth / width, targetHeight / height)
      g.drawImage(readOnlyCanvas, 0, 0)

  def copyPortionXYWH(
      topLeftX: Double,
      topLeftY: Double,
      width: Double,
      height: Double
  ): JSBufferAdapter =
    val flooredWidth: Int  = width.floor.toInt
    val flooredHeight: Int = height.floor.toInt
    val flooredX: Int      = topLeftX.floor.toInt
    val flooredY: Int      = topLeftY.floor.toInt

    if flooredWidth < 1 || flooredHeight < 1 then return JSBufferAdapter.Empty

    require(
      flooredX >= 0 && flooredY >= 0 &&
        flooredX + flooredWidth <= this.width && flooredY + flooredHeight <= this.height,
      s"Requested portion ($flooredX,$flooredY " +
        s"${flooredWidth}x$flooredHeight) is outside the ${this.width}x${this.height} buffer"
    )

    if drawn then
      JSBufferAdapter.drawn(flooredWidth, flooredHeight):
        _.drawImage(
          readOnlyCanvas,
          flooredX,
          flooredY,
          flooredWidth,
          flooredHeight,
          0,
          0,
          flooredWidth,
          flooredHeight
        )
    else
      val source  = pixelWords
      val portion = new Array[Int](flooredWidth * flooredHeight)
      var row     = 0
      while row < flooredHeight do
        val from = (flooredY + row) * this.width + flooredX
        Array.copy(source, from, portion, row * flooredWidth, flooredWidth)
        row += 1
      new JSBufferAdapter(flooredWidth, flooredHeight, Some(portion), None)
  end copyPortionXYWH

  /** The pixels moved with wraparound. Those of a drawn buffer are drawn piece by piece, each at
    * its own size, so that they are kept exactly, as
    * `smile.infrastructure.JSBufferAdapter.copyPortionXYWH` keeps them.
    */
  override private[smile] def wrapShifted(dx: Int, dy: Int): JSBufferAdapter =
    if !drawn then super.wrapShifted(dx, dy)
    else
      JSBufferAdapter.drawn(width, height): g =>
        for
          (fromX, toX, pieceWidth)  <- Seq((0, dx, width - dx), (width - dx, 0, dx))
          (fromY, toY, pieceHeight) <- Seq((0, dy, height - dy), (height - dy, 0, dy))
          if pieceWidth > 0 && pieceHeight > 0
        do
          g.drawImage(
            readOnlyCanvas,
            fromX,
            fromY,
            pieceWidth,
            pieceHeight,
            toX,
            toY,
            pieceWidth,
            pieceHeight
          )

  def transform(
      transformation: Affine,
      settings: RenderSettings
  ): JSBufferAdapter =
    val (newWidth, newHeight, placed) =
      ImageOperations.transformedPlacement(width, height, transformation)

    JSBufferAdapter.drawn(newWidth, newHeight): g =>
      // The interpolation follows the settings, as that of AffineTransformOp does on the JVM. A
      // canvas has no bicubic filter. Its high-quality smoothing is the closest it offers.
      settings.bufferTransformMethod match
        case TransformMethod.Nearest  => g.imageSmoothingEnabled = false
        case TransformMethod.Bilinear => g.imageSmoothingEnabled = true
        case TransformMethod.Bicubic  =>
          g.imageSmoothingEnabled = true
          g.asInstanceOf[js.Dynamic].imageSmoothingQuality = "high"

      g.setTransform(placed.a, placed.b, placed.c, placed.d, placed.e, placed.f)
      g.drawImage(readOnlyCanvas, 0, 0)
  end transform
end JSBufferAdapter
