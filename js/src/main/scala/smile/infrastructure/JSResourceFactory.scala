package smile.infrastructure

import org.scalajs.dom
import org.scalajs.dom.html
import smile.modeling.{Bounds, Pos}
import smile.pictures.{AnimatedPicture, Bitmap, PixelSource, Rasters, SvgDocument}

import scala.collection.mutable
import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.scalajs.js
import scala.scalajs.js.Thenable.Implicits.*
import scala.scalajs.js.typedarray.ArrayBuffer

/** Loads images, animations and SVG documents in the browser.
  *
  * Browser image decoding is asynchronous, so the primary API here is the asynchronous
  * `smile.infrastructure.JSResourceFactory.loadBufferAdapter` and
  * `smile.infrastructure.JSResourceFactory.loadAnimatedPicture`. Loaded images are cached, after
  * which the synchronous API can serve the same paths. Calling the synchronous API for a path that
  * has not been loaded fails with an error that says to load it first.
  */
private[smile] object JSResourceFactory extends ResourceFactory:

  private given ExecutionContext = ExecutionContext.global

  /** Both asynchronous loaders are overridden below, so this never runs a loader and exists only
    * because `smile.infrastructure.ResourceFactory` declares it. Scala.js has a single thread
    * anyway, and its global context is the microtask queue the browser callbacks already resolve
    * onto.
    */
  override protected def loadingExecutionContext: ExecutionContext = ExecutionContext.global

  /** The frame delay in milliseconds used when an animation's own timing cannot be determined, in a
    * single-frame fallback load or for the first frame of a decoded animation.
    */
  private val FallbackFrameDelay = 100

  private val preloadedImages     = mutable.Map.empty[String, JSBufferAdapter]
  private val preloadedAnimations = mutable.Map.empty[String, AnimatedPicture]
  private val preloadedSvgs       = mutable.Map.empty[String, SvgDocument]

  override def loadBufferAdapter(path: String): Future[NativeBuffer] =
    preloadedImages.get(path) match
      case Some(buffer) => Future.successful(buffer)
      case None         =>
        loadImage(path).map: buffer =>
          preloadedImages(path) = buffer
          buffer

  override def loadAnimatedPicture(path: String): Future[AnimatedPicture] =
    preloadedAnimations.get(path) match
      case Some(animation) => Future.successful(animation)
      case None            =>
        // Only Chromium extracts real frames, through WebCodecs' ImageDecoder. Everywhere else
        // there is no portable frame-extraction API, so the image decodes into a single frame.
        val loading =
          if WebCodecs.hasImageDecoder then decodeEveryFrame(path)
          else loadSingleFrameAnimation(path)

        loading.map: animation =>
          preloadedAnimations(path) = animation
          animation

  /** Fetches the SVG document at `path`, parses it and waits until the browser has decoded it, so
    * that canvas output draws it at once, and caches it for the synchronous API.
    */
  override def loadSvgDocument(path: String): Future[SvgDocument] =
    preloadedSvgs.get(path) match
      case Some(document) => Future.successful(document)
      case None           =>
        for
          content  <- fetchSvg(path)
          document <- Future(SvgDocument.fromContent(path, content))
          _        <- document.ready
        yield
          preloadedSvgs(path) = document
          document

  def bufferAdapterFromPath(path: String): JSBufferAdapter =
    preloadedImages.getOrElse(
      path,
      throw new UnsupportedOperationException(
        s"Image '$path' has not been loaded. Browser image loading is asynchronous: await " +
          s"Bitmap.load(\"$path\") first; afterwards the synchronous API can serve the cached image."
      )
    )

  def svgDocumentFromPath(path: String): SvgDocument =
    preloadedSvgs.getOrElse(
      path,
      throw new UnsupportedOperationException(
        s"SVG '$path' has not been loaded. Browser loading is asynchronous: await " +
          s"SvgPicture.load(\"$path\") first; afterwards SvgPicture(\"$path\") serves the " +
          "cached document."
      )
    )

  def animatedPictureFromPath(path: String): AnimatedPicture =
    preloadedAnimations.getOrElse(
      path,
      throw new UnsupportedOperationException(
        s"Animation '$path' has not been loaded. Browser image loading is asynchronous: await " +
          s"AnimatedPicture.load(\"$path\") first; afterwards the synchronous API can serve the " +
          "cached animation."
      )
    )

  /** Decodes every frame of the image at `path` with WebCodecs' `ImageDecoder`, one frame per
    * canvas-backed buffer, and turns the frames' timestamps into per-frame delays.
    */
  private def decodeEveryFrame(path: String): Future[AnimatedPicture] =
    for
      (encoded, mimeType) <- fetchImage(path)
      decoder = new ImageDecoder(ImageDecoderInit(encoded, mimeType))
      _      <- awaited(decoder.tracks.ready)
      frames <- decodeFrames(decoder, decoder.tracks.selectedTrack.frameCount)
    yield
      decoder.close()
      AnimatedPicture.ofFrames(frames.map(_._1), frameDelaysFrom(frames.map(_._2)))

  /** Decodes frames `0` until `frameCount` in order, each one only after the previous one is done,
    * and returns them paired with their timestamps in microseconds.
    */
  private def decodeFrames(
      decoder: ImageDecoder,
      frameCount: Int
  ): Future[Seq[(Bitmap, Double)]] =
    (0 until frameCount).foldLeft(Future.successful(Vector.empty[(Bitmap, Double)])):
      (decodedSoFar, frameIndex) =>
        decodedSoFar.flatMap: decoded =>
          awaited(decoder.decode(ImageDecodeOptions(frameIndex)))
            .map(result => decoded :+ frameFrom(result.image))

  /** Draws one decoded frame onto its own buffer and closes it, because a `VideoFrame` holds memory
    * that is not garbage-collected.
    */
  private def frameFrom(videoFrame: VideoFrame): (Bitmap, Double) =
    val timestamp = videoFrame.timestamp
    // A VideoFrame is a CanvasImageSource. The cast only satisfies drawImage's union type.
    val buffer = JSBufferAdapter.drawn(videoFrame.displayWidth, videoFrame.displayHeight):
      _.drawImage(videoFrame.asInstanceOf[html.Image], 0, 0)
    videoFrame.close()

    val frame =
      Rasters.of(PixelSource.fromBuffer(buffer, Bounds(Pos.Origin, buffer.width, buffer.height)))
    (frame, timestamp)

  /** Turns the frames' presentation timestamps into the per-frame delays an
    * [[smile.pictures.AnimatedPicture]] takes. Each frame is shown until the next one starts. The
    * last frame has no successor, so it repeats the previous delay. A lone frame, or a decoder that
    * does not timestamp its frames, falls back to `FallbackFrameDelay`.
    *
    * @param timestampsInMicroseconds
    *   One presentation timestamp per frame, in microseconds, in display order.
    * @return
    *   One delay per frame, in milliseconds.
    */
  private[smile] def frameDelaysFrom(timestampsInMicroseconds: Seq[Double]): Seq[Int] =
    val delays        = Vector.newBuilder[Int]
    var previousDelay = FallbackFrameDelay

    for index <- timestampsInMicroseconds.indices do
      val delay =
        if index + 1 >= timestampsInMicroseconds.size then previousDelay
        else
          val elapsedMilliseconds =
            (timestampsInMicroseconds(index + 1) - timestampsInMicroseconds(index)) / 1000.0
          if elapsedMilliseconds.isFinite && elapsedMilliseconds > 0 then
            math.round(elapsedMilliseconds).toInt
          else previousDelay

      delays += delay
      previousDelay = delay

    delays.result()

  /** The MIME type `ImageDecoder` is told to expect. The response's content type wins when it names
    * an image type. For a `data:` URL that is the URL's own type. Anything else, such as a missing
    * header, `application/octet-stream` or a text type, falls back to the path's extension.
    *
    * @param declaredType
    *   The `Content-Type` header of the response, or `null` when there was none.
    */
  private[smile] def mimeTypeFrom(declaredType: String, path: String): String =
    val declared = Option(declaredType)
      .map(_.takeWhile(_ != ';').trim.toLowerCase)
      .getOrElse("")
    if declared.startsWith("image/") then declared else mimeTypeForPath(path)

  /** The MIME type guessed from the path's extension. GIF is the default because animations are
    * what this path exists for.
    */
  private[smile] def mimeTypeForPath(path: String): String =
    val withoutQuery = path.takeWhile(character => character != '?' && character != '#')
    val extension    = withoutQuery.reverse.takeWhile(_ != '.').reverse.toLowerCase
    extension match
      case "png"          => "image/png"
      case "jpg" | "jpeg" => "image/jpeg"
      case "webp"         => "image/webp"
      case "avif"         => "image/avif"
      case _              => "image/gif"

  /** Fetches the encoded bytes of the image at `path` and the MIME type to decode them as, failing
    * the same way a failed image load does so that both animation paths report a missing image
    * alike.
    */
  private def fetchImage(path: String): Future[(ArrayBuffer, String)] =
    awaited(dom.fetch(path))
      .flatMap: response =>
        if response.ok then
          awaited(response.arrayBuffer())
            .map(bytes => (bytes, mimeTypeFrom(response.headers.get("content-type"), path)))
        else Future.failed(java.io.IOException(s"Image at path '$path' not found"))
      .recoverWith:
        case error: java.io.IOException => Future.failed(error)
        case _ => Future.failed(java.io.IOException(s"Image at path '$path' could not be read"))

  /** Fetches the text of the SVG document at `path`, failing with an `IOException` naming `path` if
    * the path is empty, nothing is there or it cannot be fetched.
    */
  private def fetchSvg(path: String): Future[String] =
    if path.isBlank then Future.failed(java.io.IOException(s"SVG path '$path' is empty"))
    else
      awaited(dom.fetch(path))
        .flatMap: response =>
          if response.ok then awaited(response.text())
          else
            Future.failed(
              java.io.IOException(s"SVG at path '$path' not found (HTTP ${response.status})")
            )
        .recoverWith:
          case error: java.io.IOException => Future.failed(error)
          case error                      =>
            Future.failed(
              java.io.IOException(s"SVG at path '$path' could not be read: $error", error)
            )

  /** Bridges a JS promise into a Scala future. The implicit conversion would do this unseen, and
    * the explicit call keeps the asynchronous boundaries visible.
    */
  private def awaited[A](promise: js.Promise[A]): Future[A] = promise

  /** The fallback for environments without `ImageDecoder`. The image decodes through
    * `HTMLImageElement`, which yields the first frame of a GIF and nothing else.
    */
  private def loadSingleFrameAnimation(path: String): Future[AnimatedPicture] =
    loadImage(path).map: buffer =>
      val frame =
        Rasters.of(PixelSource.fromBuffer(buffer, Bounds(Pos.Origin, buffer.width, buffer.height)))
      AnimatedPicture.ofFrames(Seq(frame), Seq(FallbackFrameDelay))

  private def loadImage(path: String): Future[JSBufferAdapter] =
    val p   = Promise[JSBufferAdapter]()
    val img = new dom.Image()
    img.addEventListener(
      "load",
      (_: js.Any) => p.success(JSBufferAdapter.drawn(img.width, img.height)(_.drawImage(img, 0, 0)))
    )
    img.addEventListener(
      "error",
      (_: js.Any) => p.failure(java.io.IOException(s"Image at path '$path' not found"))
    )
    img.src = path
    p.future
