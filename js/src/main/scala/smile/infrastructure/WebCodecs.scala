package smile.infrastructure

import scala.annotation.unused
import scala.scalajs.js
import scala.scalajs.js.annotation.JSGlobal

/** A minimal facade for the parts of the WebCodecs `ImageDecoder` API that SMILe needs to pull the
  * individual frames out of an animated image.
  *
  * The API is Chromium-only at the time of writing. Firefox, WebKit and jsdom do not implement it,
  * so every entry point here is guarded by `smile.infrastructure.WebCodecs.hasImageDecoder`. See
  * `smile.infrastructure.JSResourceFactory.loadAnimatedPicture` for the fallback.
  *
  * @see
  *   [ImageDecoder on MDN](https://developer.mozilla.org/en-US/docs/Web/API/ImageDecoder)
  */
@js.native
@JSGlobal("ImageDecoder")
private[smile] class ImageDecoder(@unused init: ImageDecoderInit) extends js.Object:

  /** The image's tracks. Nothing may be decoded before `tracks.ready` has resolved. */
  def tracks: ImageTrackList = js.native

  /** Decodes a single frame. The returned promise rejects for an out-of-range frame index. */
  def decode(options: ImageDecodeOptions): js.Promise[ImageDecodeResult] = js.native

  /** Releases the decoder's resources. */
  def close(): Unit = js.native

/** The `{ data, type }` dictionary the `smile.infrastructure.ImageDecoder` constructor takes: the
  * encoded bytes and their MIME type.
  */
@js.native
private[smile] trait ImageDecoderInit extends js.Object:
  def data: js.Any   = js.native
  def `type`: String = js.native

private[smile] object ImageDecoderInit:
  def apply(data: js.Any, mimeType: String): ImageDecoderInit =
    js.Dynamic.literal("data" -> data, "type" -> mimeType).asInstanceOf[ImageDecoderInit]

/** The `{ frameIndex }` dictionary `smile.infrastructure.ImageDecoder.decode` takes. */
@js.native
private[smile] trait ImageDecodeOptions extends js.Object:
  def frameIndex: Int = js.native

private[smile] object ImageDecodeOptions:
  def apply(frameIndex: Int): ImageDecodeOptions =
    js.Dynamic.literal("frameIndex" -> frameIndex).asInstanceOf[ImageDecodeOptions]

@js.native
private[smile] trait ImageTrackList extends js.Object:

  /** Resolves once the track metadata, the frame count in particular, is known. */
  def ready: js.Promise[Unit] = js.native

  /** The track that will be decoded, which for an animated GIF is the animation itself. */
  def selectedTrack: ImageTrack = js.native

@js.native
private[smile] trait ImageTrack extends js.Object:

  /** How many frames the track has. A still image has one. */
  def frameCount: Int = js.native

  def animated: Boolean = js.native

@js.native
private[smile] trait ImageDecodeResult extends js.Object:

  def image: VideoFrame = js.native

  /** Whether the frame is complete, i.e. decoded from fully received bytes. */
  def complete: Boolean = js.native

/** A decoded frame. It can be drawn with `CanvasRenderingContext2D.drawImage` and has to be closed
  * afterwards, because it holds memory the garbage collector does not track.
  */
@js.native
private[smile] trait VideoFrame extends js.Object:

  /** The frame's presentation timestamp, in microseconds. Successive timestamps are what the
    * per-frame delays of an animation are derived from.
    */
  def timestamp: Double = js.native

  /** The frame's display width in pixels. */
  def displayWidth: Int = js.native

  /** The frame's display height in pixels. */
  def displayHeight: Int = js.native

  /** Releases the frame's memory. */
  def close(): Unit = js.native

private[smile] object WebCodecs:

  /** Whether this JS environment provides the WebCodecs `ImageDecoder`. Chromium does, and Firefox,
    * WebKit and jsdom do not. Reading the property off the global object cannot throw the way a
    * bare reference to an undeclared variable would.
    */
  def hasImageDecoder: Boolean =
    js.typeOf(js.Dynamic.global.ImageDecoder) != "undefined"
