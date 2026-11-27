package smile.infrastructure

import smile.{RenderSettings, ResamplingMode}
import smile.pictures.{Bitmap, Element, Rasters}

object Renderer:
  def apply(): Renderer = PlatformSpecific.renderer

  /** Rejects a scale factor that is not positive and finite, and a `Stepwise` rendering at another
    * factor than 1.
    */
  private[infrastructure] def requireScaleFactor(
      scaleFactor: Double,
      settings: RenderSettings
  ): Unit =
    require(
      scaleFactor > 0 && scaleFactor.isFinite,
      s"Scale factor must be positive and finite (was: $scaleFactor)"
    )
    requireStepwiseAtOneToOne(scaleFactor, settings)

  /** Rejects a `Stepwise` rendering at another scale than one buffer pixel per logical pixel.
    * Stepwise pixels reproduce bitmaps resampled at every operation, which were only ever drawn at
    * that scale.
    */
  private[infrastructure] def requireStepwiseAtOneToOne(
      scaleFactor: Double,
      settings: RenderSettings
  ): Unit =
    require(
      settings.resamplingMode != ResamplingMode.Stepwise || scaleFactor == 1.0,
      s"A Stepwise rendering is at a scale factor of 1 (was: $scaleFactor)"
    )

/** Renders elements into bitmaps through the shared draw plan, which every platform's backend draws
  * the same way. `Renderer()` gives the platform's renderer.
  *
  * `Renderer`, with its JVM extension `smile.infrastructure.renderInto`, and, on Scala.js,
  * `smile.infrastructure.JSRenderer` are the stable entry points of `smile.infrastructure`.
  * Everything else in the package is internal to SMILe.
  */
trait Renderer:

  /** Renders `picture` into a buffer with `scaleFactor` pixels per logical pixel. A factor above 1
    * is for display on a high-DPI screen.
    *
    * The bitmap covers the whole device pixels that the picture's bounds touch, and its bounds are
    * those pixels in logical units, the same at every factor up to the rounding to device pixels,
    * while its buffer is denser by the given factor. Pixel coordinates in `getColor`/`colorAt`
    * therefore index the dense buffer and no longer correspond 1:1 to logical coordinates. Use the
    * default factor whenever that 1:1 invariant matters. The bitmap's anchor is where the picture's
    * is, and the empty picture gives an empty bitmap.
    *
    * @param scaleFactor
    *   Buffer pixels per logical pixel. Must be positive and finite, and 1 when the settings ask
    *   for `ResamplingMode.Stepwise`.
    * @param settings
    *   The settings the rendering is done with, including those of the bitmaps it draws. See
    *   [[smile.RenderSettings]].
    * @param timeMillis
    *   The instant the picture is drawn at, in milliseconds on the scale of
    *   `System.currentTimeMillis()`. Each animation in it shows its frame at that instant, and so
    *   does each animation in a bitmap of a picture that the rendering draws, unless that bitmap
    *   has been rendered already. Equal pictures drawn at the same instant draw the same pixels.
    */
  def createBitmapFrom(
      picture: Element,
      scaleFactor: Double = 1.0,
      settings: RenderSettings = RenderSettings.Default,
      timeMillis: Long = System.currentTimeMillis()
  ): Bitmap =
    Renderer.requireScaleFactor(scaleFactor, settings)
    val pixels = PlatformSpecific.renderPlan(picture, scaleFactor, settings, timeMillis)
    Rasters.anchoredAs(Rasters.of(pixels), picture)
end Renderer
