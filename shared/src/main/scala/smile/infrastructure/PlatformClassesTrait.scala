package smile.infrastructure

import smile.RenderSettings
import smile.modeling.TextMetrics
import smile.pictures.{Element, PixelSource}

/** What each platform implements to plug its rendering backend into the shared code.
  *
  * There is no runtime dispatch. Each platform source set (`jvm/src/main`, `js/src/main`) defines
  * its own `object PlatformSpecific extends PlatformClassesTrait`, and sbt's crossProject compiles
  * exactly one of them into a given artifact. Shared code refers to `PlatformSpecific` and gets the
  * implementation of whichever platform it was built for.
  */
private[smile] trait PlatformClassesTrait:
  def bufferAdapter(width: Int, height: Int): NativeBuffer

  val resourceFactory: ResourceFactory

  val renderer: Renderer

  /** How many bytes of rasters `smile.infrastructure.RasterCache.shared` keeps. */
  def rasterCacheBudget: Long

  /** How many rasters `smile.infrastructure.RasterCache.shared` keeps at most. */
  def rasterCacheLimit: Int

  /** The layout box and glyph ink of a line of text with this content and font, measured by the
    * platform and remembered by content, font, size and weight.
    */
  def textMetrics(content: String, font: String, size: Double, weight: Int): TextMetrics

  /** `element` rendered through the shared draw plan (see `smile.infrastructure.DrawPlan`) at
    * `scale` device pixels per logical pixel with `settings` at the instant `timeMillis`, as the
    * pixels of the device pixels its bounds cover, laid on those pixels in logical units. The
    * bitmaps of pictures it draws that are not rendered yet are rendered at the same instant.
    */
  private[smile] def renderPlan(
      element: Element,
      scale: Double,
      settings: RenderSettings,
      timeMillis: Long
  ): PixelSource

  /** `plan`, a plan on the grid of `scale` device pixels per logical pixel made at the instant
    * `timeMillis`, rendered as the `Element` overload renders the element it is the plan of.
    */
  private[smile] def renderPlan(
      plan: DrawPlan.Plan,
      scale: Double,
      settings: RenderSettings,
      timeMillis: Long
  ): PixelSource
