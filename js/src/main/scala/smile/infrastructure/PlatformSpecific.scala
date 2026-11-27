package smile.infrastructure

import smile.RenderSettings
import smile.modeling.{JSBoundaryCalculator, TextMetrics}
import smile.pictures.{Element, PixelSource}

private[smile] object PlatformSpecific extends PlatformClassesTrait:
  def bufferAdapter(width: Int, height: Int): NativeBuffer =
    JSBufferAdapter(width, height)

  val resourceFactory: ResourceFactory = JSResourceFactory

  val renderer: Renderer = JSRenderer

  // Less than on the JVM, as Safari caps the memory of all canvases of a page near this.
  def rasterCacheBudget: Long = 128L << 20

  // A raster is a canvas here, and a browser gives out only so many. WebKit gives about 50000 at
  // a time, after which drawing on a new one does nothing and reading it fails. Small rasters would
  // fill the budget with several times that, so the cache keeps a third of it at most and leaves
  // the rest to the canvases a program holds itself.
  def rasterCacheLimit: Int = 16384

  def textMetrics(content: String, font: String, size: Double, weight: Int): TextMetrics =
    JSBoundaryCalculator.textMetrics(content, font, size, weight)

  private[smile] def renderPlan(
      element: Element,
      scale: Double,
      settings: RenderSettings,
      timeMillis: Long
  ): PixelSource =
    CanvasPlanRenderer.render(element, scale, settings, timeMillis)

  private[smile] def renderPlan(
      plan: DrawPlan.Plan,
      scale: Double,
      settings: RenderSettings,
      timeMillis: Long
  ): PixelSource =
    CanvasPlanRenderer.render(plan, scale, settings, timeMillis)
