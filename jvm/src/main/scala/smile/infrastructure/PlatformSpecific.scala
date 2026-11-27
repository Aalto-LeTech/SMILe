package smile.infrastructure

import smile.RenderSettings
import smile.modeling.{JVMBoundaryCalculator, TextMetrics}
import smile.pictures.{Element, PixelSource}

private[smile] object PlatformSpecific extends PlatformClassesTrait:
  def bufferAdapter(width: Int, height: Int): NativeBuffer =
    new JVMBufferAdapter(width, height)

  val resourceFactory: ResourceFactory = JVMResourceFactory

  val renderer: Renderer = JVMRenderer

  def rasterCacheBudget: Long = 256L << 20

  // A raster is an array here, and their bytes are all that counts.
  def rasterCacheLimit: Int = Int.MaxValue

  def textMetrics(content: String, font: String, size: Double, weight: Int): TextMetrics =
    JVMBoundaryCalculator.textMetrics(content, font, size, weight)

  private[smile] def renderPlan(
      element: Element,
      scale: Double,
      settings: RenderSettings,
      timeMillis: Long
  ): PixelSource =
    JVMPlanRenderer.render(element, scale, settings, timeMillis)

  private[smile] def renderPlan(
      plan: DrawPlan.Plan,
      scale: Double,
      settings: RenderSettings,
      timeMillis: Long
  ): PixelSource =
    JVMPlanRenderer.render(plan, scale, settings, timeMillis)
