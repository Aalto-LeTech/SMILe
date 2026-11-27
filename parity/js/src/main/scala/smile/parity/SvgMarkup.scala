package smile.parity

import smile.RenderSettings
import smile.infrastructure.SvgPlanRenderer
import smile.pictures.Element

/** The SVG backend's markup of a picture, as the harness compares it. */
object SvgMarkup:

  /** The markup of `picture` with `settings`, its rasters embedded at `scale` pixels per logical
    * pixel. At 1x it is what [[smile.infrastructure.JSRenderer.svgString]] gives.
    */
  def of(picture: Element, settings: RenderSettings, scale: Double): String =
    SvgPlanRenderer.render(picture, settings, scale).outerHTML
