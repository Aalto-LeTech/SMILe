package smile.parity

import smile.infrastructure.JSRenderer

import scala.concurrent.ExecutionContext
import scala.scalajs.js
import scala.scalajs.js.JSConverters.*
import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}

/** The catalog as a browser sees it, called by the three-way tier on the JVM after adding this
  * project's bundle to a page. Everything crosses the boundary as strings, with the pixels as PNG
  * `data:` URLs, which is all the JVM needs to decode them.
  */
@JSExportTopLevel("SmileParity")
object ParityExports:

  private given ExecutionContext = ExecutionContext.global

  @JSExport
  def ids(): js.Array[String] = ParityCases.all.map(_.id).toJSArray

  @JSExport
  def hasFont(family: String): Boolean = JsEnvironment.hasFont(family)

  /** Renders one case on both Scala.js backends at `scale` device pixels per logical pixel.
    *
    * @param font
    *   The font the text cases are set in. The JVM passes the one it renders with.
    * @param scale
    *   Device pixels per logical pixel. The canvas backend draws at this density, and the SVG
    *   backend embeds its rasters at it and is rasterized at it by [[smile.parity.SvgRasterizer]].
    * @return
    *   A promise of `{canvasPng, svgText, svgPng}`. They are the canvas backend's bitmap, the SVG
    *   backend's markup, and that markup rasterized by this browser.
    */
  @JSExport
  def render(id: String, font: String, scale: Double): js.Promise[js.Dynamic] =
    val parityCase = ParityCases.byId(id, font)
    val picture    = parityCase.picture()
    val svgText    = SvgMarkup.of(picture, parityCase.settings, scale)

    Preparation
      .ready(parityCase)
      .flatMap(_ => SvgRasterizer.rasterize(svgText, picture, scale))
      .map: rasterized =>
        val canvas = JSRenderer
          .createBitmapFrom(picture, scale, parityCase.settings)
          .nativeBuffer(parityCase.settings)
          .readOnlyCanvas
        js.Dynamic.literal(
          canvasPng = canvas.toDataURL("image/png"),
          svgText = svgText,
          svgPng =
            if rasterized.width > 0 && rasterized.height > 0 then rasterized.toDataURL("image/png")
            else ""
        )
      .toJSPromise
