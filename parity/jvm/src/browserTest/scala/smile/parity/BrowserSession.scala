package smile.parity

import com.microsoft.playwright.{Browser, BrowserType, Page, Playwright}

import java.awt.image.BufferedImage
import java.io.ByteArrayInputStream
import java.nio.file.Path
import java.util.Base64
import javax.imageio.ImageIO
import scala.jdk.CollectionConverters.*
import smile.testing.*

/** What a browser made of one case, which is the canvas backend's bitmap, the SVG backend's markup,
  * and that markup rasterized by the browser itself.
  */
final case class BrowserRender(canvas: Raster, svg: Raster, svgText: String)

/** A headless browser with the catalog's Scala.js bundle loaded into a blank page, driven through
  * Playwright. The bundle's `ParityExports` exposes the catalog as the global `SmileParity`.
  *
  * Nothing is captured from a screen. The page hands its canvases over as PNG `data:` URLs, so the
  * device pixel ratio has no part in the result.
  *
  * @param bundle
  *   The linked bundle, a classic script.
  */
final class BrowserSession(bundle: Path) extends AutoCloseable:
  require(bundle.toFile.isFile, s"The parity bundle has not been linked: $bundle")

  private val playwright: Playwright = Playwright.create()

  private val browser: Browser =
    try playwright.chromium().launch(new BrowserType.LaunchOptions().setHeadless(true))
    catch
      case error: Throwable =>
        playwright.close()
        throw error

  private val page: Page =
    val context = browser.newContext(new Browser.NewContextOptions().setDeviceScaleFactor(1))
    val page    = context.newPage()
    page.setContent("<!doctype html><html><body></body></html>")
    val _ = page.addScriptTag(new Page.AddScriptTagOptions().setPath(bundle))
    page

  val browserVersion: String = s"Chromium ${browser.version()}"

  def ids: Seq[String] =
    page.evaluate("() => SmileParity.ids()").asInstanceOf[java.util.List[String]].asScala.toSeq

  def hasFont(family: String): Boolean =
    page.evaluate("family => SmileParity.hasFont(family)", family).asInstanceOf[Boolean]

  /** Renders the case `id`, its text set in `font`, at `scale` device pixels per logical pixel. */
  def render(id: String, font: String, scale: Double): BrowserRender =
    val result = page
      .evaluate(
        "([id, font, scale]) => SmileParity.render(id, font, scale)",
        java.util.List.of(id, font, java.lang.Double.valueOf(scale))
      )
      .asInstanceOf[java.util.Map[String, String]]
    BrowserRender(
      BrowserSession.rasterOf(result.get("canvasPng")),
      BrowserSession.rasterOf(result.get("svgPng")),
      result.get("svgText")
    )

  override def close(): Unit =
    try browser.close()
    finally playwright.close()

object BrowserSession:

  /** Decodes a PNG `data:` URL. An empty string stands for a picture without pixels. */
  def rasterOf(pngDataUrl: String): Raster =
    if pngDataUrl.isEmpty then Raster(0, 0, Array.empty)
    else
      val bytes = Base64.getDecoder.decode(pngDataUrl.substring(pngDataUrl.indexOf(',') + 1))
      val image = ImageIO.read(new ByteArrayInputStream(bytes))
      Raster(
        image.getWidth,
        image.getHeight,
        image.getRGB(0, 0, image.getWidth, image.getHeight, null, 0, image.getWidth)
      )

  def imageOf(raster: Raster): BufferedImage =
    val image =
      new BufferedImage(raster.width.max(1), raster.height.max(1), BufferedImage.TYPE_INT_ARGB)
    if raster.width > 0 && raster.height > 0 then
      image.setRGB(0, 0, raster.width, raster.height, raster.argb, 0, raster.width)
    image
