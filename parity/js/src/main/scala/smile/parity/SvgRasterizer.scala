package smile.parity

import org.scalajs.dom
import org.scalajs.dom.html

import java.nio.charset.StandardCharsets
import scala.concurrent.{Future, Promise}
import scala.scalajs.js
import smile.infrastructure.DeviceGrid
import smile.pictures.Element
import smile.testing.*

/** Turns the SVG backend's output into pixels the way a page showing it would, by loading the
  * markup as an image and drawing it onto a canvas of the image's own size.
  *
  * Both Scala.js test environments can do this, a browser and jsdom with its canvas package.
  * Neither depends on the device pixel ratio, because nothing is captured from a screen. The size
  * is the one the SVG itself declares, so a rendering whose `width` and `height` disagree with the
  * bitmap backends' shows up as a difference in dimensions.
  *
  * An SVG drawn as an image sees system fonts only, and no stylesheet or font the page has loaded.
  */
object SvgRasterizer:

  /** How long an image may take to load before the rasterization is given up, in milliseconds. */
  private val LoadTimeout = 10000

  /** The markup as a Base64 `data:` URL. The bytes are UTF-8, which `btoa` has to be handed one
    * character per byte.
    */
  def dataUrl(svg: String): String =
    val bytes = svg.getBytes(StandardCharsets.UTF_8).map(byte => (byte & 0xff).toChar).mkString
    "data:image/svg+xml;base64," + dom.window.btoa(bytes)

  /** Rasterizes the SVG backend's markup of `picture` at `scale` device pixels per logical pixel,
    * onto the device pixels a bitmap backend's rendering at that scale covers.
    *
    * The markup's `viewBox` is the whole logical pixels the picture's bounds cover, and a bitmap
    * rendering at `scale` covers the whole device pixels they touch, which can start part of a
    * logical pixel further in. So the markup is shown through the `viewBox` of those device pixels
    * in logical units, at their size, and rasterized at that intrinsic size. Changing the size an
    * SVG declares makes every rasterizer draw its vectors at the new size.
    */
  def rasterize(svg: String, picture: Element, scale: Double): Future[html.Canvas] =
    picture.bounds match
      case Some(bounds) if scale != 1.0 =>
        val device   = DeviceGrid(scale).covering(bounds)
        val document = new dom.DOMParser().parseFromString(svg, dom.MIMEType.`image/svg+xml`)
        val root     = document.documentElement
        root.setAttribute("width", device.width.toString)
        root.setAttribute("height", device.height.toString)
        root.setAttribute(
          "viewBox",
          Seq(device.left, device.top, device.width, device.height)
            .map(v => (v / scale).toString)
            .mkString(" ")
        )
        rasterize(new dom.XMLSerializer().serializeToString(root))
      case _ => rasterize(svg)

  /** Rasterizes SVG markup at its intrinsic size.
    *
    * @return
    *   The canvas the image was drawn onto. [[smile.parity.SvgRasterizer.rasterOf]] reads its
    *   pixels.
    */
  def rasterize(svg: String): Future[html.Canvas] =
    val loaded = Promise[html.Canvas]()
    val image  = dom.document.createElement("img").asInstanceOf[html.Image]

    image.addEventListener(
      "load",
      (_: js.Any) =>
        val _ = loaded.trySuccess:
          val canvas = dom.document.createElement("canvas").asInstanceOf[html.Canvas]
          canvas.width = image.naturalWidth
          canvas.height = image.naturalHeight
          if canvas.width > 0 && canvas.height > 0 then
            contextOf(canvas).drawImage(image, 0, 0, canvas.width, canvas.height)
          canvas
    )
    image.addEventListener(
      "error",
      (_: js.Any) =>
        val _ = loaded.tryFailure(new RuntimeException(s"the SVG did not load as an image: $svg"))
    )
    val _ = dom.window.setTimeout(
      () =>
        val _ = loaded.tryFailure(new RuntimeException(s"the SVG took too long to load: $svg"))
      ,
      LoadTimeout
    )
    image.src = dataUrl(svg)
    loaded.future
  end rasterize

  /** The canvas's context, asked for the way the library's own buffers ask for theirs. The option
    * matters because a browser draws a canvas that is read back often in software and any other on
    * the GPU, and the two antialias differently. With it, the SVG is rasterized by the same code
    * that draws the canvas backend's pictures.
    */
  private def contextOf(canvas: html.Canvas): dom.CanvasRenderingContext2D =
    canvas
      .getContext("2d", js.Dictionary("willReadFrequently" -> true))
      .asInstanceOf[dom.CanvasRenderingContext2D]

  def rasterOf(canvas: html.Canvas): Raster =
    if canvas.width < 1 || canvas.height < 1 then Raster(0, 0, Array.empty)
    else
      val data   = contextOf(canvas).getImageData(0, 0, canvas.width, canvas.height).data
      val pixels = new Array[Int](canvas.width * canvas.height)
      var index  = 0
      while index < pixels.length do
        val base = index * 4
        pixels(index) =
          (data(base + 3) << 24) | (data(base) << 16) | (data(base + 1) << 8) | data(base + 2)
        index += 1
      Raster(canvas.width, canvas.height, pixels)
