package smile.testing

import org.scalajs.dom

/** Rasters as PNG files, for reports that show pictures. */
object RasterPng:

  /** The raster as the bytes of a PNG file, alpha included. Encoded by the browser's canvas. */
  def encode(raster: Raster): Array[Byte] =
    val canvas = dom.document.createElement("canvas").asInstanceOf[dom.HTMLCanvasElement]
    canvas.width = raster.width
    canvas.height = raster.height
    val context = canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]
    val image   = context.createImageData(raster.width, raster.height)
    for index <- raster.argb.indices do
      val argb = raster.argb(index)
      image.data(index * 4) = (argb >> 16) & 0xff
      image.data(index * 4 + 1) = (argb >> 8) & 0xff
      image.data(index * 4 + 2) = argb & 0xff
      image.data(index * 4 + 3) = argb >>> 24
    context.putImageData(image, 0, 0)
    val base64 = canvas.toDataURL("image/png").split(",", 2)(1)
    val binary = dom.window.atob(base64)
    Array.tabulate(binary.length)(index => binary.charAt(index).toByte)
