package smile.testing

import java.awt.image.BufferedImage
import java.io.ByteArrayOutputStream
import javax.imageio.ImageIO

/** Rasters as PNG files, for reports that show pictures. */
object RasterPng:

  /** The raster as the bytes of a PNG file, alpha included. */
  def encode(raster: Raster): Array[Byte] =
    val image = BufferedImage(raster.width, raster.height, BufferedImage.TYPE_INT_ARGB)
    image.setRGB(0, 0, raster.width, raster.height, raster.argb, 0, raster.width)
    val out = ByteArrayOutputStream()
    val _   = ImageIO.write(image, "png", out)
    out.toByteArray
