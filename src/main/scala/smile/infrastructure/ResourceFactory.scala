package smile.infrastructure

import java.awt.image.BufferedImage
import java.io.File
import java.net.URL
import javax.imageio.ImageIO

object ResourceFactory:
  def bufferedImageFromPath(path: String): BufferAdapter =
//    val image = ImageIO.read(getClass.getResource(path))
    val file     = new File(path)
    val resource = getClass.getClassLoader.getResource(path)
    val image =
      if file.exists then ImageIO.read(file)
      else if resource != null then ImageIO.read(resource)
      else ImageIO.read(new URL(path))

    bufferFromImage(image)

  def bufferedImageFromUrl(url: String): BufferAdapter =
    val image = ImageIO.read(new URL(url))
    bufferFromImage(image)

  private def bufferFromImage(image: BufferedImage): BufferAdapter =
    val bufferedImage = new BufferAdapter(image.getWidth, image.getHeight)
    bufferedImage.graphics.drawImage(image, 0, 0, null)
    bufferedImage

  def saveBufferedImageToPath(image: BufferedImage, path: String): Boolean =
    ImageIO.write(image, "png", new File(path))
