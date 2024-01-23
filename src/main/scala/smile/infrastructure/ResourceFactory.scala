package smile.infrastructure

import java.awt.image.BufferedImage
import java.io.{File, IOException}
import java.net.URI
import javax.imageio.ImageIO

/** Provides functionalities for creating and saving images from various sources.
  */
object ResourceFactory:
  /** Creates a `BufferAdapter` from an image located at a given path. This method can load images
    * from local file paths, resources within the application's classpath, and URIs, including those
    * on the internet. If the path is a URL to an image on the internet, the method will attempt to
    * download and load the image.
    *
    * @param path
    *   The path to the image. It can be a local filesystem path, a resource path, or a URL to an
    *   image on the internet.
    * @return
    *   A `BufferAdapter` containing the image loaded from the specified path.
    * @throws java.io.IOException
    *   if the image at the specified path cannot be found or loaded.
    */
  def bufferedImageFromPath(path: String): BufferAdapter =
    val file     = new File(path)
    val resource = getClass.getClassLoader.getResource(path)
    try
      val image =
        if file.exists then ImageIO.read(file)
        else if resource != null then ImageIO.read(resource)
        else ImageIO.read(URI.create(path).toURL)

      bufferFromImage(image)
    catch
      case _ =>
        throw new IOException(s"Image at path '$path' not found")

  /** Converts a `BufferedImage` to a `BufferAdapter`. This is a utility method used internally to
    * wrap a `BufferedImage` into the custom `BufferAdapter` class.
    *
    * @param image
    *   The `BufferedImage` to convert.
    * @return
    *   A `BufferAdapter` containing the provided image.
    */
  private def bufferFromImage(image: BufferedImage): BufferAdapter =
    val bufferedImage = new BufferAdapter(image.getWidth, image.getHeight)
    bufferedImage.graphics.drawImage(image, 0, 0, null)
    bufferedImage

  /** Saves a `BufferedImage` to a specified path. The image is saved in PNG format.
    *
    * @param image
    *   The `BufferedImage` to save.
    * @param path
    *   The filesystem path where the image should be saved.
    * @return
    *   `true` if the image was saved successfully, `false` otherwise.
    */
  def saveBufferedImageToPath(image: BufferedImage, path: String): Boolean =
    ImageIO.write(image, "png", new File(path))
