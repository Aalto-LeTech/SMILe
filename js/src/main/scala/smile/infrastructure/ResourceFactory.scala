package smile.infrastructure

import smile.pictures.AnimatedPicture
import org.scalajs.dom.{Element, window}
/** Provides functionalities for creating and saving images from various sources.
  */
object ResourceFactory:
  window.console.log("ResourceFactory")  
  
  /*
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
   */

   */
  def bufferAdapterFromPath(path: String): BufferAdapter = ???
  /*
    val image = bufferedImageFromPath(path)
    bufferFromImage(image)

  /** Creates an `AnimatedPicture` from a GIF image located at the specified path.
   *
   * @param path
   *   The path to the GIF image, which can be a local filesystem path, a resource path, or an
   *   internet URL.
   * @return
   *   An `AnimatedPicture` instance representing the animated GIF.
   * @throws java.io.IOException
   *   if the image at the specified path cannot be found or loaded or if no suitable GIF image
   *   reader is available.
   */
   */
  def animatedPictureFromPath(path: String): AnimatedPicture = ???
  /*
    val image   = imageInputStreamFromPath(path)
    val readers = ImageIO.getImageReadersByFormatName("gif")
    if !readers.hasNext then throw new IOException("No suitable GIF image reader found")
    val reader = readers.next()
    reader.setInput(image)

    def extractDelay(metadata: IIOMetadata): Int =
      val root = metadata.getAsTree("javax_imageio_gif_image_1.0")
      val graphicsControlExtensionNode = root
        .asInstanceOf[IIOMetadataNode]
        .getElementsByTagName("GraphicControlExtension")
        .item(0)
      val delay = Option(graphicsControlExtensionNode)
        .map(_.getAttributes.getNamedItem("delayTime").getNodeValue.toInt)
        .getOrElse(10) // Default delay
      delay * 10       // Convert to milliseconds

    val frameCount = reader.getNumImages(true)
    val frames = (0 until frameCount).toVector.map: index =>
      val image    = reader.read(index)
      val metadata = reader.getImageMetadata(index)
      val delay    = extractDelay(metadata)
      (
        new Bitmap(
          new BufferAdapter(image),
          Bounds(DefaultPosition, image.getWidth, image.getHeight)
        ),
        delay
      )

    image.close()
    AnimatedPicture(frames.map(_._1), frames.map(_._2))

  private def imageInputStreamFromPath(path: String): ImageInputStream =
    val file     = new File(path)
    val resource = getClass.getClassLoader.getResource(path)
    try
      if file.exists then ImageIO.createImageInputStream(file)
      else if resource != null then ImageIO.createImageInputStream(resource.openStream)
      else ImageIO.createImageInputStream(URI.create(path).toURL.openStream)
    catch
      case _ =>
        throw new IOException(s"Image at path '$path' not found")

  private def bufferedImageFromPath(path: String): BufferedImage =
    val file     = new File(path)
    val resource = getClass.getClassLoader.getResource(path)
    try
      if file.exists then ImageIO.read(file)
      else if resource != null then ImageIO.read(resource)
      else ImageIO.read(URI.create(path).toURL)
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
    val bufferedImage = BufferAdapter(image.getWidth, image.getHeight)
    bufferedImage.get.createGraphics().drawImage(image, 0, 0, null)
    bufferedImage
   */
