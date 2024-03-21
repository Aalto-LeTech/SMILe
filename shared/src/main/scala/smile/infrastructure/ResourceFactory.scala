package smile.infrastructure

import smile.pictures.AnimatedPicture

object ResourceFactory:
  def apply(): ResourceFactory[?] = PlatformSpecific.resourceFactory

/** Provides functionalities for creating and saving images from various sources.
  */
trait ResourceFactory[Buffer]:
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
  def bufferAdapterFromPath(path: String): BufferAdapter[Buffer]

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
  def animatedPictureFromPath(path: String): AnimatedPicture
