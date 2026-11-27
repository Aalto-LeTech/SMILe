package smile.infrastructure

import smile.pictures.{AnimatedPicture, SvgDocument}

import scala.concurrent.{ExecutionContext, Future}

private[smile] object ResourceFactory:
  def apply(): ResourceFactory = PlatformSpecific.resourceFactory

/** Loads images, animated pictures and SVG documents from paths and URLs.
  */
private[smile] trait ResourceFactory:
  /** Loads the image at `path`, which can be a local file path, a resource on the application's
    * classpath, or a URI, including one on the internet, which it downloads.
    */
  def bufferAdapterFromPath(path: String): NativeBuffer

  /** Loads the GIF image at `path`, which is looked up as an image's path is.
    *
    * @throws java.io.IOException
    *   if the image at the specified path cannot be found or loaded or if no suitable GIF image
    *   reader is available.
    */
  def animatedPictureFromPath(path: String): AnimatedPicture

  /** Reads the SVG document at `path`, which is looked up as an image's path is.
    *
    * @throws java.io.IOException
    *   naming `path`, if nothing is there or it cannot be read as an SVG document
    */
  def svgDocumentFromPath(path: String): SvgDocument

  /** The context the default asynchronous loaders below run on.
    *
    * Each platform decides for itself, because loading is asynchronous in the browser but a
    * thread-bound mixture of IO and decoding on the JVM. It is abstract so that neither platform
    * inherits the other's choice by accident. The choice also cannot live here because the Scala.js
    * javalib ships no `java.util.concurrent.Executors`.
    */
  protected def loadingExecutionContext: ExecutionContext

  /** Loads an image asynchronously on every platform. The JVM runs its synchronous loader on
    * `smile.infrastructure.ResourceFactory.loadingExecutionContext`. Scala.js performs an
    * asynchronous browser load and caches the result so the synchronous API can serve the same path
    * afterwards.
    */
  def loadBufferAdapter(path: String): Future[NativeBuffer] =
    Future(bufferAdapterFromPath(path))(using loadingExecutionContext)

  /** Loads an animated picture asynchronously, as
    * `smile.infrastructure.ResourceFactory.loadBufferAdapter` describes per platform.
    */
  def loadAnimatedPicture(path: String): Future[AnimatedPicture] =
    Future(animatedPictureFromPath(path))(using loadingExecutionContext)

  /** Loads an SVG document asynchronously, as
    * `smile.infrastructure.ResourceFactory.loadBufferAdapter` describes per platform.
    */
  def loadSvgDocument(path: String): Future[SvgDocument] =
    Future(svgDocumentFromPath(path))(using loadingExecutionContext)
