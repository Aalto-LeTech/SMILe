package smile.infrastructure

import smile.RenderSettings

import smile.modeling.{Bounds, Pos}
import smile.pictures.{AnimatedPicture, PixelSource, Rasters, SvgDocument}

import java.awt.image.BufferedImage
import java.io.{ByteArrayInputStream, File, IOException}
import java.net.{HttpURLConnection, URI, URL}
import java.nio.file.{Files, Path}
import java.util.concurrent.Executors
import javax.imageio.ImageIO
import javax.imageio.metadata.{IIOMetadata, IIOMetadataNode}
import javax.imageio.stream.ImageInputStream
import scala.concurrent.ExecutionContext
import scala.jdk.CollectionConverters.IteratorHasAsScala
import scala.util.{Try, Using}
import scala.util.control.NonFatal

/** Loads images, animated GIFs and SVG documents from files, classpath resources and URLs.
  */
private[smile] object JVMResourceFactory extends ResourceFactory:
  /** A small pool of its own for the asynchronous loaders.
    *
    * Loading opens a stream and then decodes it, a whole GIF frame by frame, so it is mostly CPU
    * work that more threads would not shorten. Running it on the global pool under `blocking` would
    * let that pool grow to `availableProcessors + 256` threads, which oversubscribes the CPU and
    * holds that many partly decoded images in memory at once. A fixed pool bounds both, and keeping
    * it off the global pool means a burst of loads cannot starve the compute work a program runs
    * alongside them.
    *
    * The threads are daemons, so a program that loaded an image still exits by itself. The pool is
    * lazy, so a program that never loads one never starts a thread.
    */
  override protected lazy val loadingExecutionContext: ExecutionContext =
    ExecutionContext.fromExecutorService(
      Executors.newFixedThreadPool(
        math.max(2, Runtime.getRuntime.availableProcessors),
        runnable =>
          val thread = new Thread(runnable, "smile-image-loader")
          thread.setDaemon(true)
          thread
      )
    )

  /** The image at `path` as a buffer. The path can be a local file path, a resource within the
    * application's classpath, or a URL, including one on the internet. A URL is downloaded,
    * following HTTP redirects, including one from HTTP to HTTPS.
    *
    * @throws java.io.IOException
    *   if the path is empty, or nothing at it can be found, loaded or decoded as an image.
    */
  def bufferAdapterFromPath(path: String): JVMBufferAdapter =
    val image = bufferedImageFromPath(path)
    bufferFromImage(image)

  /** The animated GIF at `path` as an `AnimatedPicture`. The path can be a local filesystem path, a
    * resource path, or an internet URL.
    *
    * @throws java.io.IOException
    *   if the image at the specified path cannot be found or loaded or if no suitable GIF image
    *   reader is available.
    */
  def animatedPictureFromPath(path: String): AnimatedPicture =
    val image   = imageInputStreamFromPath(path)
    val readers = ImageIO.getImageReadersByFormatName("gif")
    if !readers.hasNext then
      image.close()
      throw new IOException("No suitable GIF image reader found")
    val reader = readers.next()
    try
      reader.setInput(image)

      def extractDelay(metadata: IIOMetadata): Int =
        val root                         = metadata.getAsTree("javax_imageio_gif_image_1.0")
        val graphicsControlExtensionNode = root
          .asInstanceOf[IIOMetadataNode]
          .getElementsByTagName("GraphicControlExtension")
          .item(0)
        val delay = Option(graphicsControlExtensionNode)
          .map(_.getAttributes.getNamedItem("delayTime").getNodeValue.toInt)
          .getOrElse(10)
        delay * 10 // GIF delays are in hundredths of a second.

      val frameCount = reader.getNumImages(true)
      val frames     = (0 until frameCount).toVector.map: index =>
        val image    = reader.read(index)
        val metadata = reader.getImageMetadata(index)
        val delay    = extractDelay(metadata)
        (
          Rasters.of(
            PixelSource.fromBuffer(
              new JVMBufferAdapter(image),
              Bounds(Pos.Origin, image.getWidth, image.getHeight)
            )
          ),
          delay
        )

      AnimatedPicture.ofFrames(frames.map(_._1), frames.map(_._2))
    finally
      try reader.dispose()
      finally image.close()

  /** Reads the SVG document at `path`, a local file, a classpath resource or a URL, looked up as an
    * image is, and parses it as `smile.pictures.SvgDocument.fromString` does.
    *
    * @throws java.io.IOException
    *   naming `path`, if the path is empty, nothing is there or it cannot be read as an SVG
    *   document
    */
  def svgDocumentFromPath(path: String): SvgDocument =
    SvgDocument.fromContent(path, bytesFromPath(path, "SVG"))

  /** Finds the on-disk spelling of a filename that differs only in case, so that a failed load can
    * report which spelling works on case-sensitive filesystems.
    *
    * @param directory
    *   The directory to scan. A missing or unreadable directory yields `None`.
    * @param fileName
    *   The file name that was asked for.
    * @return
    *   The spelling on disk, or `None` when `fileName` exists exactly as given, when nothing
    *   matches case-insensitively, or when the directory cannot be listed.
    */
  private[smile] def suggestCaseInsensitiveMatch(
      directory: Path,
      fileName: String
  ): Option[String] =
    if fileName.isEmpty then None
    else
      try
        Using.resource(Files.list(directory)): entries =>
          val names = entries.iterator.asScala.map(_.getFileName.toString).toVector
          if names.contains(fileName) then None
          else names.find(_.equalsIgnoreCase(fileName))
      catch case NonFatal(_) => None

  /** The error thrown when nothing could be loaded from `path`, including a "did you mean ...?"
    * hint when a local file differs from the requested one only in case. `what` names what was
    * looked for, such as `"Image"`.
    */
  private def notFound(path: String, what: String): IOException =
    val file      = new File(path)
    val directory = Option(file.getParentFile).getOrElse(new File("."))
    // A path that names no file at all, such as one with a NUL character in it, gets no hint.
    val hint = Try(directory.toPath).toOption
      .flatMap(suggestCaseInsensitiveMatch(_, file.getName))
      .map(actualName => s" (did you mean '$actualName'?)")
      .getOrElse("")
    new IOException(s"$what at path '$path' not found$hint")

  /** The error thrown for an empty or blank path, which names nothing at all. Without this check
    * the classpath lookup below would resolve `""` to the classpath root, which is a directory, and
    * the error would only say that something could not be decoded. `what` names what was looked
    * for, such as `"Image"`.
    */
  private def requirePath(path: String, what: String): Unit =
    if path.isBlank then throw new IOException(s"$what path '$path' is empty")

  /** The classpath resource at `path`, the second of the three places a path is looked up. A
    * leading `/` is dropped, since class loaders name their resources without one, so that a path
    * written for a web server, such as `/images/x.png`, also names a resource.
    */
  private def classpathResource(path: String): Option[URL] =
    Option(getClass.getClassLoader.getResource(path.stripPrefix("/")))

  /** Everything at `path`, read from the local file there, or else the classpath resource, or else
    * what the path names as a URL. `what` names what is looked for, such as `"SVG"`.
    *
    * @throws java.io.IOException
    *   naming `path`, if the path is empty or nothing at it can be read
    */
  private def bytesFromPath(path: String, what: String): Array[Byte] =
    requirePath(path, what)
    try
      val file = new File(path)
      if file.exists then Files.readAllBytes(file.toPath)
      else
        classpathResource(path) match
          case Some(url) => Using.resource(url.openStream)(_.readAllBytes)
          case None      => downloadFromPath(path).bytes
    catch case NonFatal(error) => throw notFound(path, what).initCause(error)

  /** A remote image, read in full, as its bytes and the content type the server gave them, if any.
    */
  private final case class Download(bytes: Array[Byte], contentType: Option[String])

  /** The most redirects followed for one image, as many as a browser would rarely need. */
  private val MaxRedirects = 5

  /** How long a connection or a read may stall before loading fails, in milliseconds. A loader
    * thread from the bounded pool would otherwise wait forever on a server that never answers.
    */
  private val NetworkTimeoutMillis = 30000

  /** Sent as the `User-Agent`, since some image hosts refuse requests without one. */
  private val UserAgent = "SMILe"

  /** The statuses that send the client on to a `Location`. */
  private val RedirectStatuses = Set(301, 302, 303, 307, 308)

  /** Reads everything at `url`. HTTP and HTTPS URLs follow redirects, including from HTTP to HTTPS,
    * which `HttpURLConnection` refuses to do by itself. A site that moved to HTTPS would otherwise
    * answer its old links with an empty redirect body that no image decoder can read. A redirect
    * may lead only to another HTTP or HTTPS URL, so a remote server cannot point it at a local
    * file.
    *
    * @throws java.io.IOException
    *   if the URL cannot be read, answers with an error status, or redirects too often.
    */
  private def download(url: URL, redirectsLeft: Int = MaxRedirects): Download =
    url.openConnection() match
      case connection: HttpURLConnection =>
        try
          connection.setInstanceFollowRedirects(false)
          connection.setConnectTimeout(NetworkTimeoutMillis)
          connection.setReadTimeout(NetworkTimeoutMillis)
          connection.setRequestProperty("User-Agent", UserAgent)
          connection.setRequestProperty("Accept", "image/*,*/*;q=0.8")
          if RedirectStatuses.contains(connection.getResponseCode) then
            if redirectsLeft == 0 then throw new IOException(s"Too many redirects from $url")
            val location = Option(connection.getHeaderField("Location")).getOrElse:
              throw new IOException(s"Redirect from $url names no location")
            val next = url.toURI.resolve(location).toURL
            if !isHttp(next) then throw new IOException(s"Redirect from $url leaves HTTP: $next")
            download(next, redirectsLeft - 1)
          else
            val bytes = Using.resource(connection.getInputStream)(_.readAllBytes)
            Download(bytes, Option(connection.getContentType))
        finally connection.disconnect()
      case connection =>
        Download(Using.resource(connection.getInputStream)(_.readAllBytes), None)

  private def isHttp(url: URL): Boolean =
    url.getProtocol == "http" || url.getProtocol == "https"

  /** Downloads the image a path names as a URL, the last of the three places a path is looked up.
    */
  private def downloadFromPath(path: String): Download = download(URI.create(path).toURL)

  private def imageInputStreamFromPath(path: String): ImageInputStream =
    requirePath(path, "Image")
    val file     = new File(path)
    val resource = classpathResource(path)
    val stream   =
      try
        if file.exists then ImageIO.createImageInputStream(file)
        else
          resource match
            case Some(url) => ImageIO.createImageInputStream(url.openStream)
            case None      =>
              ImageIO.createImageInputStream(ByteArrayInputStream(downloadFromPath(path).bytes))
      catch case NonFatal(error) => throw notFound(path, "Image").initCause(error)

    // createImageInputStream returns null when no stream provider fits.
    Option(stream).getOrElse(throw new IOException(s"Image at path '$path' could not be opened"))

  private def bufferedImageFromPath(path: String): BufferedImage =
    requirePath(path, "Image")
    val file     = new File(path)
    val resource = classpathResource(path)

    // ImageIO.read returns null for content it cannot decode, so each branch wraps its result.
    // The remote branch also keeps the content type the server sent, for the error message.
    val (image, servedType) =
      try
        if file.exists then (Option(ImageIO.read(file)), None)
        else
          resource match
            case Some(url) => (Option(ImageIO.read(url)), None)
            case None      =>
              val download = downloadFromPath(path)
              (Option(ImageIO.read(ByteArrayInputStream(download.bytes))), download.contentType)
      catch case NonFatal(error) => throw notFound(path, "Image").initCause(error)

    image.getOrElse:
      val served = servedType.map(contentType => s" (the server sent $contentType)").getOrElse("")
      throw new IOException(s"Image at path '$path' could not be decoded$served")

  private def bufferFromImage(image: BufferedImage): JVMBufferAdapter =
    val bufferedImage = JVMBufferAdapter(image.getWidth, image.getHeight)
    // Drawn through the adapter, so that its image does not escape and the renderer may keep
    // copies of its pixels between frames.
    bufferedImage.withGraphics2D(RenderSettings.Default): graphics =>
      val _ = graphics.drawImage(image, 0, 0, null)
    bufferedImage
