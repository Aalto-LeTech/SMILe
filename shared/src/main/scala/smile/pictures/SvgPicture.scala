package smile.pictures

import smile.infrastructure.ResourceFactory
import smile.modeling.{Affine, Pos}

import scala.concurrent.{ExecutionContext, Future}

/** The size every platform's parsed SVG document has, the box from the origin to `width` and
  * `height` that it is drawn in. Each platform defines `smile.pictures.SvgDocument` against it, so
  * shared code can name the document and the platform draws it.
  */
private[smile] trait SvgDocumentLike:

  /** The document's width in its own pixels, finite and positive. */
  def width: Double

  /** The document's height in its own pixels, finite and positive. */
  def height: Double
end SvgDocumentLike

/** How SVG pictures are made from documents on every platform. */
private[smile] object SvgPictures:

  /** Parses `svg`, an SVG document, into a picture centered at the origin, the same picture on
    * every platform. Only the static picture in the document is kept. Scripts, event attributes,
    * animation elements, metadata, elements and attributes of other namespaces, and references
    * outside the document are removed, and a `switch` draws its first child. CSS animations stay,
    * drawn as at their start in a browser and ignored on the JVM. Nothing outside the document is
    * loaded. PNG, JPEG and GIF rasters embedded in it as `data:` URLs are drawn. The size comes
    * from the root's `width`, `height` and `viewBox`, at 96 pixels per inch, and is 300 by 150
    * pixels when they give none.
    *
    * @throws java.io.IOException
    *   if the text is not well-formed XML with an SVG root, its elements nest more than 30 deep, or
    *   its size is invalid or over 16384 pixels a side
    */
  def fromString(svg: String): SvgPicture = of(SvgDocument.fromString(svg))

  /** Loads the document at `path` asynchronously using the rules of
    * [[smile.pictures.SvgPicture.apply]]. In a browser the first load must be asynchronous and
    * includes decoding for canvas output. Later calls to `apply` can load the same path
    * synchronously. A missing, unreadable or undecodable document fails the future with an
    * `IOException`.
    */
  def load(path: String): Future[SvgPicture] =
    ResourceFactory().loadSvgDocument(path).map(of)(using ExecutionContext.parasitic)

  /** A picture of `document` centered at the origin, with its anchor in the document's middle. */
  def of(document: SvgDocument): SvgPicture =
    val (halfWidth, halfHeight) = (document.width / 2, document.height / 2)
    new SvgPicture(
      document,
      Affine.translation(-halfWidth, -halfHeight),
      Pos(halfWidth, halfHeight)
    )
end SvgPictures
