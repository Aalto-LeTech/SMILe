package smile.pictures

import scala.concurrent.{ExecutionContext, Future}

extension (svg: SvgPicture)

  /** This picture, once its document is decoded for canvas drawing, which starts when the document
    * is parsed. Canvas output draws nothing in its place before that, so await this before reading
    * its pixels. Scala.js only. Every picture of the same document shares the decoding, and SVG
    * output needs none. It fails with an `IOException` if the browser cannot decode the document.
    */
  def ready: Future[SvgPicture] = svg.document.ready.map(_ => svg)(using ExecutionContext.parasitic)
end extension
