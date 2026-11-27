package smile.parity

import smile.pictures.ready

import scala.concurrent.{ExecutionContext, Future}

/** What has to happen on this platform before a case's picture can be rendered. */
object Preparation:

  /** Completes once `parityCase` can be rendered here. A canvas draws an SVG picture only once the
    * browser has decoded its document, so a case that draws one waits for every document in
    * [[smile.parity.ParityCases.svgPictures]], each decoded once for all.
    */
  def ready(parityCase: ParityCase)(using ExecutionContext): Future[Unit] =
    if parityCase.needs(Needs.SvgImages) then
      Future.sequence(ParityCases.svgPictures.map(_.ready)).map(_ => ())
    else Future.unit
