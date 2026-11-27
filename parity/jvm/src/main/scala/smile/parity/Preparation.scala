package smile.parity

import scala.concurrent.{ExecutionContext, Future}

/** What has to happen on this platform before a case's picture can be rendered. */
object Preparation:

  /** Completes once `parityCase` can be rendered here. AWT renders every picture at once. */
  def ready(parityCase: ParityCase)(using @annotation.unused ec: ExecutionContext): Future[Unit] =
    Future.unit
