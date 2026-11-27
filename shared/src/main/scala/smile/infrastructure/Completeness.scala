package smile.infrastructure

/** Whether a rendering drew everything in it. In the browser, a rendering leaves out an SVG
  * document that the browser is still decoding (see `smile.pictures.SvgDocument`). The code that
  * would keep a rendering keeps it only when nothing was left out while it was computed, so that it
  * is rendered again until the document is decoded. That code is
  * `smile.infrastructure.RasterCache`, a bitmap's memo, the flattening of a bitmap into its pixels,
  * and the checkpoint a recolor takes. On the JVM nothing is ever left out.
  *
  * One count serves every rendering, so a rendering sees what any rendering inside it left out,
  * however deep they nest. Only the browser counts, on its one thread.
  */
private[smile] object Completeness:
  @volatile private var omissions = 0L

  /** Records that the rendering under way left something out. */
  def omitted(): Unit = omissions += 1

  /** What `compute` gives, and whether nothing was left out while it ran. */
  def tracked[A](compute: => A): (A, Boolean) =
    val before = omissions
    val result = compute
    (result, omissions == before)
end Completeness
