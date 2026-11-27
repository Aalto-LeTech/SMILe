package smile.apidoc

import smile.pictures.*

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*

/** Draws the pictures the doc comments show, each by SMILe itself.
  *
  * A doc comment shows a picture as `![what it shows](pictures/name.png)` right after a code block
  * labelled `scala`, where `name` is a `val` of that block. `picturedSnippets` in build.sbt
  * collects those blocks into `Pictured`, so the picture is SMILe's rendering of exactly the code
  * beside it.
  */
object DocPictures:

  /** Writes every picture as `pictures/<name>.png` beside the source that shows it, under the
    * repository root given as the only argument, and deletes the pictures nothing shows any longer.
    * A picture is drawn at two device pixels per logical pixel, so that it is sharp where a page
    * shows it at its logical size.
    */
  def main(args: Array[String]): Unit =
    val root  = Path.of(args(0))
    val drawn = for (home, name, picture) <- Pictured.all yield
      val file =
        Files.createDirectories(root.resolve(home).resolve("pictures")).resolve(s"$name.png")
      require(picture.toBitmap(scale = 2.0).saveAsPngTo(file.toString), s"Could not write $file")
      file
    for
      base <- Seq("shared/src/main", "jvm/src/main", "apidoc").map(root.resolve)
      file <- Files.walk(base).iterator().asScala.toSeq
      if file.getParent.getFileName.toString == "pictures" && file.toString.endsWith(".png")
      if !drawn.contains(file)
    do Files.delete(file)
