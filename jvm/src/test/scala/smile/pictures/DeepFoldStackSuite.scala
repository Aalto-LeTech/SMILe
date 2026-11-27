package smile.pictures

import smile.RenderSettings
import smile.infrastructure.JVMPlanRenderer
import smile.modeling.{Bounds, Pos}

import java.util.concurrent.{ConcurrentLinkedQueue, CountDownLatch}

import scala.concurrent.duration.Duration
import scala.jdk.CollectionConverters.*

/** Every operation on the result of a long fold works on a thread with the stack of Swing's event
  * dispatch thread, 1 MB, where a tree that nests one level per step overflows the stack after
  * about a thousand steps. The folds are those of [[smile.pictures.DeepFoldSuite]].
  */
class DeepFoldStackSuite extends munit.FunSuite:
  import DeepFoldSuite.*

  override val munitTimeout: Duration = Duration(300, "s")

  private val Steps = 10000

  private val settings = RenderSettings.Default

  /** The value of `body` computed on a new thread with a stack of 1 MB, or its exception. */
  private def onSmallStack[A](body: => A): A =
    var result: Either[Throwable, A] = Left(new IllegalStateException("no result"))
    def run(): Unit                  =
      result =
        try Right(body)
        catch case t: (Exception | Error) => Left(t)
    val thread = new Thread(null, () => run(), "deep", 1L << 20)
    thread.start()
    thread.join()
    result.fold(t => throw t, identity)

  /** `e` drawn through the renderer of the JVM at 1 and 1.5 device pixels per logical pixel. */
  private def draw(e: Element): Unit =
    for scale <- Seq(1.0, 1.5) do
      assert(JVMPlanRenderer.render(e, scale, settings, 0L).buffer(settings, 0L).width > 0)

  private def everything(e: => Element): Unit = DeepChecks.everything(e, draw)

  for fold <- Folds do
    test(s"${fold.name}: every operation works after $Steps steps on a 1 MB stack") {
      onSmallStack(everything(fold.flat(Steps)))
    }

  for d <- DeepTrees.nested do
    test(s"${d.name}: every operation after $Steps steps on a 1 MB stack") {
      onSmallStack(everything(d.build(Steps)))
    }

  for d <- DeepTrees.lazyChains do
    test(s"${d.name}: every operation after $Steps steps on a 1 MB stack") {
      onSmallStack {
        // Built once, since bitmaps of pictures made apart are not equal.
        val chain = d.build(Steps)
        everything(chain)
      }
    }

  test(s"a mask of masks has its bounds and its hash after $Steps steps on a 1 MB stack") {
    onSmallStack {
      val chain = (0 until Steps).foldLeft(Rectangle(400, 400, green, None): Element)((acc, _) =>
        MaskGroup(acc, Circle(Pos.Origin, 150, green, None))
      )
      assert(chain.bounds.isDefined)
      assertEquals(chain.hashCode, chain.hashCode)
    }
  }

  for d <- DeepTrees.counterTurned do
    test(
      s"${d.name}, rebuilt as a recolor rebuilds it, measures after $Steps steps on a 1 MB stack"
    ) {
      onSmallStack {
        val rebuilt = DeepTrees.rebuilt(d.build(Steps))
        assert(rebuilt.rotateBy(30).bounds.isDefined)
        assert(rebuilt.bounds.isDefined)
      }
    }

  test("two threads hash and measure one fresh deep tree alike") {
    // A chain of pictures that know no box, so that both the hash and the bounds walk the tree.
    val tree         = onSmallStack(DeepTrees.unmeasured(Steps))
    val start        = new CountDownLatch(1)
    val seen         = new ConcurrentLinkedQueue[(Int, Option[Bounds])]()
    def read(): Unit =
      start.await()
      val _ = seen.add((tree.hashCode, tree.bounds))
    val readers = Seq.fill(2)(Thread.ofPlatform().stackSize(1L << 20).unstarted(() => read()))
    readers.foreach(_.start())
    start.countDown()
    readers.foreach(_.join())
    val pairs = seen.asScala.toSeq
    assertEquals(pairs.size, 2, "a reader failed")
    assertEquals(pairs.head, pairs.last)
  }

  test(s"places in a row onto a sky work after $Steps steps on a 1 MB stack") {
    // A course exercise folds thousands of stars onto a sky one place at a time.
    onSmallStack(everything {
      val sky = blank(400, 300).place(Rectangle(400, 300, Pos.Origin, green, None), 0, 0)
      (0 until Steps).foldLeft(sky)((scene, i) =>
        scene.place(dot(i), (i * 37) % 400 - 200.0, (i * 53) % 300 - 150.0)
      )
    })
  }
end DeepFoldStackSuite
