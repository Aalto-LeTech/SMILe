package aalto.smcl.common


import scala.collection.immutable.HashMap
import scala.swing.Font

import aalto.smcl.infrastructure.{FontProvider, SMCLInitializationInvoker}




/**
 *
 *
 * @author Aleksi Lukkarinen
 */
class Fonts private[common]()
  extends Map[String, Font]
  with Immutable
  with SMCLInitializationInvoker {

  /** A dummy variable needed to enforce the library initialization. */
  private val __smcl_initialization_ensuring_dummy_variable__ = null



  /** */
  private[this] var _fontMap: Map[String, Font] = new HashMap[String, Font]()


  initializeFontMap()


  /**
   *
   */
  def initializeFontMap(): Unit = {
    new FontProvider().availableFonts() foreach {font =>
      _fontMap = _fontMap + (font.getName -> font)
    }
  }

  /**
   *
   *
   * @param key
   * @return
   */
  override def get(key: String): Option[Font] = _fontMap.get(key)

  /**
   *
   *
   * @return
   */
  override def iterator: Iterator[(String, Font)] = _fontMap.iterator

  /**
   *
   *
   * @param key
   * @return
   */
  override def -(key: String): Map[String, Font] = _fontMap.-(key)

  /**
   *
   *
   * @param kv
   * @tparam B1
   * @return
   */
  override def +[B1 >: Font](kv: (String, B1)): Map[String, B1] = _fontMap + kv

}
