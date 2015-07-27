package aalto.smcl.images.immutable


import java.awt.Font

import scala.collection.immutable.HashMap

import aalto.smcl.common.Platform




/**
 *
 *
 * @author Aleksi Lukkarinen
 */
object Fonts extends Map[String, Font] with Immutable {

  /** */
  private[this] var _fontMap: Map[String, Font] = new HashMap[String, Font]()


  initializeFontMap()


  /**
   * 
   */
  def initializeFontMap(): Unit = {
    Platform.awtGraphEnv.getAllFonts.foreach {font =>
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
  override def - (key: String): Map[String, Font] = _fontMap.-(key)

  /**
   *
   *
   * @param kv
   * @tparam B1
   * @return
   */
  override def +[B1 >: Font] (kv: (String, B1)): Map[String, B1] = _fontMap + kv

}
