package aalto.smcl.common

/**
 *
 *
 * @author Aleksi Lukkarinen
 */
private[smcl] object MetaInformationMap {

  // Messages
  private val MSG_KEY_CANNOT_BE_NULL =
    "MetaInformationMap content key cannot be null"

  private val MSG_KEY_CANNOT_CONTAIN_ONLY_WHITESPACE =
    "MetaInformationMap content key cannot contain only white space"

  /**
   * Processes the given content proposal and instantiates
   * a new MetaInformationMap if content is acceptable.
   */
  def apply(contentProposal: Map[String, Option[String]]) = {
    var content = Map[String, Option[String]]()

    contentProposal foreach { pair =>
      var key = pair._1
      require(key != null, MSG_KEY_CANNOT_BE_NULL)

      key = key.trim()
      require(key.nonEmpty, MSG_KEY_CANNOT_CONTAIN_ONLY_WHITESPACE)

      var value = pair._2
      if (value == null)
        value = None

      content = content + (key -> value)
    }

    new MetaInformationMap(content)
  }

}

/**
 *
 *
 * @author Aleksi Lukkarinen
 */
private[smcl] class MetaInformationMap private (private val content: Map[String, Option[String]])
    extends Immutable {

  /**
   *
   */
  def size: Int = content.size

  /**
   *
   */
  def keyValuePairs: Seq[(String, Option[String])] = content.toSeq

  /**
   *
   */
  def get(key: String): Option[String] = content.get(key).get

  /**
   *
   */
  def foreach[T](f: ((String, Option[String])) => T): Unit = content.foreach(f)

}
