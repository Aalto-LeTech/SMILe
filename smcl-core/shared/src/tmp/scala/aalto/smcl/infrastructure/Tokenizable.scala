package aalto.smcl.infrastructure


/**
 * Ensures that a class has a meta information map based on which the class can be tokenized.
 *
 * @author Aleksi Lukkarinen
 */
private[smcl]
trait Tokenizable {

  /** Tokenized representation of this operation, after it is calculated. */
  private lazy val _tokenizedRepresentation: String = new ClassTokenizer().tokenize(this)

  /** Meta information based on which the class is tokenized. */
  def metaInformation: MetaInformationMap

  /**
   * Returns this operation as a formal string, which could be used to assess exercises.
   */
  def toToken: String = _tokenizedRepresentation

}
