package aalto.smcl.common


/**
 * Adds a timestamp into a public field to mark the approximate time of object's creation.
 *
 * @author Aleksi Lukkarinen
 */
private[smcl] trait TimestampedCreation {

  /** Approximate creation time of this object. */
  val created: TimeStamp = TimeStamp()

}
