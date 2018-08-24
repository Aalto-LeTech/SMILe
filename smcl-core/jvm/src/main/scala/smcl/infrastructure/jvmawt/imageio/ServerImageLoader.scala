/* .            .           .                   .                 +             .          +      */
/*         +-----------+  +---+    +  +---+  +-----------+  +---+    Media Programming in Scala   */
/*   *     |           |  |    \     /    |  |           | +|   |            Since 2015           */
/*         |   +-------+  |     \   /     |  |   +-------+  |   |   .                        .    */
/*         |   |          |      \ /      |  |   |          |   |         Aalto University        */
/*       . |   +-------+  |   .   V   .   |  |   |   .      |   |      .   Espoo, Finland       . */
/*  +      |           |  |   |\     /|   |  |   |          |   |                  .    +         */
/*         +------+    |  |   | \   / |   |  |   |          |   |    +        *                   */
/*    *           |    |  |   |  \ /  |   |  |   |      *   |   |                     .      +    */
/*      -- +------+    |  |   |   V  *|   |  |   +-------+  |   +-------+ --    .                 */
/*    ---  |           |  |   | .     |   |  |           |  |           |  ---      +      *      */
/*  ------ +-----------+  +---+       +---+  +-----------+  +-----------+ ------               .  */
/*                                                                                     .          */
/*     T H E   S C A L A   M E D I A   C O M P U T A T I O N   L I B R A R Y      .         +     */
/*                                                                                    *           */

package smcl.infrastructure.jvmawt.imageio


import java.io.{IOException, InputStream}
import java.net.{HttpURLConnection, ProtocolException, SocketTimeoutException, URL, UnknownServiceException}
import java.util.Locale

import scala.util.Try

import javax.imageio.{ImageIO, ImageReader}

import smcl.infrastructure.exceptions._
import smcl.infrastructure.jvmawt.HTTPConnectionProvider
import smcl.infrastructure.{BitmapBufferAdapter, EnsureClosingOfAfter}
import smcl.pictures.BitmapValidator
import smcl.pictures.exceptions.{MaximumBitmapSizeExceededError, MinimumBitmapSizeNotMetError}




/**
 *
 *
 * @param targetURL
 * @param shouldLoadOnlyFirst
 * @param httpConnectionProvider
 * @param imageInputStreamProvider
 * @param bitmapValidator
 * @param supportedReadableMimeTypes
 *
 * @author Aleksi Lukkarinen
 */
private[smcl]
class ServerImageLoader(
    private val targetURL: URL,
    private val shouldLoadOnlyFirst: Boolean,
    private val httpConnectionProvider: HTTPConnectionProvider,
    private val imageInputStreamProvider: ImageInputStreamProvider,
    private val bitmapValidator: BitmapValidator,
    private val supportedReadableMimeTypes: Seq[String],
    private val connectionTimeoutInMilliseconds: Int,
    private val readTimeoutInMilliseconds: Int) {

  require(connectionTimeoutInMilliseconds > 0, "The connection timeout has to be a positive number")
  require(readTimeoutInMilliseconds > 0, "The read timeout has to be a positive number")

  /**
   *
   *
   * @return
   *
   * @throws AccessDeniedByServerError                   for HTTP status codes 401, 402, 403, 407, and 451
   * @throws ImageInputStreamNotCreatedError             if a cache file is needed but could not be created
   * @throws ImageNotFoundError                          for HTTP status codes 204, 205, 404, and 410
   * @throws RedirectionRequestedError                   for HTTP status codes 301, 302, 307, and 308
   * @throws RequestedURITooLongError                    for HTTP status code 414
   * @throws ServerError                                 for all HTTP status codes beginning with 5
   * @throws SuitableImageStreamProviderNotFoundError    if [[ImageIO]] did not find a suitable image stream service provider instance
   * @throws TooManyRequestsToServerError                for HTTP status code 429
   * @throws UnknownHTTPResponseError                    for all HTTP status codes other than 200 that are not reported with other exceptions
   * @throws UnknownMIMETypeError                        if the MIME type sent by the server is not supported
   * @throws UnableToRetrieveDataOverHTTPConnectionError if an I/O error occurs while creating an [[InputStream]] or if the protocol to be used does not support input
   * @throws UnableToOpenHTTPConnectionError             if an [[HttpURLConnection]] instance could not be created; if the HTTP request method cannot be reset; if the request method is not valid; if the connection timeout expires before a connection has been established; or if an I/O error occurs during establishing the connection
   */
  def load: Seq[Try[BitmapBufferAdapter]] = {
    ensureThatMimeTypeIsSupported()
    retrieveImages()
  }

  /**
   *
   *
   * @throws AccessDeniedByServerError       for HTTP status codes 401, 402, 403, 407, and 451
   * @throws ImageNotFoundError              for HTTP status codes 204, 205, 404, and 410
   * @throws RedirectionRequestedError       for HTTP status codes 301, 302, 307, and 308
   * @throws RequestedURITooLongError        for HTTP status code 414
   * @throws ServerError                     for all HTTP status codes beginning with 5
   * @throws TooManyRequestsToServerError    for HTTP status code 429
   * @throws UnableToOpenHTTPConnectionError if an [[HttpURLConnection]] instance could not be created; if the HTTP request method cannot be reset; if the request method is not valid; if the connection timeout expires before a connection has been established; or if an I/O error occurs during establishing the connection
   * @throws UnknownHTTPResponseError        for all HTTP status codes other than 200 that are not reported with other exceptions
   * @throws UnknownMIMETypeError            if the MIME type sent by the server is not supported
   */
  private
  def ensureThatMimeTypeIsSupported(): Unit = {
    val connection = new WrappedHTTPConnection(
      targetURL,
      HTTPMethodHead,
      connectionTimeoutInMilliseconds,
      readTimeoutInMilliseconds)

    connection.useFor{connection =>
      val mimeType = {
        val typeString = connection.getContentType.trim.toLowerCase(Locale.getDefault)
        stripMIMETypeExtensionsFrom(typeString)
      }

      if (!supportedReadableMimeTypes.contains(mimeType))
        throw UnknownMIMETypeError(mimeType, supportedReadableMimeTypes)
    }
  }

  /**
   *
   *
   * @param mimeType
   *
   * @return
   */
  private
  def stripMIMETypeExtensionsFrom(mimeType: String): String = {
    val placeOfSemicolon = mimeType.indexOf(";")
    if (placeOfSemicolon > -1) {
      return mimeType.substring(0, placeOfSemicolon).trim
    }

    mimeType
  }

  /**
   *
   *
   * @return
   *
   * @throws AccessDeniedByServerError                   for HTTP status codes 401, 402, 403, 407, and 451
   * @throws ImageInputStreamNotCreatedError             if a cache file is needed but could not be created
   * @throws ImageNotFoundError                          for HTTP status codes 204, 205, 404, and 410
   * @throws ImageReaderNotRetrievedError                if the first suitable [[ImageReader]] cannot be retrieved
   * @throws MaximumBitmapSizeExceededError              if a bitmap is larger than the maximum allowed bitmap size
   * @throws MinimumBitmapSizeNotMetError                if a bitmap is smaller than the minimum allowed bitmap size
   * @throws RedirectionRequestedError                   for HTTP status codes 301, 302, 307, and 308
   * @throws RequestedURITooLongError                    for HTTP status code 414
   * @throws ServerError                                 for all HTTP status codes beginning with 5
   * @throws SuitableImageReaderNotFoundError            if no suitable [[ImageReader]] is found
   * @throws SuitableImageStreamProviderNotFoundError    if [[ImageIO]] did not find a suitable image stream service provider instance
   * @throws TooManyRequestsToServerError                for HTTP status code 429
   * @throws UnableToOpenHTTPConnectionError             if an [[HttpURLConnection]] instance could not be created; if the HTTP request method cannot be reset; if the request method is not valid; if the connection timeout expires before a connection has been established; or if an I/O error occurs during establishing the connection
   * @throws UnableToRetrieveDataOverHTTPConnectionError if an I/O error occurs while creating an [[InputStream]] or if the protocol to be used does not support input
   * @throws UnknownHTTPResponseError                    for all HTTP status codes other than 200 that are not reported with other exceptions
   */
  private
  def retrieveImages(): Seq[Try[BitmapBufferAdapter]] = {
    val connection = new WrappedHTTPConnection(
      targetURL,
      HTTPMethodGet,
      connectionTimeoutInMilliseconds,
      readTimeoutInMilliseconds)

    connection.openInputStreamFor{stream =>
      EnsureClosingOfAfter(imageInputStreamProvider.createFor(stream)){inputStream =>
        val loader = new ImageStreamLoader(
          inputStream,
          targetURL.toExternalForm,
          shouldLoadOnlyFirst,
          bitmapValidator)

        loader.load
      }
    }
  }




  /**
   * Wraps Java's [[HttpURLConnection]].
   *
   * @param targetURL
   * @param connectionTimeOutInMilliseconds
   * @param readTimeOutInMilliseconds
   *
   * @throws AccessDeniedByServerError       for HTTP status codes 401, 402, 403, 407, and 451
   * @throws IllegalArgumentException        if either of the connection timeouts is negative
   * @throws ImageNotFoundError              for HTTP status codes 204, 205, 404, and 410
   * @throws RedirectionRequestedError       for HTTP status codes 301, 302, 307, and 308
   * @throws RequestedURITooLongError        for HTTP status code 414
   * @throws ServerError                     for all HTTP status codes beginning with 5
   * @throws TooManyRequestsToServerError    for HTTP status code 429
   * @throws UnableToOpenHTTPConnectionError if an [[HttpURLConnection]] instance could not be created; if the HTTP request method cannot be reset; if the request method is not valid; if the connection timeout expires before a connection has been established; or if an I/O error occurs during establishing the connection
   * @throws UnknownHTTPResponseError        for all HTTP status codes other than 200 that are not reported with other exceptions
   */
  private
  class WrappedHTTPConnection(
      targetURL: URL,
      method: HTTPMethod,
      connectionTimeOutInMilliseconds: Int,
      readTimeOutInMilliseconds: Int) {

    private
    val HTTP_TEMPORARY_REDIRECT: Int = 307

    private
    val HTTP_PERMANENT_REDIRECT: Int = 308

    private
    val HTTP_URI_TOO_LONG: Int = 414

    private
    val HTTP_TOO_MANY_REQUESTS: Int = 429

    private
    val HTTP_UNAVAILABLE_FOR_LEGAL_REASONS: Int = 451

    private
    val connection: HttpURLConnection = httpConnectionProvider.createBasedOn(targetURL)

    init()

    /**
     *
     */
    private
    def init(): Unit = {
      createConnection()
      checkHTTPStatusCode()
    }

    /**
     *
     *
     * @throws IllegalArgumentException        if either of the connection timeouts is negative
     * @throws UnableToOpenHTTPConnectionError if an [[HttpURLConnection]] instance could not be created; if the HTTP request method cannot be reset; if the request method is not valid; if the connection timeout expires before a connection has been established; or if an I/O error occurs during establishing the connection
     */
    private
    def createConnection(): Unit = {
      connection.setInstanceFollowRedirects(true)
      connection.setConnectTimeout(connectionTimeOutInMilliseconds)
      connection.setReadTimeout(readTimeOutInMilliseconds)

      try {
        // A SecurityException will be thrown, "if a security manager is set and the method
        // is 'TRACE', but the 'allowHttpTrace' NetPermission is not granted". However, the
        // 'TRACE' method is not used in this class, so in practice, this will not happen.

        connection.setRequestMethod(method.name)
      }
      catch {
        case e: ProtocolException =>
          throw UnableToOpenHTTPConnectionError(targetURL, e)
      }

      try {
        connection.connect()
      }
      catch {
        case e@(_: IOException | _: SocketTimeoutException) =>
          throw UnableToOpenHTTPConnectionError(targetURL, e)
      }
    }

    /**
     *
     *
     * @throws AccessDeniedByServerError    for HTTP status codes 401, 402, 403, 407, and 451
     * @throws ImageNotFoundError           for HTTP status codes 204, 205, 404, and 410
     * @throws RedirectionRequestedError    for HTTP status codes 301, 302, 307, and 308
     * @throws RequestedURITooLongError     for HTTP status code 414
     * @throws ServerError                  for all HTTP status codes beginning with 5
     * @throws TooManyRequestsToServerError for HTTP status code 429
     * @throws UnknownHTTPResponseError     for all HTTP status codes other than 200 that are not reported with other exceptions
     */
    private
    def checkHTTPStatusCode(): Unit = {
      val statusCode: Int = connection.getResponseCode

      if (statusCode.toString.charAt(0) == '5') {
        throw ServerError(targetURL, statusCode)
      }
      else {
        statusCode match {
          case HttpURLConnection.HTTP_OK =>                     // 200 --> Everything OK

          case HttpURLConnection.HTTP_MOVED_PERM                // 301
               | HttpURLConnection.HTTP_MOVED_TEMP              // 302
               | HTTP_TEMPORARY_REDIRECT                        // 307
               | HTTP_PERMANENT_REDIRECT =>                     // 308
            val location = connection.getHeaderField("Location")
            throw RedirectionRequestedError(targetURL, location, statusCode)

          case HttpURLConnection.HTTP_NOT_FOUND                 // 404
               | HttpURLConnection.HTTP_GONE                    // 410
               | HttpURLConnection.HTTP_NO_CONTENT              // 204
               | HttpURLConnection.HTTP_RESET =>                // 205
            throw ImageNotFoundError(targetURL, statusCode)

          case HttpURLConnection.HTTP_UNAUTHORIZED              // 401
               | HttpURLConnection.HTTP_PAYMENT_REQUIRED        // 402
               | HttpURLConnection.HTTP_FORBIDDEN               // 403
               | HttpURLConnection.HTTP_PROXY_AUTH              // 407
               | HTTP_UNAVAILABLE_FOR_LEGAL_REASONS =>          // 451
            throw AccessDeniedByServerError(targetURL, statusCode)

          case HTTP_URI_TOO_LONG =>                             // 414
            throw RequestedURITooLongError(targetURL)

          case HTTP_TOO_MANY_REQUESTS =>                        // 429
            throw TooManyRequestsToServerError(targetURL)

          case _ =>
            throw UnknownHTTPResponseError(targetURL, statusCode)
        }
      }
    }

    /**
     *
     *
     * @param workUnit
     * @tparam A
     *
     * @return
     */
    def useFor[A](workUnit: HttpURLConnection => A): A = {
      try {
        workUnit(connection)
      }
      finally {
        connection.disconnect()
      }
    }

    /**
     *
     *
     * @param workUnit
     * @tparam A
     *
     * @return
     *
     * @throws UnableToRetrieveDataOverHTTPConnectionError if an I/O error occurs while creating an [[InputStream]] or if the protocol to be used does not support input
     */
    def openInputStreamFor[A](workUnit: InputStream => A): A = {
      val inputStream =
        try {
          connection.getInputStream
        }
        catch {
          case e@(_: IOException | _: UnknownServiceException) =>
            throw UnableToRetrieveDataOverHTTPConnectionError(targetURL, e)
        }

      try {
        workUnit(inputStream)
      }
      finally {
        inputStream.close()
      }
    }

  }




}
