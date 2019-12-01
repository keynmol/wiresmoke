package com.vbm.wiresmoke
import org.http4s._
import models._
import cats.effect.Concurrent
import cats.effect.concurrent.Ref
import cats.syntax.all._

trait ServerMocks[F[_]] {
  def whenGet(uri: Uri, response: F[Response[F]]): F[ServerMocks[F]]
  def whenPost(uri: Uri, response: F[Response[F]]): F[ServerMocks[F]]
  def whenPatch(uri: Uri, response: F[Response[F]]): F[ServerMocks[F]]
  def whenDelete(uri: Uri, response: F[Response[F]]): F[ServerMocks[F]]
  def whenPut(uri: Uri, response: F[Response[F]]): F[ServerMocks[F]]

  def when(method: Method, uri: Uri, response: F[Response[F]]): F[ServerMocks[F]]

  def when[A](method: Method, uri: Uri, body: A, response: F[Response[F]])(
      implicit e: EntityEncoder[F, A]
  ): F[ServerMocks[F]]

  def whenGet[A](uri: Uri, body: A, response: F[Response[F]])(
      implicit e: EntityEncoder[F, A]
  ): F[ServerMocks[F]]

  def whenPost[A](uri: Uri, body: A, response: F[Response[F]])(
      implicit e: EntityEncoder[F, A]
  ): F[ServerMocks[F]]

  def whenPatch[A](uri: Uri, body: A, response: F[Response[F]])(
      implicit e: EntityEncoder[F, A]
  ): F[ServerMocks[F]]

  def whenDelete[A](uri: Uri, body: A, response: F[Response[F]])(
      implicit e: EntityEncoder[F, A]
  ): F[ServerMocks[F]]

  def whenPut[A](uri: Uri, body: A, response: F[Response[F]])(
      implicit e: EntityEncoder[F, A]
  ): F[ServerMocks[F]]

  private[wiresmoke] def mocked: F[List[(MockedRequest[F], MockResponse[F])]]
}

private[wiresmoke] case class RefServerMocks[F[_]: Concurrent](
    ref: Ref[F, List[(MockedRequest[F], MockResponse[F])]]
) extends ServerMocks[F] {

  override def whenGet(uri: Uri, response: F[Response[F]]): F[ServerMocks[F]] =
    stubRequest[Unit](uri, Method.GET, None, response)

  override def whenPost(uri: Uri, response: F[Response[F]]): F[ServerMocks[F]] =
    stubRequest[Unit](uri, Method.POST, None, response)

  override def whenPatch(uri: Uri, response: F[Response[F]]): F[ServerMocks[F]] =
    stubRequest[Unit](uri, Method.PATCH, None, response)

  override def whenDelete(uri: Uri, response: F[Response[F]]): F[ServerMocks[F]] =
    stubRequest[Unit](uri, Method.DELETE, None, response)

  override def whenPut(uri: Uri, response: F[Response[F]]): F[ServerMocks[F]] =
    stubRequest[Unit](uri, Method.PUT, None, response)

  def whenGet[A](uri: Uri, body: A, response: F[Response[F]])(
      implicit e: EntityEncoder[F, A]
  ): F[ServerMocks[F]] =
    stubRequest[A](uri, Method.GET, Some(body), response)

  def whenPost[A](uri: Uri, body: A, response: F[Response[F]])(
      implicit e: EntityEncoder[F, A]
  ): F[ServerMocks[F]] =
    stubRequest[A](uri, Method.POST, Some(body), response)

  def whenPatch[A](uri: Uri, body: A, response: F[Response[F]])(
      implicit e: EntityEncoder[F, A]
  ): F[ServerMocks[F]] =
    stubRequest[A](uri, Method.PATCH, Some(body), response)

  def whenDelete[A](uri: Uri, body: A, response: F[Response[F]])(
      implicit e: EntityEncoder[F, A]
  ): F[ServerMocks[F]] =
    stubRequest[A](uri, Method.DELETE, Some(body), response)

  def whenPut[A](uri: Uri, body: A, response: F[Response[F]])(
      implicit e: EntityEncoder[F, A]
  ): F[ServerMocks[F]] =
    stubRequest[A](uri, Method.PUT, Some(body), response)

  def when(method: Method, uri: Uri, response: F[Response[F]]) =
    stubRequest[Unit](uri, method, None, response)

  def when[A](method: Method, uri: Uri, body: A, response: F[Response[F]])(
      implicit e: EntityEncoder[F, A]
  ) =
    stubRequest[A](uri, method, Some(body), response)

  private def stubRequest[A](uri: Uri, method: Method, body: Option[A], response: F[Response[F]])(
      implicit e: EntityEncoder[F, A]
  ): F[ServerMocks[F]] = {
    val req = MockedRequest[F](
      uri,
      method,
      body match {
        case Some(b) => e.toEntity(b).body.compile.toList.map(a => Some(a.toVector))
        case None    => Option.empty[Vector[Byte]].pure[F]
      }
    )

    ref.update(_ :+ (req, MockResponse(response))).map(_ => this)
  }

  override def mocked: F[List[(MockedRequest[F], MockResponse[F])]] = ref.get
}
