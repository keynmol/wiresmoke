package com.vbm.wiresmoke

import cats.effect._
import cats.effect.concurrent.Ref
import models._
import org.http4s.{Header, Method, Uri}
import org.http4s.client.Client
import org.http4s.client.blaze._

import scala.concurrent.ExecutionContext.global

object Wiresmoke {
  private def modifyClient[F[_]: ConcurrentEffect](
      port: Int,
      host: String,
      base: Client[F]
  ): Client[F] = {
    Client.apply { req =>
      import Method._
      val newUri = req.method match {
        case GET    => Uri.unsafeFromString(s"http://$host:$port/get")
        case POST   => Uri.unsafeFromString(s"http://$host:$port/post")
        case PATCH  => Uri.unsafeFromString(s"http://$host:$port/patch")
        case DELETE => Uri.unsafeFromString(s"http://$host:$port/delete")
        case m      => Uri.unsafeFromString(s"http://$host:$port/${m.name.toLowerCase()}")

      }

      val originalUri = req.uri.toString

      val newReq = req
        .withUri(newUri)
        .withMethod(Method.POST)
        .withHeaders(Header.apply("X-WireSmoke-Original-URL", originalUri))

      base.run(newReq)
    }
  }

  def setup[F[_]: ConcurrentEffect](
      mock: ServerMocks[F] => F[ServerMocks[F]]
  )(
      implicit T: Timer[F],
      C: ContextShift[F],
      portSelector: PortSelector[F]
  ): Resource[F, Client[F]] = {

    for {
      r <- Resource.liftF(Ref.of[F, List[(MockedRequest[F], MockResponse[F])]](List.empty))
      mocker = RefServerMocks(r)
      baseClient <- BlazeClientBuilder[F](global).resource
      mocked     <- Resource.liftF(mock(mocker)).flatMap(s => Resource.liftF(s.mocked))

      httpApp = MockRoutes.serverRoutes[F](mocked)

      (port, _) <- portSelector.select(httpApp)

      newClient = modifyClient(port, "0.0.0.0", baseClient)
    } yield newClient
  }
}
