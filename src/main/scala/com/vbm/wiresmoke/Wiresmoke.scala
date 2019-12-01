package com.vbm.wiresmoke

import cats.effect._
import cats.data.Kleisli
import org.http4s.Uri
import org.http4s.EntityEncoder
import org.http4s.client.Client
import cats.effect.concurrent.Ref
import org.http4s.EntityDecoder
import scala.concurrent.ExecutionContext.global

import org.http4s.client.blaze._
import org.http4s.dsl.Http4sDsl
import org.http4s.Response

import cats.syntax.all._
import org.http4s.client.dsl.Http4sClientDsl
import org.http4s.HttpRoutes
import io.circe.{Encoder, Decoder, Json, HCursor}
import io.circe.generic.semiauto._

import org.http4s.circe._
import cats.Applicative
import org.http4s.server.blaze.BlazeServerBuilder
import org.http4s.server.middleware.Logger
import org.http4s.Method
import org.http4s.Request
import org.http4s.Header
import org.http4s.util.CaseInsensitiveString
import org.http4s.server.Server
import org.http4s.HttpApp

object Wiresmoke {

  final case class MockedRequest(uri: Uri, method: Method, body: Option[Array[Byte]]) {
    def matches[F[_]](request: Request[F]): Boolean = {
      request.uri == uri && method == request.method
    }
  }

  final case class MockResponse[F[_]](resp: F[Response[F]])

  trait ServerMocks[F[_]] {
    def whenGet[A](uri: Uri, response: F[Response[F]])(
        implicit e: EntityDecoder[F, A]
    ): F[ServerMocks[F]]

    private[wiresmoke] def mocked: F[List[(MockedRequest, MockResponse[F])]]
  }

  private case class RefServerMocks[F[_]: Concurrent](
      ref: Ref[F, List[(MockedRequest, MockResponse[F])]]
  ) extends ServerMocks[F] {
    override def whenGet[A](uri: Uri, response: F[Response[F]])(
        implicit e: EntityDecoder[F, A]
    ): F[ServerMocks[F]] = {
      val req = MockedRequest(uri, Method.GET, None)

      val pair = (req, MockResponse(response))

      ref.update(_ :+ pair).map(_ => this)
    }

    override def mocked: F[List[(MockedRequest, MockResponse[F])]] = ref.get
  }

  import org.http4s.implicits._

  private def serverMockRoutes[F[_]](
      mocked: List[(MockedRequest, MockResponse[F])]
  )(implicit F: Sync[F]): HttpRoutes[F] = {
    val dsl = new Http4sDsl[F] {}
    import dsl._

    def matchRequest(req: Request[F], original: Method): F[Response[F]] = {
      for {
        originalUri <- req.headers.get(CaseInsensitiveString("X-WireSmoke-Original-URL")) match {
          case Some(uri) =>
            F.pure(uri.value)
          case None =>
            F.raiseError(
              new RuntimeException(
                "This request is trying to leave the grasp of wiresmoke! (done via non-instrumented client that is)"
              )
            )
        }

        originalReq = req.withMethod(Method.GET).withUri(Uri.unsafeFromString(originalUri))

        matched <- mocked.find(_._1.matches(originalReq)) match {
          case None =>
            F.raiseError(new RuntimeException(s"Request $originalReq was not matched"))
          case Some(foundResponse) =>
            foundResponse._2.resp
        }
      } yield matched
    }

    HttpRoutes.of[F] {
      case req @ POST -> Root / "get" =>
        matchRequest(req, Method.GET)

      case req @ POST -> Root / "post" =>
        matchRequest(req, Method.POST)

      case req @ POST -> Root / "patch" =>
        matchRequest(req, Method.PATCH)

      case req @ POST -> Root / "delete" =>
        matchRequest(req, Method.DELETE)

    }

  }

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

  trait PortSelector[F[_]] {
    def select(routes: HttpApp[F]): Resource[F, (Int, Server[F])]
  }

  case class RangePortSelector[F[_]: ConcurrentEffect: Timer](min: Int, max: Int)
      extends PortSelector[F] {
    override def select(routes: HttpApp[F]): Resource[F, (Int, Server[F])] = {
      def rec(pn: Int): Resource[F, (Int, Server[F])] = {
        BlazeServerBuilder[F]
          .bindHttp(pn, "0.0.0.0")
          .withHttpApp(routes)
          .resource
          .map(pn -> _)
          .handleErrorWith {
            case ex =>
              if (pn >= max) {
                Resource.liftF(
                  new RuntimeException("exhausted port range, couldn't bind!")
                    .raiseError[F, (Int, Server[F])]
                )
              } else {
                rec(pn + 1)
              }
          }
      }

      rec(min)
    }
  }

  def setup[F[_]: ConcurrentEffect](
      mock: ServerMocks[F] => F[ServerMocks[F]]
  )(
      implicit T: Timer[F],
      C: ContextShift[F],
      portSelector: PortSelector[F]
  ): Resource[F, Client[F]] = {

    import org.http4s.syntax._

    for {
      r <- Resource.liftF(Ref.of[F, List[(MockedRequest, MockResponse[F])]](List.empty))
      mocker = RefServerMocks(r)
      baseClient <- BlazeClientBuilder[F](global).resource
      mocked     <- Resource.liftF(mock(mocker)).flatMap(s => Resource.liftF(s.mocked))

      httpApp = serverMockRoutes[F](mocked).orNotFound

      (port, server) <- portSelector.select(httpApp)

      newClient = modifyClient(port, "0.0.0.0", baseClient)
    } yield newClient
  }
}
