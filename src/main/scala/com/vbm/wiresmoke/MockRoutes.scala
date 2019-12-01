package com.vbm.wiresmoke

import models._

import cats.effect._
import org.http4s._
import org.http4s.dsl._
import org.http4s.util.CaseInsensitiveString
import cats.syntax.all._
import cats.Traverse

object MockRoutes {

  import org.http4s.implicits._

  def serverRoutes[F[_]](
      mocked: List[(MockedRequest[F], MockResponse[F])]
  )(implicit F: Sync[F]): HttpApp[F] = {
    val dsl = new Http4sDsl[F] {}
    import dsl._

    import cats.instances.list._

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

        originalReq = req.withMethod(original).withUri(Uri.unsafeFromString(originalUri))

        requestMatches <- Traverse[List].traverse(mocked) {
          case (req, resp) =>
            req.matches(originalReq).map(_ -> resp)
        }

        matched <- requestMatches.find(_._1) match {
          case None =>
            F.raiseError(
              new RuntimeException(
                s"Request $originalReq was not matched; Mocked requests: $mocked"
              )
            )
          case Some(foundResponse) =>
            foundResponse._2.resp
        }
      } yield matched
    }

    HttpRoutes
      .of[F] {
        case req @ POST -> Root / "get" =>
          matchRequest(req, Method.GET)

        case req @ POST -> Root / "post" =>
          matchRequest(req, Method.POST)

        case req @ POST -> Root / "patch" =>
          matchRequest(req, Method.PATCH)

        case req @ POST -> Root / "delete" =>
          matchRequest(req, Method.DELETE)
        
        case req @ POST -> Root / "put" =>
          matchRequest(req, Method.PUT)
      }
      .orNotFound

  }
}
