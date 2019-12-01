package com.vbm.wiresmoke
import cats.effect.Sync
import org.http4s._
import cats.syntax.all._

object models {

  final case class MockedRequest[F[_]: Sync](
      uri: Uri,
      method: Method,
      body: F[Option[Vector[Byte]]]
  ) {
    def matches(request: Request[F]): F[Boolean] = {
      for {
        originalBody <- request.body.compile.toList.map(_.toVector)
        bodyMatches <- body.map { res =>
          res match {
            case None    => originalBody.isEmpty
            case Some(b) => originalBody == b
          }
        }
      } yield request.uri == uri && method == request.method && bodyMatches
    }
  }

  final case class MockResponse[F[_]](resp: F[Response[F]])

}
