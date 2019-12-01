package com.vbm.wiresmoke

import org.http4s._
import org.http4s.server._
import org.http4s.server.blaze._
import cats.syntax.all._

import cats.effect._

trait PortSelector[F[_]] {
  def select(routes: HttpApp[F]): Resource[F, (Int, Server[F])]
}

case class RangePortSelector[F[_]: ConcurrentEffect: Timer](min: Int, max: Int)
    extends PortSelector[F] {
  override def select(routes: HttpApp[F]): Resource[F, (Int, Server[F])] = {
    def rec(pn: Int): Resource[F, (Int, Server[F])] = {
      BlazeServerBuilder[F]
        .bindHttp(pn, "0.0.0.0")
        // See: https://github.com/http4s/http4s/issues/2383
        .withNio2(true)
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
