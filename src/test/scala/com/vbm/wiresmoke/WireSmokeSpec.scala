package com.vbm.wiresmoke

import cats.effect.IO
import org.http4s._
import org.http4s.implicits._
import org.specs2.matcher.MatchResult
import _root_.com.vbm.wiresmoke.Wiresmoke.ServerMocks
import cats.effect.Resource
import org.http4s.client.Client
import cats.effect.implicits._
import cats.effect.Timer
import cats.effect.ContextShift
import scala.concurrent.ExecutionContext.global
import org.http4s.dsl.Http4sDsl
import org.http4s.client.dsl.Http4sClientDsl
import cats.effect.specs2.CatsIO
import org.specs2.execute.Result

class WiresmokeSpec
    extends org.specs2.Specification
    with Http4sDsl[IO]
    with Http4sClientDsl[IO]
    with CatsIO {
  override def is = s2"""
    testOk: $testOk
    testNotFound: $testNotFound
    testServerError: $testServerError

  """

  val MyDesireToWriteTestsForThis = 0

  val uri = uri"http://server.com"

  def testOk = {
    withMocks {
      _.whenGet[String](uri, Ok("hello"))
    }.use { client =>
      client
        .expect[String](GET(uri))
        .map(_ must_== "hello")
    }
  }

  def testNotFound = {
    withMocks {
      _.whenGet[String](uri, NotFound())
    }.use { client =>
      client
        .status(GET(uri))
        .map {
          _.code must_=== 404
        }
    }
  }

  def testServerError = {
    withMocks {
      _.whenGet[String](uri, InternalServerError())
    }.use { client =>
      client
        .status(GET(uri))
        .map {
          _.code must_=== 500
        }
    }
  }

  def withMocks(f: ServerMocks[IO] => IO[ServerMocks[IO]]): Resource[IO, Client[IO]] = {
    import Wiresmoke._

    implicit val ps: PortSelector[IO] = RangePortSelector(8080, 8082)

    setup[IO](f)
  }
}
