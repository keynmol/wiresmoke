package com.vbm.wiresmoke

import cats.effect.IO
import org.http4s._
import org.http4s.implicits._
import org.specs2.matcher.MatchResult
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
import cats.effect.concurrent.Ref
import cats.syntax.all._

class NoBodySpec
    extends org.specs2.Specification
    with Http4sDsl[IO]
    with Http4sClientDsl[IO]
    with CatsIO {
  override def is = s2"""
    $test_GET
    $test_POST
    $test_PATCH
    $test_DELETE
    $test_PUT
  """

  val MyDesireToWriteTestsForThis = 0

  val uri = uri"http://server.com"

  val test_GET    = testMethod(Method.GET)
  val test_POST   = testMethod(Method.POST)
  val test_PATCH  = testMethod(Method.PATCH)
  val test_DELETE = testMethod(Method.DELETE)
  val test_PUT    = testMethod(Method.PUT)

  def testMethod(m: Method) =
    withMocks {
      _.when(m, uri, Ok("hello"))
    }.use { client =>
      client
        .expect[String](Request[IO](method = m, uri = uri))
        .map(_ must_== "hello")
    }

  def withMocks(f: ServerMocks[IO] => IO[ServerMocks[IO]]): Resource[IO, Client[IO]] = {
    import Wiresmoke._

    implicit val ps: PortSelector[IO] = RangePortSelector(8080, 8090)

    setup[IO](f)
  }
}
