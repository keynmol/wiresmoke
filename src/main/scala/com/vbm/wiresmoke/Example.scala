package com.vbm.wiresmoke

import cats.effect._
import org.http4s.Uri
import org.http4s.client.Client
import scala.concurrent.ExecutionContext.global

import org.http4s.client.blaze._
import org.http4s.dsl.Http4sDsl
import org.http4s.Response

import cats.syntax.all._
import org.http4s.client.dsl.Http4sClientDsl


object Example extends Http4sClientDsl[IO] with Http4sDsl[IO] {
  import Wiresmoke._  

  def main(args: Array[String]): Unit = {
    implicit val t: Timer[IO]         = IO.timer(global)
    implicit val cs: ContextShift[IO] = IO.contextShift(global)

    case class MyService(client: Client[IO])  {
      def call(num: Int): IO[String] = {
        val uri = Uri.unsafeFromString(s"http://oneid.com/hello/${num}")

        client.expect[String](GET(uri))
      }
    }

    val mocking: ServerMocks[IO] => IO[ServerMocks[IO]] = { server =>
      server
        .whenGet[Unit](
          Uri.unsafeFromString("http://oneid.com/hello/1"),
          Ok("hello from the dark side")
        ) *>
        server.whenGet[Unit](
          Uri.unsafeFromString("http://oneid.com/hello/2"),
          Ok("hello from the bright side")
        )
    }

    def myTest(client: Client[IO]): IO[Unit] = {

      val service = MyService(client)

      for {
        c1 <- service.call(1)
        c2 <- service.call(2)
        c3 <- service.call(3).attempt

      } yield {
        println(c2)
        println(c1)
        assert(c1 == "hello from the dark side")
        assert(c2 == "hello from the bright side")
      }
    }

    val clientRes = setup[IO](mocking)

    clientRes.use(myTest).unsafeRunSync()
  }
}
