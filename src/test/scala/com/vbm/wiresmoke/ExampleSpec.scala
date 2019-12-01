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

class ExampleSpec
    extends org.specs2.Specification
    with Http4sDsl[IO]
    with Http4sClientDsl[IO]
    with CatsIO {
  override def is = s2"""
    demonstrate stateful mocking    $demonstrate_state
  """

  val uri = uri"http://server.com"

  def demonstrate_state = {
    import cats.syntax.apply._
    import cats.syntax.traverse._
    import cats.instances.list._

    val state = Ref.of[IO, Int](0)

    val mocked = Resource.liftF(state).flatMap { ref =>
      val stateLogic = ref.get.flatMap {
        case 5 => Ok("THAT'S A BINGO!") <* ref.set(0)
        case n => Ok(s"State is $n") <* ref.update(_ + 1)
      }

      withMocks {
        _.whenGet(uri, stateLogic)
      }
    }

    mocked
      .map(client => (0 to 5).toList.traverse(n => client.expect[String](GET(uri))))
      .use { results =>
        for {
          hasBingo   <- results.map(_ must contain("THAT'S A BINGO!"))
          hasRegular <- results.map(_ must contain("State is 0"))
        } yield (hasBingo and hasRegular)
      }
  }

  def withMocks(f: ServerMocks[IO] => IO[ServerMocks[IO]]): Resource[IO, Client[IO]] = {
    import Wiresmoke._

    implicit val ps: PortSelector[IO] = RangePortSelector(8080, 8090)

    setup[IO](f)
  }
}
