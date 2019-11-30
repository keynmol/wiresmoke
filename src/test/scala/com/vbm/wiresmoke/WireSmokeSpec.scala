package com.vbm.wiresmoke

import cats.effect.IO
import org.http4s._
import org.http4s.implicits._
import org.specs2.matcher.MatchResult

class WiresmokeSpec extends org.specs2.Specification {
  override def is = s2"""
    test: $test
  """

  val MyDesireToWriteTestsForThis = 0

  def test: MatchResult[_] = {
    MyDesireToWriteTestsForThis must_== 0
  }
}