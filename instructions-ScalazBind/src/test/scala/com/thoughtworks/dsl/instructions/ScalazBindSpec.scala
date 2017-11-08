package com.thoughtworks.dsl.instructions

import org.scalatest.{FreeSpec, Matchers}

import scalaz.OptionT
import scalaz.std.stream._

/**
  * @author 杨博 (Yang Bo)
  */
class ScalazBindSpec extends FreeSpec with Matchers {
  type AsyncFunction[Domain, +A] = (A => Domain) => Domain

  "Given a continuation that uses Yield and ScalazBind expressions" - {

    def asyncFunction: AsyncFunction[Stream[String], Unit] = _ {
      !Yield("Entering asyncFunction")
      val subThreadId = !ScalazBind(Stream(0, 1))
      !Yield(s"Fork sub-thread $subThreadId")
      !Yield("Leaving asyncFunction")
    }

    "When create a generator that contains Yield, Shift, and ScalazBind expressions" - {

      def generator: Stream[String] = {
        !Yield("Entering generator")
        val threadId = !ScalazBind(Stream(0, 1))
        !Yield(s"Fork thread $threadId")
        !Shift(asyncFunction)
        Stream("Leaving generator")
      }

      "Then the generator should contains yield values" in {
        generator should be(
          Seq(
            /**/ "Entering generator",
            /****/ "Fork thread 0",
            /******/ "Entering asyncFunction",
            /********/ "Fork sub-thread 0",
            /**********/ "Leaving asyncFunction",
            /**********/ "Leaving generator",
            /********/ "Fork sub-thread 1",
            /**********/ "Leaving asyncFunction",
            /**********/ "Leaving generator",
            /****/ "Fork thread 1",
            /******/ "Entering asyncFunction",
            /********/ "Fork sub-thread 0",
            /**********/ "Leaving asyncFunction",
            /**********/ "Leaving generator",
            /********/ "Fork sub-thread 1",
            /**********/ "Leaving asyncFunction",
            /**********/ "Leaving generator"
          ))
      }

    }

  }

  "Given a monadic expression that contains a Scalaz OptionT" - {
    def myOptionalList: OptionT[Stream, String] = {
      // TODO: Is it possible to have `Yield` expressions here?
      val threadId = !ScalazBind(Stream(0, 1, 2))
      val subThreadId = !ScalazBind(OptionT(Stream(Some(10), None, Some(30))))
      val subSubThreadId = !ScalazBind(OptionT(Stream(Some(100), Some(200), None)))
      OptionT[Stream, String](Stream(Some(s"Fork thread $threadId-$subThreadId-$subSubThreadId")))
    }

    "Then it should skips those elements that contains a None" in {
      myOptionalList.run should be(
        Seq(
          Some("Fork thread 0-10-100"),
          Some("Fork thread 0-10-200"),
          None,
          None,
          Some("Fork thread 0-30-100"),
          Some("Fork thread 0-30-200"),
          None,
          Some("Fork thread 1-10-100"),
          Some("Fork thread 1-10-200"),
          None,
          None,
          Some("Fork thread 1-30-100"),
          Some("Fork thread 1-30-200"),
          None,
          Some("Fork thread 2-10-100"),
          Some("Fork thread 2-10-200"),
          None,
          None,
          Some("Fork thread 2-30-100"),
          Some("Fork thread 2-30-200"),
          None
        ))
    }

  }
}
