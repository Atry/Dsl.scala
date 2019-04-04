package com.thoughtworks.dsl.keywords

import com.thoughtworks.dsl.Dsl
import com.thoughtworks.dsl.Dsl.{!!, Keyword}
import com.thoughtworks.dsl.keywords.Catch.CatchDsl

import scala.concurrent.{ExecutionContext, Future}
import scala.language.implicitConversions
import scala.util.{Failure, Success, Try}
import scala.util.control.NonFatal

/**
  * @author 杨博 (Yang Bo)
  */
final case class Using[R <: AutoCloseable](open: () => R) extends AnyVal with Keyword[Using[R], R]

/**
  * @see [[dsl]] for usage of this [[Using]] keyword in continuations
  */
object Using {

  implicit def implicitUsing[R <: AutoCloseable](r: => R): Using[R] = Using[R](r _)

  def apply[R <: AutoCloseable](r: => R)(
      implicit dummyImplicit: DummyImplicit = DummyImplicit.dummyImplicit): Using[R] = new Using(r _)

  implicit def throwableContinuationUsingDsl[Domain, Value, R <: AutoCloseable](
      implicit catchDsl: CatchDsl[Domain, Domain, Value],
      shiftDsl: Dsl[Shift[Domain, Value], Domain, Value]
  ): Dsl[Using[R], Domain !! Value, R] =
    new Dsl[Using[R], Domain !! Value, R] {
      def cpsApply(keyword: Using[R], handler: R => Domain !! Value): Domain !! Value = _ {
        val r = keyword.open()
        try {
          !Shift(handler(r))
        } finally {
          r.close()
        }
      }
    }

  implicit def scalaFutureUsingDsl[R <: AutoCloseable, A](
      implicit executionContext: ExecutionContext): Dsl[Using[R], Future[A], R] =
    new Dsl[Using[R], Future[A], R] {
      def cpsApply(keyword: Using[R], handler: R => Future[A]): Future[A] = {
        Future(keyword.open()).flatMap { r: R =>
          def onFailure(e: Throwable): Future[Nothing] = {
            try {
              r.close()
              Future.failed(e)
            } catch {
              case NonFatal(e2) =>
                Future.failed(e2)
            }
          }

          def onSuccess(a: A): Future[A] = {
            try {
              r.close()
              Future.successful(a)
            } catch {
              case NonFatal(e2) =>
                Future.failed(e2)
            }
          }

          def returnableBlock(): Future[A] = {
            val fa: Future[A] = try {
              handler(r)
            } catch {
              case NonFatal(e) =>
                return onFailure(e)
            }
            fa.recoverWith {
                case NonFatal(e) =>
                  onFailure(e)
              }
              .flatMap(onSuccess)
          }
          returnableBlock()
        }
      }
    }

}
