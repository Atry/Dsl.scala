package com.thoughtworks.dsl.keywords

import com.thoughtworks.dsl.Dsl
import com.thoughtworks.dsl.Dsl.{!!, Keyword}

import scala.language.implicitConversions
import scala.util.control.NonFatal

/**
  * @author 杨博 (Yang Bo)
  */
final case class Catch[Domain](failureHandler: Throwable => Domain) extends AnyVal with Keyword[Catch[Domain], Unit]

private[keywords] trait LowPriorityCatch0 { this: Catch.type =>

  implicit def catchContinuationDsl[Domain, Value](
      implicit restCatchDsl: Dsl[Catch[Domain], Domain, Unit]): Dsl[Catch[Domain !! Value], Domain !! Value, Unit] =
    new Dsl[Catch[Domain !! Value], Domain !! Value, Unit] {

      def interpret(keyword: Catch[Domain !! Value], block: Unit => Domain !! Value): Domain !! Value = {
        (continue: Value => Domain) =>
          restCatchDsl.interpret(
            Catch[Domain] { e =>
              keyword.failureHandler(e)(continue)
            }, { _: Unit =>
              block(())(continue)
            }
          )
      }
    }

}

object Catch extends LowPriorityCatch0 {

  implicit def implicitCatch[Domain](onFailure: Throwable => Domain): Catch[Domain] = Catch[Domain](onFailure)

  implicit def throwableCatchDsl2: Dsl[Catch[Throwable], Throwable, Unit] = new Dsl[Catch[Throwable], Throwable, Unit] {
    def interpret(keyword: Catch[Throwable], handler: Unit => Throwable): Throwable = {
      keyword.failureHandler(try {
        handler(())
      } catch {
        case NonFatal(e) =>
          e
      })
    }
  }

  implicit def throwableCatchDsl[Domain]: Dsl[Catch[Domain !! Throwable], Domain !! Throwable, Unit] =
    new Dsl[Catch[Domain !! Throwable], Domain !! Throwable, Unit] {
      def interpret(keyword: Catch[Domain !! Throwable], handler: Unit => Domain !! Throwable): Domain !! Throwable = {
        finalFailureHandler =>
          @inline
          def jvmCatch(block: => Domain !! Throwable)(failureHandler: Throwable => Domain): Domain = {
            (try {
              block
            } catch {
              case NonFatal(e) =>
                return failureHandler(e)
            }).apply(failureHandler)
          }

          jvmCatch(handler(())) { throwable =>
            jvmCatch(keyword.failureHandler(throwable))(finalFailureHandler)
          }

      }
    }

}
