package com.thoughtworks.dsl.domains

import com.thoughtworks.dsl.Dsl
import com.thoughtworks.dsl.instructions.Catch

import scala.util.control.Exception.Catcher
import scala.util.control.NonFatal

/** The state for DSL in exception-handling domain.
  *
  * @author 杨博 (Yang Bo)
  */
trait ExceptionHandling[OtherDomain] { self =>
  def onFailure(handler: Throwable => OtherDomain): OtherDomain

}

object ExceptionHandling {

  implicit def catchDsl[OtherDomain]: Dsl[Catch[ExceptionHandling[OtherDomain]],
                                          ExceptionHandling[OtherDomain],
                                          ExceptionHandling[OtherDomain] => ExceptionHandling[OtherDomain]] = {
    new Dsl[Catch[ExceptionHandling[OtherDomain]],
            ExceptionHandling[OtherDomain],
            ExceptionHandling[OtherDomain] => ExceptionHandling[OtherDomain]] {
      def interpret(
          instruction: Catch[ExceptionHandling[OtherDomain]],
          continuation: (
              ExceptionHandling[OtherDomain] => ExceptionHandling[OtherDomain]) => ExceptionHandling[OtherDomain])
        : ExceptionHandling[OtherDomain] = {

        new ExceptionHandling[OtherDomain] {
          def onFailure(failureHandler: Throwable => OtherDomain): OtherDomain = {
            def handleRethrow(e: Throwable): OtherDomain = {
              locally {
                try {
                  instruction.onFailure(e)
                } catch {
                  case NonFatal(rethrown) =>
                    return failureHandler(rethrown)
                }
              }.onFailure(failureHandler)

            }

            locally {
              try {
                continuation { domain =>
                  ExceptionHandling.success(domain.onFailure(failureHandler))
                }
              } catch {
                case NonFatal(e) => return handleRethrow(e)
              }
            }.onFailure(handleRethrow)
          }
        }

      }
    }
  }

  def success[Domain](r: Domain): ExceptionHandling[Domain] = new ExceptionHandling[Domain] {
    def onFailure(handler: Throwable => Domain): Domain = r
  }

  def failure[Domain](e: Throwable): ExceptionHandling[Domain] = new ExceptionHandling[Domain] {
    def onFailure(handler: Throwable => Domain): Domain = handler(e)
  }

  implicit def exceptionHandlingDsl[Instruction, Domain, A](
      implicit restDsl: Dsl[Instruction, Domain, A]): Dsl[Instruction, ExceptionHandling[Domain], A] =
    new Dsl[Instruction, ExceptionHandling[Domain], A] {
      def interpret(instruction: Instruction,
                    successHandler: A => ExceptionHandling[Domain]): ExceptionHandling[Domain] =
        new ExceptionHandling[Domain] {
          def onFailure(failureHandler: Throwable => Domain): Domain = {
            def restHandler(a: A): Domain =
              (try {
                successHandler(a)
              } catch {
                case NonFatal(e) =>
                  return failureHandler(e)
              }).onFailure(failureHandler)

            restDsl.interpret(instruction, restHandler)
          }
        }

    }

}
