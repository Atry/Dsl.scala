package com.thoughtworks.dsl.instructions

import com.thoughtworks.dsl.Dsl
import com.thoughtworks.dsl.Dsl.Instruction

import scala.util.control.Exception.Catcher
import scala.util.control.NonFatal

/**
  * @author 杨博 (Yang Bo)
  */
final case class Catch[Domain](onFailure: Throwable => Domain)
    extends AnyVal
    with Instruction[Catch[Domain], Domain => Domain]

object Catch {

  implicit def catchContinuationDsl[Domain, Value](implicit restCatchDsl: Dsl[Catch[Domain], Domain, Domain => Domain])
    : Dsl[Catch[(Value => Domain) => Domain],
          (Value => Domain) => Domain,
          ((Value => Domain) => Domain) => ((Value => Domain) => Domain)] =
    new Dsl[Catch[(Value => Domain) => Domain],
            (Value => Domain) => Domain,
            ((Value => Domain) => Domain) => ((Value => Domain) => Domain)] {
      def interpret(
          catcher: Catch[(Value => Domain) => Domain],
          block: (((Value => Domain) => Domain) => (Value => Domain) => Domain) => (Value => Domain) => Domain)
        : (Value => Domain) => Domain = _ {
        try {
          !Shift(
            block { (continuation: (Value => Domain) => Domain) =>
              _ {
                try {
                  !Shift(continuation)
                } catch {
                  case NonFatal(e) =>
                    !Shift(catcher.onFailure(e))
                }
              }
            }
          )
        } catch {
          case NonFatal(e) =>
            !Shift(catcher.onFailure(e))
        }
      }
    }
}
