package com.thoughtworks.dsl.instructions

import com.thoughtworks.dsl.Dsl
import com.thoughtworks.dsl.Dsl.Instruction

/**
  * @author 杨博 (Yang Bo)
  */
final case class AutoClose[R <: AutoCloseable](open: () => R) extends AnyVal with Instruction[AutoClose[R], R]

object AutoClose {
  def apply[R <: AutoCloseable](r: => R)(
      implicit dummyImplicit: DummyImplicit = DummyImplicit.dummyImplicit): AutoClose[R] = new AutoClose(r _)

  implicit def autoCloseDsl[Domain, R <: AutoCloseable, A](
      implicit dsl: com.thoughtworks.dsl.Dsl[com.thoughtworks.dsl.instructions.Catch[Domain], Domain, Domain => Domain])
    : Dsl[AutoClose[R], ((A => Domain) => Domain), R] =
    new Dsl[AutoClose[R], ((A => Domain) => Domain), R] {
      def interpret(autoClose: AutoClose[R], inUse: R => ((A => Domain) => Domain)): ((A => Domain) => Domain) = _ {
        val r = autoClose.open()
        try {
          !Shift(inUse(r))
        } finally {
          r.close()
        }
      }
    }

}
