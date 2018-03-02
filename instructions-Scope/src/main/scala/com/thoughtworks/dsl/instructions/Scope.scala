package com.thoughtworks.dsl.instructions

import com.thoughtworks.dsl.Dsl
import com.thoughtworks.dsl.Dsl.{Continuation, Instruction}

/**
  * @author 杨博 (Yang Bo)
  */
final case class Scope[Domain, Value](continuation: Continuation[Domain, Value])
    extends AnyVal
    with Instruction[Scope[Domain, Value], Value]
object Scope {

  implicit def scopeContinuationDsl[Domain, DomainValue, ScopeValue](
      implicit restScopeDsl: Dsl[Scope[Domain, DomainValue], Domain, DomainValue])
    : Dsl[Scope[Continuation[Domain, DomainValue], ScopeValue], Continuation[Domain, DomainValue], ScopeValue] =
    new Dsl[Scope[Continuation[Domain, DomainValue], ScopeValue], Continuation[Domain, DomainValue], ScopeValue] {

      def interpret(instruction: Scope[Continuation[Domain, DomainValue], ScopeValue],
                    handler: ScopeValue => Continuation[Domain, DomainValue]): Continuation[Domain, DomainValue] = {
        restScopeDsl.interpret(Scope[Domain, DomainValue](instruction.continuation(handler)), _)
      }
    }
}
