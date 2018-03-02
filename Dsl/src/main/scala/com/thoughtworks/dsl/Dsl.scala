package com.thoughtworks.dsl

import scala.annotation._

/** The domain-specific interpreter for `Instruction` in `Domain`,
  * which is a dependent type type class that registers an asynchronous callback function,
  * to handle the `Value` inside `Instruction`.
  *
  * @tparam Value The value held inside `Instruction`.
  * @author 杨博 (Yang Bo)
  */
trait Dsl[-Instruction, Domain, +Value] {

  /** Registers an asynchronous callback `handler` on `instruction`, to handle the `Value`. */
  def interpret(instruction: Instruction, handler: Value => Domain): Domain

}

object Dsl {

  trait Trampoline1[A, R] extends Function1[A, R] {
    def step(): A => R

    @tailrec
    final def apply(a: A): R = {
      step() match {
        case trampoline: Trampoline1[A, R] =>
          trampoline(a)
        case last =>
          last(a)
      }
    }
  }

  implicit def continuationDsl[Instruction, Domain, FinalResult, InstructionValue](
      implicit restDsl: Dsl[Instruction, Domain, InstructionValue])
    : Dsl[Instruction, Continuation[Domain, FinalResult], InstructionValue] = {
    new Dsl[Instruction, Continuation[Domain, FinalResult], InstructionValue] {
      def interpret(
          instruction: Instruction,
          handler: InstructionValue => Continuation[Domain, FinalResult]): Continuation[Domain, FinalResult] = {
        (continue: FinalResult => Domain) =>
          restDsl.interpret(instruction, { a =>
            handler(a)(continue)
          })
      }
    }
  }

  private[dsl] /* sealed */ trait ResetAnnotation extends Annotation with StaticAnnotation
  private[dsl] final class nonTypeConstraintReset extends ResetAnnotation with StaticAnnotation

  /** An annotation to explicitly perform reset control operator on a code block. */
  final class reset extends ResetAnnotation with StaticAnnotation with TypeConstraint

  /** An annotation to mark a method is a shift control operator. */
  final class shift extends StaticAnnotation

  type Continuation[Domain, +Value] = ((Value => Domain @reset) => Domain @reset) @reset

  def apply[Instruction, Domain, Value](
      implicit typeClass: Dsl[Instruction, Domain, Value]): Dsl[Instruction, Domain, Value] =
    typeClass

  /**
    *
    * @tparam Self the self type
    * @see [[https://en.wikipedia.org/wiki/Curiously_recurring_template_pattern Curiously recurring template pattern]] for the reason why we need `Instruction` type parameter
    */
  trait Instruction[Self, Value] extends Any { this: Self =>

    @shift
//    @compileTimeOnly(
//      """This method requires the following compiler plugin: `addCompilerPlugin("com.thoughtworks.dsl" %% "compilerplugin" % "latest.release")`""")
    final def unary_! : Value = sys.error("Calls to this method should have been translated to `cpsApply`.")

    final def cpsApply[Domain](handler: Value => Domain)(implicit dsl: Dsl[Self, Domain, Value]): Domain = {
      dsl.interpret(this, handler)
    }

  }

}
