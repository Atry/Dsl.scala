package com.thoughtworks.dsl

import com.thoughtworks.dsl.annotations.shift

import scala.annotation.{compileTimeOnly, implicitNotFound}

/** The domain-specific interpreter for `Instruction` in `Domain`,
  * which is a dependent type type class that registers an asynchronous callback function,
  * to handle the `Value` inside `Instruction`.
  *
  * @tparam Value The value held inside `Instruction`.
  * @author 杨博 (Yang Bo)
  * @example Creating a collaborative DSL in [[https://github.com/ThoughtWorksInc/Dsl.scala Dsl.scala]] is easy.
  *          Only two steps are required:
  *
  *           - Defining their domain-specific [[com.thoughtworks.dsl.Dsl.Instruction Instruction]].
  *           - Implementing [[Dsl]] type class, which is a interpreter for an [[com.thoughtworks.dsl.Dsl.Instruction Instruction]].
  *
  *          TODO: an example for creating a DSL
  */
@implicitNotFound("Cannot interpret the DSL instruction ${Instruction} inside a function that returns ${Domain}.")
trait Dsl[-Instruction, Domain, +Value] {

  /** Registers an asynchronous callback `handler` on `instruction`, to handle the `Value`. */
  def interpret(instruction: Instruction, handler: Value => Domain): Domain

}

object Dsl {

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
    @compileTimeOnly(
      """This method requires the following compiler plugin: `addCompilerPlugin("com.thoughtworks.dsl" %% "compilerplugin" % "latest.release")`""")
    final def unary_! : Value = sys.error("Calls to this method should have been translated to `cpsApply`.")

    final def cpsApply[Domain](handler: Value => Domain)(implicit dsl: Dsl[Self, Domain, Value]): Domain = {
      dsl.interpret(this, handler)
    }

  }

}
