package com.thoughtworks.dsl.instructions

import com.thoughtworks.dsl.Dsl
import com.thoughtworks.dsl.Dsl.Instruction
import resource.Resource
import scala.language.implicitConversions

/**
  * @author 杨博 (Yang Bo)
  */
final case class Arm[R](resourceFactory: () => R, resource: Resource[R]) extends Instruction[Arm[R], R]

object Arm {

  implicit def implicitArm[R: Resource](r: => R): Arm[R] = Arm[R](r)

  def apply[R](r: => R)(implicit resource: Resource[R], dummyImplicit: DummyImplicit = DummyImplicit.dummyImplicit) = {
    new Arm[R](r _, resource)
  }

  implicit def armDsl[Domain, R, A](
      implicit catchDsl: Dsl[Catch[Domain], Domain, Unit]): Dsl[Arm[R], ((A => Domain) => Domain), R] =
    new Dsl[Arm[R], ((A => Domain) => Domain), R] {
      def interpret(arm: Arm[R], inUse: R => ((A => Domain) => Domain)): ((A => Domain) => Domain) = {
        val Arm(resourceFactory, resource) = arm
        _ {
          val r = resourceFactory()
          try {
            resource.open(r)
            !Shift(inUse(r))
          } finally {
            resource.close(r)
          }
        }
      }
    }

}
