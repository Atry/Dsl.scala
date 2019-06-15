//package com.thoughtworks.dsl.keywords
//
//import com.thoughtworks.dsl.Dsl.!!
//
///**
//  * @author 杨博 (Yang Bo)
//  */
//class Foo {
//
//  def xx = {
//    var i = 0
//    import com.thoughtworks.dsl
//    import dsl._
//    {
//      @inline def while1[Domain1](endWhile1: => Domain1)(body1: (=> Domain1) => Domain1,
//                                                         condition1: (Boolean => Domain1) => Domain1): Domain1 =
//        condition1.apply(
//          (
//              (conditionValue1: Boolean) =>
//                if (conditionValue1)
//                  body1.apply(while1[Domain1](endWhile1)(body1, condition1))
//                else
//                  endWhile1));
//      while1[Stream[String] !! Throwable](({
//        throw new java.lang.AssertionError("Unreachable code")
//      }: Stream[String] !! Throwable))(
//        (
//            (continue1: (Stream[String] !! Throwable)) =>
//              Catch
//                .tryCatch[Stream[String] !! Throwable, Stream[String] !! Throwable, Unit](((result1: Unit) => {
//                  i = i.+(1);
//                  continue1
//                }))(keywords.Catch.throwableCatchDsl[Stream[String], Unit](
//                  keywords.Shift.stackUnsafeShiftDsl[Stream[String], Throwable]))
//                .apply(
//                  ((finalizer1: Unit => Stream[String] !! Throwable @com.thoughtworks.dsl.Dsl.reset) =>
//                    new com.thoughtworks.dsl.keywords.Yield[String]("x")
//                      .cpsApply[Stream[String] !! Throwable](((a1: Unit) =>
//                        finalizer1.apply({
//                          if (true)
//                            throw new java.lang.AssertionError("x")
//                          else
//                            ()
//                        })))(dsl.Dsl
//                        .throwableContinuationDsl[com.thoughtworks.dsl.keywords.Yield[String], Stream[String], Unit](
//                          keywords.Yield.streamYieldDsl[String, String]))),
//                  PartialFunction.empty[Throwable, Stream[String] !! Throwable !! Unit]
//                )),
//        ((conditionHandler1: Boolean => Stream[String] !! Throwable) => conditionHandler1.apply(true))
//      )
//    }
//  }
//}
