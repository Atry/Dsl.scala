package com.thoughtworks.dsl.keywords

import com.thoughtworks.dsl.Dsl
import com.thoughtworks.dsl.Dsl.Keyword
import scala.language.implicitConversions

/**
  * @author 杨博 (Yang Bo)
  * @example This `Yield` keyword must be put inside a function that returns `Stream[Element]` or `Stream[Element] !! ...`,
  *          or it will not compile.
  *
  *          {{{
  *          "def f(): Unit = !Yield(1)" shouldNot compile
  *          }}}
  *
  */
final case class Yield[Narrow](element: Narrow) extends AnyVal with Keyword[Yield[Narrow], Narrow]

object Yield {

  implicit def implicitYield[Narrow](element: Narrow): Yield[Narrow] = Yield[Narrow](element)

  implicit def yieldDsl[Narrow, Element >: Narrow]: Dsl[Yield[Narrow], Stream[Element], Narrow] =
    new Dsl[Yield[Narrow], Stream[Element], Narrow] {
      def interpret(keyword: Yield[Narrow], mapper: Narrow => Stream[Element]): Stream[Element] = {
        new Stream.Cons(keyword.element, mapper(keyword.element))
      }
    }
}
