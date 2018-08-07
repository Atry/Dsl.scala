package com.thoughtworks.dsl.keywords

import com.thoughtworks.dsl.Dsl
import com.thoughtworks.dsl.Dsl.Keyword

import scala.concurrent.Future
import scala.language.implicitConversions

/**
  * @author 杨博 (Yang Bo)
  * @example This `Yield` keyword must be put inside a function that returns `Stream[Element]` or `Stream[Element] !! ...`,
  *          or it will not compile.
  *
  *          {{{
  *          "def f(): Int = !Yield(1)" shouldNot compile
  *          }}}
  *
  */
final case class Yield[Element](element: Element) extends AnyVal with Keyword[Yield[Element], Unit]

object Yield {

  implicit def implicitYield[Element](element: Element): Yield[Element] = Yield[Element](element)

  implicit def yieldDsl[Element, That >: Element]: Dsl[Yield[Element], Stream[That], Unit] =
    new Dsl[Yield[Element], Stream[That], Unit] {
      def cpsApply(keyword: Yield[Element], mapper: Unit => Stream[That]): Stream[That] = {
        new Stream.Cons(keyword.element, mapper(()))
      }
    }

  implicit def futureYieldDsl[Element, That >: Element]: Dsl[Yield[Element], Stream[Future[That]], Unit] =
    new Dsl[Yield[Element], Stream[Future[That]], Unit] {
      def cpsApply(keyword: Yield[Element], handler: Unit => Stream[Future[That]]): Stream[Future[That]] = {
        new Stream.Cons(Future.successful(keyword.element), handler(()))
      }
    }
}
