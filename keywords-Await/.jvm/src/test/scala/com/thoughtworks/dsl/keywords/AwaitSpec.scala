package com.thoughtworks.dsl.keywords

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import akka.stream.ActorMaterializer
import akka.util.ByteString
import com.thoughtworks.dsl.Dsl.reset
import org.scalatest.{AsyncFreeSpec, Matchers}

import scala.concurrent.Future
import scala.concurrent.duration._

/**
  * @author 杨博 (Yang Bo)
  */
final class AwaitSpec extends AsyncFreeSpec with Matchers {
  
  implicit val system = ActorSystem()
  implicit val materializer = ActorMaterializer()

  def downloadTwoPages(): Future[(ByteString, ByteString)] = Future {
    val response1 = !Await(Http().singleRequest(HttpRequest(HttpMethods.GET, "http://example.com")))
    val content1 = !Await(response1.entity.toStrict(timeout = 5.seconds))
    val response2 = !Await(Http().singleRequest(HttpRequest(HttpMethods.GET, "http://example.net")))
    val content2 = !Await(response2.entity.toStrict(timeout = 5.seconds))
    (content1.data, content2.data)
  }

  "download two pages" in {
    downloadTwoPages().map {
      case (bytes1, bytes2) =>
        bytes1 should be(bytes2)
    }
  }

  "https get" in ({
    val response = !Await(Http().singleRequest(HttpRequest(uri = "https://example.com")))
    response.status should be(StatusCodes.OK)
  }: @reset)

  "multiple https" in ({

    def createAsynchronousStream(): Stream[Future[Int]] = {
      val response1 = !Await(Http().singleRequest(HttpRequest(uri = "http://example.com")))
      !Yield(response1.status.intValue())
      response1.discardEntityBytes()
      val response2 = !Await(Http().singleRequest(HttpRequest(uri = "http://example.net")))
      !Yield(response2.status.intValue())
      response2.discardEntityBytes()
      Stream.empty[Future[Int]]
    }

    val asynchronousStream = createAsynchronousStream()
    !Await(asynchronousStream(0)) should be(StatusCodes.OK.intValue)
    !Await(asynchronousStream(1)) should be(StatusCodes.OK.intValue)

  }: @reset)

}
