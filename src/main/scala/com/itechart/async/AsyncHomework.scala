package com.itechart.async

import java.net.URL
import java.nio.charset.Charset
import java.util.concurrent.Executors
import java.util.concurrent.atomic.AtomicReference
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.io.{Codec, Source}
import scala.util.{Failure, Success}

/** Application:
  * - takes a web-page URL from arguments (args array)
  * - loads the web-page body, extracts HTTP links from it
  * - for all the found links, tries to fetch a server name header if there is one
  * - prints all the encountered unique server name values in alphabetical order
  *
  * Each link processing should be done in parallel.
  * Validation of arguments is not needed.
  *
  * Try to test it on http://google.com!
  */
object AsyncHomework {
  implicit private val ec: ExecutionContext = ExecutionContext.fromExecutor(Executors.newCachedThreadPool())

  import cats.syntax.traverse._
  import cats.instances.list._
  def main(args: Array[String]): Unit = {
    val future = args.toList
      .map(processUrl)
      .sequence

    future.onComplete {
      case Failure(exception) => println(exception)
      case Success(value)     => println(uniqueServers(value))
    }
  }

  private def uniqueServers(list: List[List[String]]): List[String] = {
    list.flatten
      .filterNot(_ == null)
      .distinct
      .sorted
  }

  private def processUrl(url: String): Future[List[String]] = {
    for {
      body        <- fetchPageBody(url)
      links       <- findLinkUrls(body)
      serverNames <- links.traverse(fetchServerName)
    } yield serverNames
  }

  private def fetchPageBody(url: String): Future[String] = {
    println(f"Fetching $url")
    Future {
      val source = Source.fromURL(url)(new Codec(Charset.forName("windows-1251")))
      try {
        source.mkString
      } finally {
        source.close()
      }
    }
  }

  private def fetchServerName(url: String): Future[String] = {
    println(s"Fetching server name header for $url")
    Future {
      new URL(url).openConnection().getHeaderField("Server")
    }
  }

  private def findLinkUrls(html: String): Future[List[String]] = Future {
    val linkPattern = """href="(http[^"]+)"""".r
    linkPattern.findAllMatchIn(html).map(m => m.group(1)).toList
  }
}

/*
Make this work correctly a) first with synchronized blocks, b) then with AtomicReference
 */
object ExerciseSolve1 extends App {
  import scala.concurrent.ExecutionContext.Implicits.global
  val tasksCount     = 100
  val taskIterations = 1000
  val initialBalance = 10

  // PLACE TO FIX - START
  var balance1: Int = initialBalance
  var balance2: Int = initialBalance

  def doTaskIteration(): Unit = synchronized {
    val State(newBalance1, newBalance2) = transfer(State(balance1, balance2))
    balance1 = newBalance1
    balance2 = newBalance2
  }

  def printBalancesSum(): Unit = {
    println(balance1 + balance2)
  }
  // PLACE TO FIX - FINISH

  def transfer(state: State): State = {
    if (state.balance1 >= state.balance2) {
      State(state.balance1 - 1, state.balance2 + 1)
    } else {
      State(state.balance1 + 1, state.balance2 - 1)
    }
  }

  val tasks = (1 to tasksCount).toVector.map(_ =>
    Future {
      (1 to taskIterations).foreach(_ => doTaskIteration())
    }
  )
  val tasksResultFuture: Future[Vector[Unit]] = Future.sequence(tasks)
  Await.ready(tasksResultFuture, 5.seconds)

  printBalancesSum() //should print 20

  final case class State(balance1: Int, balance2: Int)
}

object ExerciseSolve2 extends App {
  import scala.concurrent.ExecutionContext.Implicits.global
  val tasksCount     = 100
  val taskIterations = 1000
  val initialBalance = 10

  // PLACE TO FIX - START
  val balances: AtomicReference[State] = new AtomicReference[State](State(initialBalance, initialBalance))

  def doTaskIteration(): Unit = {
    balances.updateAndGet(transfer)
  }

  def printBalancesSum(): Unit = {
    val state = balances.get()
    println(state.balance1 + state.balance2)
  }
  // PLACE TO FIX - FINISH

  def transfer(state: State): State = {
    if (state.balance1 >= state.balance2) {
      State(state.balance1 - 1, state.balance2 + 1)
    } else {
      State(state.balance1 + 1, state.balance2 - 1)
    }
  }

  val tasks = (1 to tasksCount).toVector.map(_ =>
    Future {
      (1 to taskIterations).foreach(_ => doTaskIteration())
    }
  )
  val tasksResultFuture: Future[Vector[Unit]] = Future.sequence(tasks)
  Await.ready(tasksResultFuture, 5.seconds)

  printBalancesSum() //should print 20

  final case class State(balance1: Int, balance2: Int)

}
