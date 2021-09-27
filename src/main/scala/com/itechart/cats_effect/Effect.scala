package com.itechart.cats_effect

import cats.effect.implicits.catsEffectSyntaxBracket
import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits.{catsSyntaxApplicativeError, catsSyntaxOptionId, catsSyntaxTuple2Semigroupal, none}

import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scala.io.StdIn

object Effect {

  // 1 - sequence two IOs and take the result of the LAST one
  // hint: use flatMap
  def sequenceTakeLast[A, B](ioa: IO[A], iob: IO[B]): IO[B] = {
    for {
      _      <- ioa
      result <- iob
    } yield result
  }

  // 2 - sequence two IOs and take the result of the FIRST one
  // hint: use flatMap
  def sequenceTakeFirst[A, B](ioa: IO[A], iob: IO[B]): IO[A] = {
    for {
      _      <- iob
      result <- ioa
    } yield result
  }

  // 3 - repeat an IO effect forever
  // hint: use flatMap + recursion
  def forever[A](io: IO[A]): IO[A] = {
    for {
      _      <- io
      result <- forever(io)
    } yield result
  }

  // 4 - convert an IO to a different type
  // hint: use map
  def convert[A, B](ioa: IO[A], value: B): IO[B] = ioa.map(_ => value)

  // 5 - discard value inside an IO, just return Unit
  def asUnit[A](ioa: IO[A]): IO[Unit] = ioa.void

  def main(args: Array[String]): Unit = {
    val ioa = IO(println("a"))
    val iob = IO(5)
    println(sequenceTakeFirst(iob, ioa).unsafeRunSync())
    println((iob <* ioa).unsafeRunSync())
  }
}

object Additional1 extends IOApp {

  /** 1. Write a function that runs an IO on another thread, and, depending on the result of the fiber
    *   - return the result in an IO
    *   - if errored or cancelled, return a failed IO
    */
  def processResultsFromFiber[A](io: IO[A]): IO[A] = {
    for {
      fiber <- io
        .onCancel(IO(println("The effect was cancelled")).void)
        .onError(IO.raiseError(_))
        .start
      result <- fiber.join
    } yield result
  }

  override def run(args: List[String]): IO[ExitCode] = {
    val ioa = IO(4 / 2)
    val iob = IO(4 / 0)
    println(processResultsFromFiber(ioa).unsafeRunSync())
    println(processResultsFromFiber(iob).unsafeRunSync())
    processResultsFromFiber(ioa) as ExitCode.Success
  }
}

object Additional2 extends IOApp {

  /** 2. Write a function that takes two IOs, runs them on different fibers and returns an IO with a tuple containing both results.
    *   - if both IOs complete successfully, tuple their results
    *   - if the first IO returns an error, raise that error (ignoring the second IO's result/error)
    *   - if the first IO doesn't error but second IO returns an error, raise that error
    *   - if one (or both) canceled, raise a RuntimeException
    */
  def tupleIOs[A, B](ioa: IO[A], iob: IO[B]): IO[(A, B)] = {
    IO.racePair(
      ioa.onCancel(IO(throw new RuntimeException("IO is cancelled")).void),
      iob.onCancel(IO(throw new RuntimeException("IO is cancelled")).void)
    ).flatMap {
      case Left((a, fb))  => (IO.pure(a), fb.join).tupled
      case Right((fa, b)) => (fa.join, IO.pure(b)).tupled
    }
  }

  override def run(args: List[String]): IO[ExitCode] = {
    val ioa = IO("a")
    val iob = IO(println(2 / 0))
    println(tupleIOs(ioa, iob).unsafeRunSync())
    tupleIOs(ioa, iob) as ExitCode.Success
  }
}

object Additional3 extends IOApp {

  /**  3. Write a function that adds a timeout to an IO:
    *    - IO runs on a fiber
    *    - if the timeout duration passes, then the fiber is canceled
    *    - the method returns an IO[A] which contains
    *      - the original value if the computation is successful before the timeout signal
    *      - the exception if the computation is failed before the timeout signal
    *      - a RuntimeException if it times out (i.e. cancelled by the timeout)
    */
  def timeout[A](io: IO[A], duration: FiniteDuration): IO[A] = {
    val sleep = IO.sleep(duration)
    IO.racePair(io, sleep).flatMap {
      case Left((a, fb))  => IO.pure(a)
      case Right((fa, b)) => fa.cancel *> IO.raiseError(new RuntimeException("IO is cancelled"))
    }
  }

  override def run(args: List[String]): IO[ExitCode] = {
    val io = IO((1 to 10000).toList.sum)
    println(timeout(io, 2.seconds).unsafeRunSync())
    println(timeout(io, 2.nanoseconds).unsafeRunSync())
    timeout(io, 2.seconds) as ExitCode.Success
  }
}

trait Console {
  def putStrLn(value: String): IO[Unit]
  def readStrLn: IO[String]
}

object Console {
  object Real extends Console {
    def putStrLn(value: String): IO[Unit] = IO(println(value))
    def readStrLn: IO[String] = IO(StdIn.readLine())
  }
}

object Exercise1_Common {
  def response(animal: String): Option[String] = animal.trim match {
    case "cat" | "cats"  => "In ancient times cats were worshipped as gods; they have not forgotten this.".some
    case "dog" | "dogs"  => "Be the person your dog thinks you are.".some
    case x if x.nonEmpty => s"I don't know what to say about '$x'.".some
    case _               => none
  }
}

object Exercise1_Functional extends IOApp {
  import Exercise1_Common._

  def process(console: Console, counter: Int = 0): IO[ExitCode] = {
    def processOutput(option: Option[String]): IO[ExitCode] = {
      option match {
        case Some(value) => console.putStrLn(value) as ExitCode.Success
        case None =>
          if (counter >= 2)
            console.putStrLn("I am disappoint. You have failed to answer too many times.") as ExitCode.Error
          else
            console.putStrLn("Empty input is not valid, try again...") *> process(console, counter + 1)
      }
    }

    for {
      _      <- console.putStrLn("What is your favourite animal?")
      animal <- console.readStrLn
      output  = response(animal)
      code   <- processOutput(output)
    } yield code
  }

  override def run(args: List[String]): IO[ExitCode] = process(Console.Real)
}
