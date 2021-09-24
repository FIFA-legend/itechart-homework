package com.itechart.cats_effect

import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits.{catsSyntaxOptionId, none}

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
