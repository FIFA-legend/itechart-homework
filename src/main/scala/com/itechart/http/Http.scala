package com.itechart.http

import cats.data.{EitherT, Kleisli}
import cats.effect.{Blocker, ExitCode, IO, IOApp}
import cats.implicits.{catsSyntaxEitherId, toSemigroupKOps}
import com.itechart.http.Result.{Greater, Lose, Lower, Win}
import io.circe.generic.JsonCodec
import org.http4s._
import org.http4s.dsl.io._
import org.http4s.implicits._
import org.http4s.circe.CirceEntityCodec._
import org.http4s.server.blaze.BlazeServerBuilder
import org.http4s.client.Client
import org.http4s.client.dsl.io._
import org.http4s.client.blaze.BlazeClientBuilder

import java.util.concurrent.atomic.{AtomicLong, AtomicReference}
import scala.concurrent.ExecutionContext
import scala.io.StdIn
import scala.util.{Random, Try}

// Homework. Place the solution under `http` package in your homework repository.
//
// Write a server and a client that play a number guessing game together.
//
// Communication flow should be as follows:
// 1. The client asks the server to start a new game by providing the minimum and the maximum number that can
//    be guessed.
// 2. The server comes up with some random number within the provided range.
// 3. The client starts guessing the number. Upon each attempt, the server evaluates the guess and responds to
//    the client, whether the current number is lower, greater or equal to the guessed one.
// 4. The game ends when the number is guessed or there are no more attempts left. At this point the client
//    should terminate, while the server may continue running forever.

@JsonCodec final case class Game(min: Int, max: Int, value: Int, attempts: Int)
@JsonCodec final case class CreateGame(min: Int, max: Int, attempts: Int)
@JsonCodec final case class Move(id: Long, value: Int)

@JsonCodec sealed trait Result

object Result {
  case object Win extends Result
  case object Lose extends Result
  case object Lower extends Result
  case object Greater extends Result
}

object GuessServer extends IOApp {
  override def run(args: List[String]): IO[ExitCode] = BlazeServerBuilder[IO](ExecutionContext.global)
    .bindHttp(port = 8080, host = "localhost")
    .withHttpApp(httpApp)
    .serve
    .compile
    .drain
    .as(ExitCode.Success)

  private val idReference: AtomicLong = new AtomicLong(0)

  private val gamesReference: AtomicReference[Map[Long, Game]] = new AtomicReference(Map.empty)

  private val createRoute = HttpRoutes.of[IO] { case req @ POST -> Root / "create" =>
    req.as[CreateGame].flatMap { game =>
      if (game.max <= game.min) BadRequest("Max value must be greater than min")
      else {
        val value = Random.nextInt(game.max - game.min + 1) + game.min
        val id    = idReference.incrementAndGet()
        gamesReference.updateAndGet(_.updated(id, Game(game.min, game.max, value, game.attempts)))
        Created(id)
      }
    }
  }

  private val playRoute = HttpRoutes.of[IO] { case req @ POST -> Root / "move" =>
    req.as[Move].flatMap { move =>
      val gameMap = gamesReference.get()
      gameMap.get(move.id) match {
        case None       => BadRequest("Wrong id parameter")
        case Some(game) => makeDecision(move, game)
      }
    }
  }

  def makeDecision(move: Move, game: Game): IO[Response[IO]] = {
    if (move.value < game.min || move.value > game.max) BadRequest("Value is out of bounds")
    else if (move.value == game.value) {
      gamesReference.updateAndGet(_.removed(move.value))
      Ok(Win.toString)
    } else if (game.attempts == 1) {
      gamesReference.updateAndGet(_.removed(move.value))
      Ok(Lose.toString)
    } else {
      gamesReference.updateAndGet(_.updated(move.id, game.copy(attempts = game.attempts - 1)))
      if (move.value < game.value) Ok(Greater.toString)
      else Ok(Lower.toString)
    }
  }

  private[http] val httpApp = { createRoute <+> playRoute }.orNotFound
}

object GuessClient extends IOApp {

  private val uri = uri"http://localhost:8080"

  private def printLine(string: String = ""): IO[Unit] = IO(println(string))

  private def readLine: IO[String] = IO(StdIn.readLine())

  override def run(args: List[String]): IO[ExitCode] = {
    BlazeClientBuilder[IO](ExecutionContext.global).resource
      .parZip(Blocker[IO])
      .use { case (client, blocker) =>
        for {
          tuple               <- configureGame()
          (min, max, attempts) = tuple
          id                  <- client.expect[Long](Method.POST(CreateGame(min, max, attempts), uri / "create"))
          _                   <- playGame(id, min, max, client)
        } yield ()
      }
      .as(ExitCode.Success)
  }

  implicit val userDecoder: EntityDecoder[IO, Result] = EntityDecoder.decodeBy(MediaType.text.plain) { m: Media[IO] =>
    EitherT {
      m.as[String].map {
        case "Win"     => Win.asRight
        case "Lose"    => Lose.asRight
        case "Lower"   => Lower.asRight
        case "Greater" => Greater.asRight
        case _         => InvalidMessageBodyFailure("Invalid value").asLeft
      }
    }
  }

  def playGame(id: Long, min: Int, max: Int, client: Client[IO]): IO[Unit] = {
    for {
      value  <- attempt(min, max)
      result <- client.expect[Result](Method.POST(Move(id, value), uri / "move"))
      _      <- routePlay(id, min, max, value, client).apply(result)
    } yield ()
  }

  def routePlay[F[_]](id: Long, min: Int, max: Int, value: Int, client: Client[IO]): Kleisli[IO, Result, Unit] =
    Kleisli[IO, Result, Unit] {
      case Result.Win  => IO(println(s"Congratulations, You won! Hidden value $value"))
      case Result.Lose => IO(println("Sorry, You lose :("))
      case Result.Lower =>
        IO(println(s"The hidden number is lower than $value")) *> playGame(id, min, value - 1, client)
      case Result.Greater =>
        IO(println(s"The hidden number is greater than $value")) *> playGame(id, value + 1, max, client)
    }

  def attempt(min: Int, max: Int): IO[Int] = IO((min + max) / 2)

  def configureGame(): IO[(Int, Int, Int)] = {
    for {
      values <- inputValues()
      tuple  <- routeConfiguration.apply(values)
    } yield tuple
  }

  def routeConfiguration[F[_]]: Kleisli[IO, Option[(Int, Int, Int)], (Int, Int, Int)] =
    Kleisli[IO, Option[(Int, Int, Int)], (Int, Int, Int)] {
      case Some(value) => IO(value)
      case None        => configureGame()
    }

  def inputValues(): IO[Option[(Int, Int, Int)]] = {
    for {
      _        <- printLine("Input min border:")
      min      <- readLine
      _        <- printLine("Input max border:")
      max      <- readLine
      _        <- printLine("Input amount of attempts:")
      attempts <- readLine
      tuple    <- parseInput(min, max, attempts)
    } yield tuple
  }

  def parseInput(min: String, max: String, attempts: String): IO[Option[(Int, Int, Int)]] = {
    IO {
      for {
        minimum <- Try(min.toInt).toOption
        maximum <- Try(max.toInt).toOption
        tries   <- Try(attempts.toInt).toOption
      } yield (minimum, maximum, tries)
    }
  }

}
