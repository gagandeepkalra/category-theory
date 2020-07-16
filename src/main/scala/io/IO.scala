package io

import scala.io.StdIn

sealed class IO[A](val unsafePerformIO: () => A) {
  final def map[B](ab: A => B): IO[B] =
    new IO(() => ab(unsafePerformIO()))

  final def flatMap[B](afb: A => IO[B]): IO[B] =
    new IO(() => afb(unsafePerformIO()).unsafePerformIO())

  final def attempt: IO[Either[Throwable, A]] = new IO(() => {
    try Right(unsafePerformIO())
    catch {
      case t: Throwable => Left(t)
    }
  })
}

object IO {
  final def apply[A](a: => A): IO[A] = new IO(() => a)

  final def fail[A](t: Throwable): IO[A] = new IO(() => throw t)

  def main(args: Array[String]): Unit = {

    val effect: IO[Unit] =
      for {
        _ <- IO(println("xyz"))
        x <- IO(StdIn.readInt())
        _ <- IO(println(s"you typed $x"))
      } yield ()

    effect.attempt.unsafePerformIO()
  }
}