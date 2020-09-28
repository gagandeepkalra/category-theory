package free

object Trampolining {
  sealed trait TailRec[A] {
    def flatMap[B](f: A => TailRec[B]): TailRec[B] =
      FlatMap(this, f)
    def map[B](f: A => B): TailRec[B] =
      flatMap(f andThen Return.apply)

    @scala.annotation.tailrec
    final def run: A = {
      this match {
        case Return(a)                    => a
        case Suspend(resume)              => resume().run
        case FlatMap(Return(z), g)        => g(z).run
        case FlatMap(Suspend(resumeZ), g) => resumeZ().flatMap(g).run
        case FlatMap(FlatMap(fz, f), g)   => fz.flatMap(f(_).flatMap(g)).run
      }
    }
  }

  case class Return[A](a: A) extends TailRec[A]
  case class Suspend[A](resume: () => TailRec[A]) extends TailRec[A]
  case class FlatMap[A, B](fa: TailRec[A], f: A => TailRec[B]) extends TailRec[B]

  @scala.annotation.tailrec
  def run[A](tailRec: TailRec[A]): A = {
    tailRec match {
      case Return(a)                    => a
      case Suspend(resume)              => run(resume())
      case FlatMap(Return(z), g)        => run(g(z))
      case FlatMap(Suspend(resumeZ), g) => run(resumeZ().flatMap(g))
      case FlatMap(FlatMap(fz, f), g)   => run(fz.flatMap(f(_).flatMap(g)))
    }
  }

  def main(args: Array[String]): Unit = {
    val f: (Int => TailRec[Int]) = Return(_)

    val g = List.fill(10000)(f).foldLeft(f) {
      (acc: Int => TailRec[Int], elem: Int => TailRec[Int]) => x =>
        Suspend(() => acc(x).flatMap(elem))
    }

    println(run(g(42)))

  }

}
