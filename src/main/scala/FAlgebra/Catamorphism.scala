package FAlgebra

import algebra.Functor

/**
  * creation of a recursive data structure and then folding over it is a common pattern that cata tries to generalize.
  */
object Catamorphism {
  sealed trait Expression

  case class Value(v: Int) extends Expression
  case class Add(e1: Expression, e2: Expression) extends Expression
  case class Mult(l: Expression, e2: Expression) extends Expression

  def evalExpr(expr: Expression): Int = expr match {
    case Value(v)     => v
    case Add(e1, e2)  => evalExpr(e1) + evalExpr(e2)
    case Mult(e1, e2) => evalExpr(e1) * evalExpr(e2)
  }

  val expr: Expression = Mult(Add(Value(1), Value(2)), Value(3))

  /**
    * All references to Expression are replaced with a type parameter so the data structure is no longer recursive.
    */
  sealed trait ExpressionF[A]

  case class ValueF[A](v: Int) extends ExpressionF[A]
  case class AddF[A](e1: A, e2: A) extends ExpressionF[A]
  case class MultF[A](e1: A, e2: A) extends ExpressionF[A]

  implicit object ExpressionFunctor extends Functor[ExpressionF] {
    override def map[A, B](fa: ExpressionF[A])(f: A => B): ExpressionF[B] =
      fa match {
        case ValueF(v)     => ValueF(v)
        case AddF(e1, e2)  => AddF(f(e1), f(e2))
        case MultF(e1, e2) => MultF(f(e1), f(e2))
      }
  }

  /**
    * expr collapses everything into a single Expression while exprF encodes information about the nesting level of our expression tree.
    */
  val exprF: ExpressionF[ExpressionF[ExpressionF[Int]]] =
    MultF(AddF(ValueF(1), ValueF(2)), ValueF(3))

  // Single level
  def evalExprF(e: ExpressionF[Int]): Int = e match {
    case ValueF(v)     => v
    case AddF(e1, e2)  => e1 + e2
    case MultF(e1, e2) => e1 * e2
  }

  // 2 Level, reusing evalExprF defined above
  def evalExprF2(e: ExpressionF[ExpressionF[Int]]): Int = e match {
    case ValueF(v)     => v
    case AddF(e1, e2)  => evalExprF(e1) + evalExprF(e2)
    case MultF(e1, e2) => evalExprF(e1) * evalExprF(e2)
  }

  /**
    * generalize this nesting with a new type
    */
  final case class Fix[F[_]](unFix: F[Fix[F]])

  val fixedExprF: Fix[ExpressionF] =
    Fix(MultF(Fix(AddF(Fix(ValueF(1)), Fix(ValueF(2)))), Fix(ValueF(3))))

  def evalFixedExprF(e: Fix[ExpressionF]): Int =
    e.unFix match {
      case ValueF(v)     => v
      case AddF(e1, e2)  => evalFixedExprF(e1) + evalFixedExprF(e2)
      case MultF(e1, e2) => evalFixedExprF(e1) * evalFixedExprF(e2)
    }

  def almostCata(evaluator: ExpressionF[Int] => Int = evalExprF, e: Fix[ExpressionF]): Int =
    evaluator(Functor[ExpressionF].map(e.unFix)(almostCata(evaluator, _)))

  type Algebra[F[_], A] = F[A] => A

  def cata[F[_], A](alg: Algebra[F, A])(e: Fix[F])(implicit F: Functor[F]): A =
    alg(F.map(e.unFix)(cata(alg)))

  val algebra0: Algebra[ExpressionF, Int] = {
    case ValueF(v)     => v
    case AddF(e1, e2)  => e1 + e2
    case MultF(e1, e2) => e1 * e2
  }

  val algebra1: Algebra[ExpressionF, String] = {
    case ValueF(v)     => v.toString
    case AddF(e1, e2)  => "(" ++ e1 ++ " " ++ e2 ++ ")"
    case MultF(e1, e2) => e1 ++ e2
  }

  val initialAlgebra: Algebra[ExpressionF, Fix[ExpressionF]] = Fix[ExpressionF]

}
