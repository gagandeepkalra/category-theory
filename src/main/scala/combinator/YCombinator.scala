package combinator

/**
  *
  * Y = lambda f.(lambda x.f (x x)) (lambda x.f (x x))
  *
  * Y g  = (λf . (λx . f (x x)) (λx . f (x x))) g    (by definition of Y)
  * = (λx . g (x x)) (λx . g (x x))                  (β-reduction of λf: applied main function to g)
  * = (λy . g (y y)) (λx . g (x x))                  (α-conversion: renamed bound variable)
  * = g ((λx . g (x x)) (λx . g (x x)))              (β-reduction of λy: applied left function to right function)
  * = g (Y g)                                        (by second equality) [1]
  *
  *
  * Fixed point, value of x such that f(x) = x
  *
  * A fixed-point combinator is a higher-order function that returns some fixed point of its argument function, if one exists.
  *
  * def factorialGenerator(f: Int => Int): Int => Int =
  *   (x: Int) => if (x == 0) 1 else x * f(x - 1)
  *
  * The fixed point of the `factorialGenerator` is the `factorial` function
  *
  * factorial = Y(factorialGenerator)
  *
  * This is why it's known as fixed point combinator.
  *
  */
trait YCombinator {

  /**
    * copied implementation from Rosetta code, refer- https://rosettacode.org/wiki/Y_combinator#Scala
    *
    * General implementation-
    *
    * def Y[T](func: (T => T) => (T => T)): T => T =
    *  func(Y[T](func)(_: T))
    *
    * but here Y is not a combinator.
    *
    * A combinator is function with no free variables. That means, amongst other things, that the combinator does not
    * have dependencies on things outside of the function, only on the function parameters.
    */
  def Y[A, B](f: (A => B) => (A => B)): A => B = {
    case class W(wf: W => A => B) {
      def get: A => B =
        wf(this)
    }
    val g: W => A => B = w => a => f(w.get)(a)

    g(W(g))
  }

}

object YCombinator extends YCombinator
