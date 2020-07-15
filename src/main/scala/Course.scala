object Course {

  /**
   * [Functional Programming]
   *
   * Total, Deterministic, Pure
   *
   * Referential transparency, if all instances of an expression can be replaced by the result of evaluating the expression
   * without effecting the meaning of the program
   *
   * Don't use vars and don't throw exceptions, no return, break or continue
   */

  /**
   * [Category Theory]
   *
   * Rather than another branch of mathematics it's more like a common gene that unites different branches, a means of
   * getting a bird's-eye-view.
   *
   * By transporting the problem to a different realm (say, algebra), you can see the problem in a different light and
   * perhaps discover new tools, and the solution may become much easier.
   *
   * All np complete problems are reducible to each other, e.g. Subset-sum is a special case of Knapsack problem
   *
   * A category, then, is any collection of objects that can relate to each other via morphisms in sensible ways, like
   * composition and associativity.
   *
   * Objects are determined by--and understood by--the network of relationships they enjoy with all the other objects
   * of their species.
   *
   * A category is made up from objects and maps (aka morphisms or arrows) between these objects. Maps can be composed
   * in an associative fashion and for each object there is an identity map which is neutral with regard to composition.
   *
   * Doesn't care about what you're composing only care about how you're composing.
   *
   * Identity, Composition and Associativity
   *
   */

  object Category {
    def id[A]: A => A = a => a

    def compose[A, B, C](g: B => C, f: A => B): A => C =
      (a: A) => g(f(a))
  }

  /**
   * [Algebra]
   *
   * A collection of functions operating over some data type, along with a set of laws specifying relationships between these functions
   *
   * Algebraic design, we start with algebra (including it's laws) and decide on a representation later; we refine our
   * algebra to a minimal set of primitives
   *
   * An algebraic structure consists of:
   *
   * 1. A set of objects
   * 2. The operations that can be applied to those objects to create new objects from that set
   * 3. The laws that govern the operations
   *
   * e.g. set of numbers, operation be addition and multiplication
   *
   * There is an analogy in programming where:
   *
   * 1. Sets are types (specifically algebraic data types)
   * 2. Operators are functions
   * 3. Laws are properties (predicates expressed in terms of the algebra backed up by property-based tests)
   *
   * Equational reasoning
   *
   * Algebraic data types, ADTs, are composite types of (think algebra for data types)-
   *
   * 1. Sum type: is-a relationship, sealed trait hierarchies, let's you model the variations within a particular data type
   * 2. Product type: has-a relationship, product type using A and B has #A * #B instances, help cluster related data into
   * a larger abstraction
   *
   * We model our domain using ADTs
   *
   * e.g. case class creates a new type Pair from two instances of the existing type Int. The class constructor itself
   * is an operator that lets you create new types from existing Scala types, just like + and *.
   *
   * Objects are Types arrows are functions
   *
   * A Type is given meaning based on it's relationship to other types (which are specified by the set of functions and
   * their laws), rather than it's internal representation.
   *
   * When we recognize common structure among different solutions in different context we unite all of those instances of
   * the structure under a single definition and give it a name.
   *
   * The elements of algebraic structures can be related to domain driven design as follows:
   *
   * 1. Sets are types of entities or value objects
   * 2. Operators are business behavior
   * 3. Laws are business rules
   *
   * A program is a pure polymorphic function that uses algebras by combining their operators with other input parameters
   * to produce a pure value.
   *
   */

  /**
   * Expression Problem
   *
   * The goal is to define a datatype by cases, where one can add new cases to the datatype and new functions over the
   * datatype, without recompiling existing code, and while retaining static type safety
   */

  case class Triangle(a: Double, b: Double, c: Double)

  case class Square(edge: Double)

  trait Areable {
    def area: Double
  }

  implicit class AreableTriangle(val x: Triangle) extends Areable {
    /**
     * use Heron's formula to calculate area
     */
    def area: Double = {
      import x._
      val s = (a + b + c) / 2
      math.sqrt(s * (s - a) * (s - b) * (s - c))
    }
  }

  /**
   * Type class
   *
   * a type class as a programming technique that lets you add new behavior to closed data types without using inheritance,
   * and without having access to the original source code of those types, defines a set of operations that must be
   * supported by all instances of that type class.
   */

  /**
   *
   * [Category with 1 object: Monoid]
   *
   * every monoid is a kind of category, a category with one object.
   *
   * If there's only one object then every arrow has same domain and codomain, since there are no other objects around,
   * every arrow can be composed with every other arrow, composition is associative, and composing with identity on
   * either side leaves any arrow unchanged.
   *
   * arrows, are like elements that can be composed using `combine`
   *
   * e.g. intMonoid, identity = 0, combine = +, arrows are all integer values, we compose them using `combine`
   *
   * A monoid is a `type`, together with the monoid operations and a set of laws. Other than the laws satisfying, various
   * monoid instances have nothing in common
   *
   * If we say a type `A` is monoidal then having Monoid[A] instance is evidence of this fact
   *
   * The real power, monoids compose.
   */

  /**
   *
   * Foldable
   *
   * Higher kinds
   *
   * Just like functions that take other functions as arguments are called higher order functions, Foldable is higher
   * order type constructor or a higher kinded type
   *
   * having some metadata around your data, lifting, context vs content, effect
   *
   * Either, Future, Try, Option, List
   *
   * [Category in a box: Functor]
   *
   * Functor is a mapping between categories. It maps some source category C to destination category F by mapping objects
   * in C to objects in F and morphisms in C to morphisms in F.
   *
   * Consider two categories C1 and C2; then a functor F is a structure-preserving mapping between these categories.
   *
   * every object A ∈ C1 is mapped to to an object F(A) ∈ C2 and
   * every morphism A → B between two objects A, B ∈ C1 is mapped to a morphism F(A) → F(B) between two objects F(A), F(B) ∈ C2.
   *
   * Scala category: objects are Scala types and morphisms are functions between those types.
   *
   * Option Functor, both categories are the same, Scala category, it maps every type into Option(type), and every morphism
   * between types in Scala (that is, function between Scala types) into Option(thatFunction).
   *
   * given some object a or some arrow f: a → b from the original category, corresponding object (the one functor maps into)
   * is denoted as F(a) and corresponding arrow is denoted as F(f): F(a) → F(b)
   *
   * Effects
   *
   * an effect adds some capabilities to a computation. An effect is modeled usually in the form of a type constructor
   * that constructs types with these additional capabilities.
   *
   * Say you have any type A and you’d like to add the capability of aggregation, so that you can treat a collection of A
   * as a separate type. You do this by constructing a type List[A] (for which the corresponding type constructor is List),
   * which adds the effect of aggregation on A.
   *
   * Similarly, you can have Option[A] that adds the capability of optionality for the type A. Type constructors such as
   * Try and Future to model the effects of exceptions and latency, respectively.
   *
   * [Independent effects that want to be put together: Applicative]
   *
   * Applicative functors are more powerful than functors because they are able to deal with functions in a context (ap)
   *
   * Applicatives are about Effects, Batching and aggregation, Concurrency/Independency.
   *
   * All applicatives are functors
   *
   * [Monad]
   *
   * for loop, laws
   *
   * Monads don't compose
   */


}
