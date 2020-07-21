package challenges.typesAndFunctions

import scala.util.Random

/*
Pure functions
 */
object RandomNumberGenenratorMemoized {

  def main(args: Array[String]): Unit = {

    // Part 1. Try to memoize a function from your standard library that you normally use to produce random numbers.
    // Does it work?
    val rng = new Random()
    val memoized = Memoization.memoize(rng.nextInt)

    println(memoized(6))
    println(memoized(6))
    println(memoized(6))
    println(memoized(6))
    println(memoized(6))
    // always produce the same output so NO.

    // Part 2. Most random number generators can be initialized with a seed. Implement a function that takes a seed, calls the
    // random number generator with that seed, and returns the result. Memoize that function. Does it work?
    val memoizedWithSeed: Int => Int => Int = Memoization.memoize((seed: Int) => (n: Int) => new Random(seed).nextInt(n))

    val rand = memoizedWithSeed(6)
    println(rand(5))
    println(rand(5))

    println(rand(4))
  }

}
