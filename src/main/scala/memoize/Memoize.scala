package memoize

import scala.collection.mutable

trait Memoize {

  /**
    * This function takes a pure function f as an argument and returns a function that behaves almost exactly like f,
    * except that it only calls the original function once for every argument, stores the result internally, and
    * subsequently returns this stored result every time it’s called with the same argument.
    */
  def memoize[I, O](f: I => O): I => O = new mutable.HashMap[I, O]() {
    override def apply(key: I): O = getOrElseUpdate(key, f(key))
  }

}

object Memoize extends Memoize
