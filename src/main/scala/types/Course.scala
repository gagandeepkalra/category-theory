package types

object Course {

  /**
    * Type parameters
    */
  trait TypeParameterStack[E] {
    def push(e: E): Unit
    def pop(): E
  }
  object Test {

    /**
      * Existential Type
      */
    def popAndPush(stack: TypeParameterStack[_]): Unit = {
      val elem = stack.pop()
      // stack.push(elem) // error
    }
  }

  /**
    * Type members
    */
  trait TypeMemberStack {
    type E
    def push(e: E): Unit
    def pop(): E
  }

  object TypeMemberStack {
    def popAndPush(stack: TypeMemberStack): Unit = {
      val elem = stack.pop() // no error :) This is because type members (even though they're abstract) can be referred to outside of its containing class/trait.
      stack.push(elem)
    }
  }

  /**
    *
    * Advantages of type members over generics:
    *
    * 1. type members don't pollute type declarations of your trait, like generics do
    * 2. adding a type member to class or trait is by itself a fully backwards compatible change, while adding a type parameter
    *    will require you to rewrite a lot of code that uses your trait or class
    * 3. type member may be referred to outside of its declaring class or trait using path-dependent types
    * 4. you can concretize an abstract type member with inner class or trait
    *
    * Advantages of generics over type members:
    *
    * 1. generics may be referred to in constructor parameters (and self-type annotation)
    * 2. type parameters are recognized by Java
    * 3. names of type parameters may be much more freely changed than names of type members (without breaking backwards compatibility)
    */
  class A {
    class B
    var b: List[B] = Nil
  }
  val a1 = new A
  val a2 = new A

  val b1 = new a1.B
  val b2 = new a2.B

  a1.b = List(b1)
  // a2.b = List(b1) // error

  /**
    * Path dependent types
    *
    * A dependent type is a type that depends on a value.
    *
    * Type dependent on a value.
    *
    * Type safety that is dependent on values and not types alone.
    *
    * Place logic that is usually only available at runtime into types.
    *
    * A nested type is bound to a specific instance of the outer type, not to the outer type itself.
    */
  sealed trait LeadIQField {
    type Value
  }

  case class CustomMapping private (
      private val m: Map[LeadIQField, Any] = Map.empty[LeadIQField, Any]
  ) {

    def get(key: LeadIQField): Option[key.Value] =
      m.get(key).map(_.asInstanceOf[key.Value])

    def put(key: LeadIQField)(value: key.Value): CustomMapping =
      CustomMapping(m.+((key, value)))
  }

  object CustomMapping {
    def empty: CustomMapping = new CustomMapping()
  }

  def main(args: Array[String]): Unit = {}
}
