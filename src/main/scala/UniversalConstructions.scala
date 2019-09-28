object UniversalConstructions {
  /*

  A type is given meaning based on it's relationship to other types
  This object interacts with the rest of the universe in this particular way

   */

  /*

  product = (A, B)
  p1 = _._1
  p2 = _._2

  With product we have mapping in, anybody who claims to have a function to A and B, will have a function to product
  of A and B

             C
            /| \
           / |h \
          /  ↓   \
       f / (A, B) \ g
        /  /    \  \
       /  /p1  p2\  \
       ↓ ↓       ↓  ↓
        A          B

  functions f and g are factorised

  f = p1 . h
  g = p2 . h

  isomorphism

  h : C => (A, B) = c => (f(c), g(c))

  f: C => A = h andThen p1
  g: C => B = h andThen p2

  (f, g) <==> h

   */

  def fanOut[A, B, C](f: C => A, g: C => B): C => (A, B) =
    c => (f(c), g(c))

  def fanIn[A, B, C](h: C => (A, B)): (C => A, C => B) =
    (h(_)._1, h(_)._2)

  /*
  BiFunctor, lifting two functions at the same time

  Here, ((A, B)) maps into product (A1, B1), it's like type C
  wherein f = (_._1) andThen x
  and     g = (_._2) andThen y

  So all we need do is fanOut.
   */
  def biMap[A, A1, B, B1](x: A => A1, y: B => B1): ((A, B)) => (A1, B1) =
    fanOut(x compose (_._1), y compose (_._2))

  /*

  sum = Either[A, B]

  l: A => Either[A, B] = Left(_)
  r: B => Either[A, B] = Right(_)

  With Sum we have a unique mapping out, anybody who claims to have a function from A and from B, will have a
  function from sum of A and B

          A           B
         / \         / \
         \  \ l   r /  |
          \  ↓     ↓   /
        f  \ (A + B)  / g
            \   | h  /
             \  |   /
              \ |  /
              ↓ ↓ ↓
                C

   f = h . l
   g = h . r

   h : A + B => C

   f: A => C = l andThen h
   g: B => C = r andThen h

   (f, g) <==> h

   Anytime we want to define a function from Sum, it's enough to define function from the individual types, a difficult
   problem splits into two simpler ones.

   Case statements are a result of these universal constructions.
   */
  def either[A, B, C](f: A => C, g: B => C): Either[A, B] => C = {
    case Left(a)  => f(a)
    case Right(b) => g(b)
  } // _.fold(f, g)

  def choice[A, B, C](h: Either[A, B] => C): (A => C, B => C) =
    (a => h(Left(a)), b => h(Right(b)))

  /*

  (A + B) * C <==> A * C + B * C, Proving this for types

  disRight
  (A + B) * C <== A * C + B * C

  f: A * C => (A + B) * C
  g: B * C => (A + B) * C

  Going from product to product is biMap.

  l: A * C => A * C + B * C
  r: B * C => A * C + B * C

  h: A * C + B * C => (A + B) * C
   */
  def disRight[A, B, C]: Either[(A, C), (B, C)] => (Either[A, B], C) =
    either(biMap(Left(_), identity), biMap(Right(_), identity))

  def disLeft[A, B, C]: ((Either[A, B], C)) => Either[(A, C), (B, C)] = {
    uncurry(either(curry(Left(_)), curry(Right(_))))
  }

  def curry[A, B, C](f: ((A, B)) => C): A => B => C =
    a => b => f((a, b))

  def uncurry[A, B, C](f: A => B => C): ((A, B)) => C =
    ab => f(ab._1)(ab._2)

}
