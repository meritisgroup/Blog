object Monad {
  /**
    * Turns a value into a Monad value
    * @param x the value
    * @return a Monad of type [A]
    */
  def unit[A](x: A) = List(x)

  /**
    * Allows a Monad function to be chained with other Monad functions
    * @param f a function that returns a monad of type [B]
    * @return a function that takes a Monad of type [A] and return a Monad of type [B]
    */
  def bind[A](f : A => List[A]) : List[A] => List[A] = {
    (value: List[A]) => {
      value.map(x => f(x)).flatten
    }
  }

  /**
    * Chain two functions together
    * @param g call on value
    * @param f call on (g(value))
    * @return f(g(value))
    */
  def compose[A, B, C](g: A => B, f: B => C) = (value: A) => f(g(value))
}

import Monad._

def replicate(n:Int)(str:String): List[String] = List.fill(n)(str)
val bound = bind(replicate(3) _)

compose(compose(bound,bound), bound)(unit("foo")) // Liste de 27 "foo"'s !
