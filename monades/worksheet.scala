case class Monad[A]() {
  type Monadic = (A, String)

  def unit(x: A) = (x, "")

  def lift[B](f: B => A): B => Monadic = (value: B) => unit(f(value))

  def bind(f: A => Monadic): Monadic => Monadic = {
    (value: Monadic) => {
      val (newVal, log) = f(value._1)
      (newVal, log + value._2)
    }
  }

  def compose[B, C](f: C => Monadic, g: B => C) = (value: B) => f(g(value))
}

def add2(x: Int) = x + 2

// we create our monad
val monad = Monad[Int]()

// we make our function return a monad
val lifted = monad.lift(add2)

// we make this new functions take a monad as input
val bound = monad.bind(lifted)

// we compose the two functions
val f = monad.compose(bound, bound)

// now we could call it this way
f(monad.unit(3)) // returns 7

// or even this way, since unit is also composable
monad.compose(f, monad.unit)(3) // returns 7
