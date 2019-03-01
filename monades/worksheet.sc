object Monad {
  /**
    * Turns a value into a Monad value
    * @param x the value
    * @return a Monad of type [A]
    */
  def unit[A](x: A) = (x, "")

  /**
    * Turns a function into a Monad function
    * @param f a function that takes A and returns B
    * @return a function that returns a Monad of type [B]
    */
  def lift[A, B](f: A => B): A => (B, String) = (value: A) => unit(f(value))

  /**
    * Allow a Monad function to be chained with other Monad functions
    * @param f a function that returns a monad of type [B]
    * @return a function that takes a Monad of type [A] and return a Monad of type [B]
    */
  def bind[A, B](f: A => (B, String)): ((A, String)) => (B, String) = {
    (value: (A, String)) => {
      val (newVal, log) = f(value._1)
      (newVal, log + value._2)
    }
  }

  /**
    * Chain two functions together
    * @param g call on value
    * @param f call on (g(value))
    * @return
    */
  def compose[A, B, C](g: A => B, f: B => C) = (value: A) => f(g(value))
}

// we define some functions that will compose our chain, some of them need to be debuggable

def add2(value:Int) = value+2
def intToString(value:Int) = (value.toString, "intToString called. ")
def stringToInt(value:String) = value.toInt
def add5(value: Int) = (value+5, "add5 called. ")
def identityDebug(value:Int) = (value, "identityDebug called. ")

import Monad._

// we bind the functions to make them composable
// the functions that do not return a log are lifted as well to return an empty log
val add2_ = bind(lift(add2))
val intToString_ = bind(intToString)
val stringToInt_ = bind(lift(stringToInt))
val add5_ = bind(add5)
val identityDebug_ = bind(identityDebug)

val f = compose(unit[Int], compose(compose(compose(compose(add2_, intToString_), stringToInt_), add5_), identityDebug_))

f(3) // (10,"identityDebug called. add5 called. intToString called. ")

