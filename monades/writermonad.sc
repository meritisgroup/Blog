case class Writer[A](value:A, log:String=""){
  def map[B](f: A => B): Writer[B] = Writer(f(value), log)
  def flatMap[B](f: A => Writer[B]) : Writer[B] = {
    f(value) match {
      case Writer(fValue, fLog) => Writer(fValue, log + fLog)
    }
  }
}

def multiply(n:Int)(value:Int) = {
  Writer(value*n,  s"multiplied by $n")
}

Writer(5).map(_*3).flatMap(multiply(3))
