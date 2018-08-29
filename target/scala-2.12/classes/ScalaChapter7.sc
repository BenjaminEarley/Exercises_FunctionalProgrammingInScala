object unsoundStatic {
  trait LowerBound[T] {
    type M >: T;
  }
  trait UpperBound[U] {
    type M <: U;
  }
  object Coerce {
    val bounded : LowerBound[Int] with UpperBound[String] = Coerce.bounded
  }

  def upcast[T](ub : LowerBound[T], t : T) : ub.M = t

  def main(args : Array[String]) : Unit = {
    val zero : String = upcast(Coerce.bounded, 0)
    println("...")
  }
}