object unsound {
  trait A { type L >: Any}
  def upcast(a: A, x: Any): a.L = x
  val p: A { type L <: Nothing } = null
  def coerce(x: Any): Nothing = upcast(p, x)
  coerce("Uh oh!")
}
