val fib:Stream[BigInt] = {
  def next(fib: Stream[BigInt]): Stream[BigInt] = (fib.head + fib.tail.head) #:: next(fib.tail)
  0 #:: 1 #:: next(fib)
}
fib.drop(10).head