import Stream._

sealed trait Stream[+A] {
  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => cons(h(), empty)
    case _ => empty
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    case _ => empty
  }

  def foldLeft[B](z: B)(f: (B, A) => B): B =
    this match {
      case Cons(h, t) => t().foldLeft(f(z, h()))(f)
      case _ => z
    }

  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] =
    foldRight((z, Stream(z)))((a, p0) => {
      // p0 is passed by-name and used in by-name args in f and cons. So use lazy val to ensure only one evaluation...
      lazy val p1 = p0
      val b2 = f(a, p1._1)
      (b2, cons(b2, p1._2))
    })._2

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  def takeWhile_1(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((a, b) =>
      if (p(a)) cons(a, b)
      else empty
    )

  def map[B](p: A => B): Stream[B] =
    foldRight(empty[B])((a, b) =>
      cons(p(a), b)
    )

  def filter(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((a, b) =>
      if (p(a)) cons(a, b)
      else b
    )

  def append[B >: A](p: => Stream[B]): Stream[B] =
    foldRight(p)((a, b) =>
      cons(a, b)
    )

  def flatMap[B](p: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((a, b) =>
      p(a).append(b)
    )

  def head(): A = this match {
    case Cons(h, _) => h()
    case _ => throw new NullPointerException
  }

  def tail(): Stream[A] = this match {
    case Cons(_, t) => t()
    case _ => empty
  }
}

case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, t1: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = t1
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
}


val stream = Stream(1, 2, 3, 4, 5, 6, 7, 8, 9)
val apply = stream.toList
val take = stream.take(2).toList
val takeWhile = stream.takeWhile {
  _ < 3
}.toList
val map = stream.map(_ + 5).toList
val flatMap = stream.flatMap({ a => Stream(a, a) }).toList


val fib: Stream[BigInt] = {
  def next(fib: Stream[BigInt]): Stream[BigInt] = cons(fib.head + fib.tail().head, next(fib.tail()))

  cons(0, cons(1, next(fib)))
}

val fibs = {
  def go(f0: Int, f1: Int): Stream[Int] =
    cons(f0, go(f1, f0 + f1))

  go(0, 1)
}

fib.tail().tail().tail().tail().tail().tail().tail().tail().head()

stream.exists({ a =>
  println(a)
  a == 5
})

val ones: Stream[Int] = Stream.cons(1, ones)
ones.takeWhile(_ == 1)
ones.forAll(_ != 1)

def constant[A](a: A): Stream[A] =
  Stream.cons(a, Stream.apply(a))

def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
  f(z) match {
    case Some((h, s)) => Stream.cons(h, unfold(s)(f))
    case None => Stream.empty
  }

val fibs_1 = unfold((0, 1))({ z => Some((z._1, (z._2, z._1 + z._2))) })

def constant_1[A](a: A): Stream[A] =
  unfold(a)(s => Some((a, s)))

constant_1(1).take(10).toList

def map_1[A, B](str: Stream[A])(p: A => B): Stream[B] =
  unfold(str) {
    case Cons(h, t) => Some((p(h()), t()))
    case Empty => None
  }

def take_1[A](str: Stream[A])(n: Int): Stream[A] =
  unfold((str, n)) {
    case (Cons(h, t), nn) if nn > 0 => Some((h(), (t(), nn - 1)))
    case _ => None
  }

def takeWhile_2[A](str: Stream[A])(p: A => Boolean): Stream[A] =
  unfold(str) {
    case Cons(h, t) if p(h()) => Some((h(), t()))
    case _ => None
  }

def zip[A, B](strA: Stream[A], strB: Stream[B]): Stream[(A, B)] =
  unfold((strA, strB)) {
    case ((Cons(h, t), Cons(hh, tt))) => Some(((h(), hh()), (t(), tt())))
    case _ => None
  }

def zipWith[A, B, C](strA: Stream[A], strB: Stream[B])(f: (A, B) => C): Stream[C] =
  map_1(zip(strA, strB)) { case (x, y) => f(x, y) }

def zipAll[A, B](strA: Stream[A], strB: Stream[B]): Stream[(Option[A], Option[B])] =
  unfold((strA, strB)) {
    case (Cons(h, t), Cons(hh, tt)) => Some(((Some(h()), Some(hh())), (t(), tt())))
    case (Cons(h, t), Empty) => Some(((Some(h()), None), (t(), Stream.empty)))
    case (Empty, Cons(hh, tt)) => Some(((None, Some(hh())), (Stream.empty, tt())))
    case _ => None
  }

val map1 = map_1(stream) { n => n + n }.toList
val take1 = take_1(fibs_1)(5).toList
val takeWhile1 = takeWhile_2(fibs_1) {
  _ < 100
}.toList
val zipWith1 = zipWith[Int, Int, Int](stream, stream) {
  _ + _
}.toList
val zipAll1 = zipAll(stream, stream.append(stream)).toList

def startsWith[A, B](s: Stream[A], ss: Stream[B]): Boolean = {
  val i = zipAll(s, ss)
  i.foldRight(true) { (x, y) =>
    x match {
      case (_, None) => true
      case (a, b) if a == b => y
      case _ => false
    }
  }
}

//Book Answer
def startsWith_1[A, B](s: Stream[A], ss: Stream[B]): Boolean =
  zipAll(s, ss).takeWhile(_._2.isDefined) forAll {
    case (h, h2) => h == h2
  }

val foo = startsWith(fibs, fibs.take(10).map(_ * 2))
val bar = startsWith(fibs, fibs.take(10))

def tails[A](s: Stream[A]): Stream[Stream[A]] =
  unfold(s) {
    case Cons(_, t) => Some(s, t())
    case _ => None
  } append Stream(empty)







