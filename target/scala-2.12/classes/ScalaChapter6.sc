import scala.collection.mutable

val rng = new scala.util.Random

rng.nextDouble

rng.nextDouble

rng.nextInt

rng.nextInt(10)

trait RNG {
  def nextInt: (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }

}

val rng0 = SimpleRNG(42)
val (n1, rng1) = rng0.nextInt
val (n2, rng2) = rng1.nextInt

val rng00 = SimpleRNG(42)
val (n11, rng11) = rng00.nextInt
val (n22, rng22) = rng11.nextInt

assert(n1 == n11)
assert(n2 == n22)
assert(rng1 == rng11)
assert(rng2 == rng22)

def nonNegativeInt(rng: RNG): (Int, RNG) = {
  val (value, newRng) = rng.nextInt
  (if (value < 0) -(value + 1) else value, newRng)
}

def double(rng: RNG): (Double, RNG) = {
  val (value, newRng) = nonNegativeInt(rng)
  (value / (Int.MaxValue.toDouble + 1), newRng)
}

def intDouble(rng: RNG): ((Int, Double), RNG) = {
  val (i, r1) = rng.nextInt
  val (d, r2) = double(r1)
  ((i, d), r2)
}

def doubleInt(rng: RNG): ((Double, Int), RNG) = {
  val (pair, r) = intDouble(rng)
  (pair.swap, r)
}

def double3(rng: RNG): ((Double, Double, Double), RNG) = {
  val (value1, rng1) = double(rng)
  val (value2, rng2) = double(rng1)
  val (value3, newRng) = double(rng2)
  ((value1, value2, value3), newRng)
}

def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
  val ints = mutable.MutableList[Int]()
  var currentRng = rng
  for (_ <- 0 until count) {
    val (value, newRng) = rng.nextInt
    ints += value
    currentRng = newRng
  }
  (ints.toList, currentRng)
}

// A recursive solution
def ints2(count: Int)(rng: RNG): (List[Int], RNG) =
  if (count <= 0)
    (List(), rng)
  else {
    val (x, r1) = rng.nextInt
    val (xs, r2) = ints(count - 1)(r1)
    (x :: xs, r2)
  }

// A tail-recursive solution
def ints3(count: Int)(rng: RNG): (List[Int], RNG) = {
  def go(count: Int, r: RNG, xs: List[Int]): (List[Int], RNG) =
    if (count <= 0)
      (xs, r)
    else {
      val (x, r2) = r.nextInt
      go(count - 1, r2, x :: xs)
    }

  go(count, rng, List())
}

type Rand[+A] = RNG => (A, RNG)

val int: Rand[Int] = _.nextInt

def unit[A](a: A): Rand[A] = rng => (a, rng)

def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
  rng => {
    val (a, rng2) = s(rng)
    (f(a), rng2)
  }

def nonNegativeEven: Rand[Int] =
  map(nonNegativeInt)(i => i - 1 % 2)

def double2: Rand[Double] = {
  map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))
}

def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
  rng => {
    val (a, r1) = ra(rng)
    val (b, r2) = rb(r1)
    (f(a, b), r2)
  }

def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
  map2(ra, rb)((_, _))

val randIntDouble2: Rand[(Int, Double)] =
  both(int, double)

val randDoubleInt2: Rand[(Double, Int)] =
  both(double, int)

// In `sequence`, the base case of the fold is a `unit` action that returns
// the empty list. At each step in the fold, we accumulate in `acc`
// and `f` is the current element in the list.
// `map2(f, acc)(_ :: _)` results in a value of type `Rand[List[A]]`
// We map over that to prepend (cons) the element onto the accumulated list.
//
// We are using `foldRight`. If we used `foldLeft` then the values in the
// resulting list would appear in reverse order. It would be arguably better
// to use `foldLeft` followed by `reverse`. What do you think?
def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
fs.foldRight(unit(List[A]()))((f, acc) => map2(f, acc)(_ :: _))

// It's interesting that we never actually need to talk about the `RNG` value
// in `sequence`. This is a strong hint that we could make this function
// polymorphic in that type.

def _ints(count: Int): Rand[List[Int]] =
  sequence(List.fill(count)(int))

def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
  rng => {
    val (a, newRng) = f(rng)
    g(a)(newRng)
  }

def mapf[A, B](s: Rand[A])(f: A => B): Rand[B] =
  flatMap(s)(a => unit(f(a)))

def map2f[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
  flatMap(ra)(a => map(rb)(b => f(a, b)))