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

