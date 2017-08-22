def partial1[A, B, C](a: A, f: (A, B) => C): B => C =
  (b) => f(a, b)

val add = partial1(1, (a: Int, b: Int) => a + b)
val add2 = add(1)

def curry[A, B, C](f: (A, B) => C): A => B => C =
  (a: A) => (b: B) => f(a, b)

def partial1WithCurry[A, B, C](a: A, f: (A, B) => C): B => C =
  curry(f)(a)

val add3 = partial1WithCurry(1, (a: Int, b: Int) => a + b)
val add4 = add3(1)

def uncurry[A, B, C](f: A => B => C): (A, B) => C =
  (a: A, b: B) => f(a)(b)

def compose[A, B, C](f: B => C, g: A => B): A => C =
  (a: A) => f(g(a))

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(x, xs) => x * product(xs)
  }

  def tail[A](xs: List[A]): List[A] = xs match {
    case Nil => throw new IllegalArgumentException()
    case Cons(_, ys) => ys
  }

  def setHead[A](x: A, xs: List[A]): List[A] = xs match {
    case Nil => throw new IllegalArgumentException()
    case Cons(_, ys) => Cons(x, ys)
  }

  def drop[A](l: List[A], n: Int): List[A] = l match {
    case Cons(_, xs) if n > 0 => drop(xs, n - 1)
    case _ => l
  }

  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Cons(x, xs) if f(x) => dropWhile(xs)(f)
    case _ => l
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => throw new IllegalArgumentException()
    case Cons(_, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  def sumRight(ns: List[Int]): Int =
    foldRight(ns, 0)(_ + _)

  def productRight(ds: List[Double]): Double =
    foldRight(ds, 1.0)(_ * _)

  def lengthRight[A](as: List[A]): Int =
    foldRight(as, 0)((_, y) => y + 1)

  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  def sumLeft(ns: List[Int]): Int =
    foldLeft(ns, 0)(_ + _)

  def productLeft(ds: List[Double]): Double =
    foldLeft(ds, 1.0)(_ * _)

  def lengthLeft[A](as: List[A]): Int =
    foldLeft(as, 0)((x, _) => x + 1)

  def reverse[A](as: List[A]): List[A] =
    foldLeft(as, Nil: List[A])((x, y) => Cons(y, x))

  def append[A](as: List[A], bs: List[A]): List[A] =
    foldRight(as, bs)(Cons(_, _))

  def copy[A](as: List[A]): List[A] =
    append(as, Nil: List[A])

  def map[A, B](as: List[A])(f: A => B): List[B] =
    List.foldRight(as, Nil: List[B])((x, y) => Cons(f(x), y))

  def flatten[A](as: List[List[A]]): List[A] =
    foldRight(as, Nil: List[A])(append)

  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    List.foldRight(as, Nil: List[A])((x, y) => if (f(x)) Cons(x, y) else y)

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
    foldRight(as, Nil: List[B])((x, y) => append(f(x), y))

  def filterWithFlatMap[A](as: List[A])(f: A => Boolean): List[A] =
    List.flatMap(as)((x) => if (f(x)) List(x) else List())

  def zip[A,B](as: List[A], bs: List[B]): List[(A,B)] =
    List.reverse(foldLeft(as, (Nil:List[(A,B)], bs)) { case ((c, Cons(b, bss)), a) =>
      (Cons((a, b), c), bss)
    }._1)

  def zipWith[A,B,C](as: List[A], bs: List[B])(f: (A,B) => C): List[C] =
    map(zip(as, bs)) { case (x,y) => f(x,y) }

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = sup match {
    case Nil => sup == sub
    case Cons(_,as) =>
      def compare(sup: List[A], sub: List[A]): Boolean = sub match {
        case Nil => true
        case Cons(b,bs) => sup match {
          case Nil => false
          case Cons(a, ass) => a == b && compare(ass, bs)
        }
      }
      compare(sup, sub) || hasSubsequence(as, sub)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
}

val list = List(1, 2, 3, 4, 5)
val doubleList = List(1.0, 2.0, 3.0, 4.0, 5.0)
val listOfLists = List(List(1, 2, 3), List(10, 20, 30), List(100, 200, 300))
val sum = List.sum(list)
val product = List.product(doubleList)
val tail = List.tail(list)
val setHead = List.setHead(0, list)
val drop = List.drop(list, 2)
val dropWhile = List.dropWhile(list) { _ < 4 }
val init = List.init(list)
val sumRight = List.sumRight(list)
val productRight = List.productRight(doubleList)
val copy = List.copy(list)
val lengthRight = List.lengthRight(list)
val sumLeft = List.sumLeft(list)
val productLeft = List.productLeft(doubleList)
val lengthLeft = List.lengthLeft(list)
val reverse = List.reverse(list)
val append = List.append(list, list)
val flatten = List.flatten(listOfLists)
val map = List.map(list) { _ + 1 }
val filterEven = List.filter(list) { _ % 2 == 0 }
val filterOdd = List.filter(list) { _ % 2 != 0 }
val flatMap = List.flatMap(list) { x => Cons(x + 1, Nil) }
val flatMapFlatten = List.flatMap(listOfLists) { x => x }
val flatMapDoubleLength = List.flatMap(list) { x => List(x, x) }
val filterEvenWithFlatMap = List.filterWithFlatMap(list) { _ % 2 == 0 }
val filterOddWithFlatMap = List.filterWithFlatMap(list) { _ % 2 != 0 }
val zip = List.zip(list, list)
val zipWith = List.zipWith(list, list) { _ + _ }
val hasSubsequenceIsTrue = List.hasSubsequence(list, List(2,3))
val hasSubsequenceIsFalse = List.hasSubsequence(list, List(5,6))

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
case object TreeNil extends Tree[Nothing]

object Tree {
  def size[A](tree: Tree[A]): Int = {
    def helper(tree: Tree[A]): Int = {
      tree match {
        case Leaf(_) => 1
        case Branch(left, right) =>
          helper(left) + helper(right) + 1
      }
    }
    helper(tree)
  }

  def maximum[A](tree: Tree[A])(f: (A, A) => A): A = {
    def helper(tree: Tree[A]): A = {
      tree match {
        case Leaf(value) => value
        case Branch(left, right) =>
          f(helper(left), helper(right))
      }
    }
    helper(tree)
  }

  def depth[A](tree: Tree[A]): Int = {
    def helper(tree: Tree[A]): Int = {
      tree match {
        case Leaf(_) => 1
        case Branch(left, right) => (helper(left) max helper(right)) + 1
      }
    }
    helper(tree)
  }

  def map[A,B](tree: Tree[A])(f: A => B): Tree[B] = {
    def helper(tree: Tree[A]): Tree[B] = {
      tree match {
        case Leaf(value) => Leaf(f(value))
        case Branch(left, right) =>
          Branch(helper(left), helper(right))
      }
    }
    helper(tree)
  }
}

val tree = Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4)))
val treeSize = Tree.size(tree)
val treeMax = Tree.maximum(tree) { (x,y) => if (x > y) x else y }
val treeDepth = Tree.depth(tree)
val treeMap = Tree.map(tree) { _ + 1 }

