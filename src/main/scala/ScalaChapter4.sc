import ScalaChapter4._

object ScalaChapter4 {

  sealed trait Option[+A] {
    def map[B](f: A => B): Option[B] = this match {
      case Some(value) => Some(f(value))
      case None => None
    }

    def flatMap[B](f: A => Option[B]): Option[B] = this match {
      case Some(value) => f(value)
      case None => None
    }

    def getOrElse[B >: A](default: => B): B = this match {
      case Some(value) => value
      case None => default
    }

    def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
      case Some(value) => Some(value)
      case None => ob
    }

    def filter(f: A => Boolean): Option[A] = this match {
      case Some(value) => if (f(value)) Some(value) else None
      case None => None
    }
  }

  case class Some[+A](get: A) extends Option[A]
  case object None extends Option[Nothing]
}

val some = Some(1)
val none = None
val map = some.map(_*2)
val flatMap = some.flatMap { x => Some(x*2) }
val getOrElseSome = some.getOrElse(0)
val getOrElseNone = none.getOrElse(0)
val orElseSome = some.orElse(Some(0))
val orElseNone = none.orElse(Some(0))
val filterTrue = some.filter(_ == 1)
val filterFalse = some.filter(_ != 1)
