package homework2

import scala.annotation.tailrec
import Function.tupled

sealed trait LazyList[+A]:
  def head: A
  def tail: LazyList[A]

  def isEmpty: Boolean

  def take(n: Int): LazyList[A] =
    if n <= 0 then LazyNil
    else LazyCons(head, tail.take(n - 1))

  def map[B](f: A => B): LazyList[B] =
    if isEmpty then LazyNil
    else LazyCons(f(head), tail.map(f))

  infix def zip[B](that: LazyList[B]): LazyList[(A, B)] =
    if this.isEmpty || that.isEmpty then LazyNil
    else LazyCons((head, that.head), tail zip that.tail)

  def toList: List[A] =
    @tailrec
    def loop(as: LazyList[A], acc: List[A]): List[A] =
      if as.isEmpty then acc.reverse
      else loop(as.tail, as.head :: acc)

    loop(this, List.empty)

class LazyCons[+A](h: => A, t: => LazyList[A]) extends LazyList[A]:
  lazy val head: A = h
  lazy val tail: LazyList[A] = t

  def isEmpty: Boolean = false

object LazyNil extends LazyList[Nothing]:
  def head: Nothing = throw new NoSuchElementException
  def tail: LazyList[Nothing] = throw new UnsupportedOperationException

  def isEmpty: Boolean = true

object LazyCons:
  def apply[A](h: => A, t: => LazyList[A]): LazyCons[A] = new LazyCons[A](h, t)

object LazyList:
  def from(start: Long, step: Int = 1): LazyList[Long] = LazyCons(start, from(start + step, step))
  val naturalNumbers: LazyList[Long] = from(0L)

  extension [A](el: => A)
    // If we define #:: on the LazyList itself it won't work because LazyList instances would be passed eagerly.
    // Extension methods allows us to pass them by name and delay their evaluation
    def #::(list: => LazyList[A]): LazyList[A] = LazyCons(el, list)

object LazyListExamples:
  // Can you make these work?
  val fibs: LazyList[Long] = 0L #:: 1L #:: (fibs zip fibs.tail).map(_ + _)
  val firstTenFibs = fibs.take(10).toList

  val factorials: LazyList[Long] = 1L #:: (factorials zip LazyList.from(1)).map(_ * _)
  val firstTenFactorials = factorials.take(10).toList

  @main def main =
    println(s"First ten fibs are: $firstTenFibs")

    println(s"First ten factorials are: $firstTenFactorials")
