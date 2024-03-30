package homework2

import scala.annotation.tailrec
import Function.tupled

sealed trait LazyList[+A]:
  def head: A
  def tail: LazyList[A]

  def isEmpty: Boolean

  def take(n: Int): LazyList[A] = ???

  def map[B](f: A => B): LazyList[B] = ???

  def zip[B](that: LazyList[B]): LazyList[(A, B)] = ???

  def toList: List[A] = ???

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

  val factorials: LazyList[Long] = ???
  val firstTenFactorials = factorials.take(10).toList

  @main def main =
    println(s"First ten fibs are: $firstTenFibs")

    println(s"First ten factorials are: $firstTenFactorials")
