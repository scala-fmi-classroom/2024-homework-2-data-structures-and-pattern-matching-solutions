package homework2

import scala.annotation.tailrec

enum Chain[+A]:
  case Singleton(a: A)
  case Append(left: Chain[A], right: Chain[A])

  // methods need to be final if we need them to be tailrec
  @tailrec
  final def head: A = this match
    case Singleton(a) => a
    case Append(left, _) => left.head

  def tail: Chain[A] = this match
    case Append(Singleton(_), right) => right
    case c @ Append(_, _) => c.listify.tail
    case Singleton(_) => throw new UnsupportedOperationException

  def isEmpty: Boolean = false

  def +:[B >: A](front: B): Chain[B] = Singleton(front) ++ this

  def :+[B >: A](back: B): Chain[B] = this ++ Singleton(back)

  def ++[B >: A](right: Chain[B]): Chain[B] = Append(this, right)

  @tailrec
  final def foldLeft[B](initial: B)(f: (B, A) => B): B = this match
    case Singleton(a) => f(initial, a)
    case Append(Singleton(a), right) => right.foldLeft(f(initial, a))(f)
    case Append(Append(a, b), c) => Append(a, Append(b, c)).foldLeft(initial)(f)

  // an alternative implementation
  def foldLeftAlternative[B](initial: B)(f: (B, A) => B): B =
    @tailrec
    def loop(chain: Chain[A], rest: List[Chain[A]], acc: B): B = chain match
      case Singleton(a) =>
        val newAcc = f(acc, a)

        if rest.isEmpty then newAcc
        else loop(rest.head, rest.tail, newAcc)
      case Append(left, right) =>
        loop(left, right :: rest, acc)

    loop(this, List.empty, initial)

  def map[B](f: A => B): Chain[B] = flatMap(a => Singleton(f(a)))

  def flatMap[B](f: A => Chain[B]): Chain[B] =
    toList.map(f).reduceRight((acc, next) => acc ++ next)

  def listify: Chain[A] = this match
    case Singleton(_) => this
    case Append(left @ Singleton(_), right) => left ++ right.listify
    case Append(Append(lleft, lright), right) => (lleft ++ (lright ++ right)).listify

  def foreach(f: A => Unit): Unit = foldLeft(())((_, next) => f(next))

  override def equals(that: Any): Boolean = that match
    case c: Chain[?] => this.toList == c.toList
    case _ => false

  override def hashCode: Int = foldLeft(0)(_ * 31 + _.hashCode)

  override def toString: String = toList.mkString("Chain(", ",", ")")

  def toList: List[A] = foldLeft(List.empty[A])((acc, next) => next :: acc).reverse
  def toSet[B >: A]: Set[B] = foldLeft(Set.empty[B])((acc, next) => acc + next)

object Chain:
  def apply[A](head: A, rest: A*): Chain[A] =
    (head +: rest).map(Singleton(_)).reduceRight((next, acc) => next ++ acc)
//    rest.foldLeft(Singleton(head))((acc, next) => acc ++ Singleton(next))

  // Allows Chain to be used in pattern matching
  //
  // As an alternative implementation we can make Chain[A] implement Seq[A] and return it directly,
  // but that requires implementing a couple of more operations which are related to the way
  // Scala collections operate behind the scenes
  def unapplySeq[A](chain: Chain[A]): Option[Seq[A]] = Some(chain.toList)
