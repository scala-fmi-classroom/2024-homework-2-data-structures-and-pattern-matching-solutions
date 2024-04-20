package homework2

import scala.annotation.tailrec

def quickSort[A](as: List[A])(isSmaller: (A, A) => Boolean): List[A] = as match
  case Nil => Nil
  case a :: rest =>
    val (smaller, bigger) = rest.partition(isSmaller(_, a))

    quickSort(smaller)(isSmaller) ::: (a :: quickSort(bigger)(isSmaller))

def mergeSort[A](as: List[A])(isSmaller: (A, A) => Boolean): List[A] =
  def merge(xs: List[A], ys: List[A]): List[A] = (xs, ys) match
    case (Nil, _) => ys
    case (_, Nil) => xs
    case (x :: xrest, y :: yrest) =>
      if isSmaller(x, y) then x :: merge(xrest, ys)
      else y :: merge(xs, yrest)

  if as.isEmpty || as.tail.isEmpty then as
  else
    val (left, right) = as.splitAt(as.size / 2)

    merge(
      mergeSort(left)(isSmaller),
      mergeSort(right)(isSmaller)
    )

def mergeSortWithTailRecMerge[A](as: List[A])(isSmaller: (A, A) => Boolean): List[A] =
  def merge(xs: List[A], ys: List[A]): List[A] =
    @tailrec
    def loop(xs: List[A], ys: List[A], acc: List[A]): List[A] = (xs, ys) match
      case (Nil, Nil) => acc.reverse
      case (x :: xrest, Nil) => loop(xrest, Nil, x :: acc)
      case (Nil, y :: yrest) => loop(Nil, yrest, y :: acc)
      case (x :: xrest, y :: yrest) =>
        if isSmaller(x, y) then loop(xrest, ys, x :: acc)
        else loop(xs, yrest, y :: acc)

    loop(xs, ys, List.empty)

  if as.isEmpty || as.tail.isEmpty then as
  else
    val middlePointIndex = as.size / 2
    val (left, right) = as.splitAt(middlePointIndex)

    merge(
      mergeSortWithTailRecMerge(left)(isSmaller),
      mergeSortWithTailRecMerge(right)(isSmaller)
    )
