package homework2

def quickSort[A](as: List[A])(isSmaller: (A, A) => Boolean): List[A] = as match
  case Nil => Nil
  case a :: rest =>
    val (smaller, bigger) = rest.partition(isSmaller(_, a))

    quickSort(smaller)(isSmaller) ::: (a :: quickSort(bigger)(isSmaller))

def mergeSort[A](as: List[A])(isSmaller: (A, A) => Boolean): List[A] = ???
