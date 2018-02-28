package ninetynineproblems

import java.util.NoSuchElementException

import scala.annotation.tailrec

object Level1_10 {

  //  P01 (*) Find the last element of a list.
  //  Example:
  //    scala> lastListElement(List(1, 1, 2, 3, 5, 8))
  //  res0: Int = 8
  @tailrec
  def lastListElement[A](ls: List[A]): A = {
    ls match {
      case x :: Nil => x
      case x :: xs => lastListElement(xs)
      case _ => throw new NoSuchElementException("Bad input")
    }
  }

  //  P02 (*) Find the last but one element of a list.
  //  Example:
  //    scala> penultimate(List(1, 1, 2, 3, 5, 8))
  //  res0: Int = 5
  @tailrec
  def penultimate[A](ls: List[A]): A = {
    ls match {
      case x :: y :: Nil => x
      case x :: Nil => throw new NoSuchElementException("Bad input, list with only one element")
      case x :: xs => penultimate(xs)
      case _ => throw new NoSuchElementException("Bad input")
    }
  }

  //  P03 (*) Find the Kth element of a list.
  //  By convention, the first element in the list is element 0.
  //  Example:
  //
  //    scala> nth(2, List(1, 1, 2, 3, 5, 8))
  //  res0: Int = 2
  def nth[A](pos: Int, ls: List[A]): A = {
    if (pos < 0) throw new NoSuchElementException("Element must be greater or equals to 0")

    def nth2[A](l: List[A]): A = {
      l match {
        case x :: Nil if (pos == ls.length) => x
        case x :: Nil if (pos > ls.length) => throw new NoSuchElementException("List too short")
        case x :: xs if (xs.length + pos + 1 == ls.length) => x
        case x :: xs if (xs.length + pos + 1 > ls.length) => nth2(xs)
        case Nil => throw new NoSuchElementException("List too short")
      }
    }

    nth2(ls)
  }

  //  P04 (*) Find the number of elements of a list.
  //    Example:
  //    scala> length(List(1, 1, 2, 3, 5, 8))
  //  res0: Int = 6
  def lengthList[A](ls: List[A]): Int = {
    ls match {
      case x :: xs => 1 + lengthList(xs)
      case x :: Nil => 1
      case Nil => 0
    }
  }

  //  P05 (*) Reverse a list.
  //  Example:
  //    scala> reverse(List(1, 1, 2, 3, 5, 8))
  //  res0: List[Int] = List(8, 5, 3, 2, 1, 1)
  def reverse[A](ls: List[A]): List[A] = {
    ls match {
      case x :: xs => reverse(xs) ::: List(x)
      case x :: Nil => List(x)
      case Nil => Nil
    }
  }

  //  P06 (*) Find out whether a list is a palindrome.
  //  Example:
  //    scala> isPalindrome(List(1, 2, 3, 2, 1))
  //  res0: Boolean = true
  def isPalindrome[A](ls: List[A]): Boolean =
  ls == reverse(ls)

  //  P07 (**) Flatten a nested list structure.
  //  Example:
  //    scala> flatten(List(List(1, 1), 2, List(3, List(5, 8))))
  //  res0: List[Any] = List(1, 1, 2, 3, 5, 8)
  def flatten[A](elems: List[A]): List[A] = {
    elems match {
      case (x: List[A]) :: xs => flatten(x) ::: flatten(xs)
      case (x: A) :: xs => List(x) ::: flatten(xs)
      case Nil => Nil
    }
  }

  // P08 (**) Eliminate consecutive duplicates of list elements.
  // If a list contains repeated elements they should be replaced with a single copy of the element.
  // The order of the elements should not be changed.
  // Example:
  //   scala> compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
  // res0: List[Symbol] = List('a, 'b, 'c, 'a, 'd, 'e)
  def removeConsecutiveDuplicates[A](ls: List[A]): List[A] = {
    ls match {
      case Nil => Nil
      case x :: Nil => List(x)
      case x :: xs if (xs.head.equals(x)) => removeConsecutiveDuplicates(xs)
      case x :: xs if (!xs.head.equals(x)) => x :: removeConsecutiveDuplicates(xs)
    }
  }

  // P09 (**) Pack consecutive duplicates of list elements into sublists.
  // If a list contains repeated elements they should be placed in separate sublists.
  // Example:
  //   scala> pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
  // res0: List[List[Symbol]] = List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e))
  def pack[A](ls: List[A]): List[List[A]] = {
    def prepacking[A](elems: List[List[A]], ls: List[A]): List[List[A]] = {
        ls match {
          case Nil =>
            elems
          case x :: xs if (elems.head.contains(x)) =>
            prepacking(((x :: elems.head): List[A]) :: elems.tail, xs)
          case x :: xs if (!elems.head.contains(x)) =>
            prepacking(List(x) :: elems, xs)
        }
    }
    if (ls.isEmpty) List(ls)
    else reverse(prepacking(List(List(ls.head)), ls.tail))
  }

  // P10 (*) Run-length encoding of a list.
  // Use the result of problem P09 to implement the so-called run-length encoding data compression method.
  // Consecutive duplicates of elements are encoded as tuples (N, E) where N is the number of duplicates
  // of the element E.
  // Example:
  //   scala> encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
  // res0: List[(Int, Symbol)] = List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))
  def encode[A](ls: List[A]): List[(Int, A)] =
    if (ls.nonEmpty) pack(ls).map(elem => (elem.length, elem.head)) else List()

  // P11 (*) Modified run-length encoding.
  // Modify the result of problem P10 in such a way that if an element has no duplicates
  // it is simply copied into the result list. Only elements with duplicates are transferred
  // as (N, E) terms.
  // Example:
  //   scala> encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
  // res0: List[Any] = List((4,'a), 'b, (2,'c), (2,'a), 'd, (4,'e))
  def encodeModified[A](ls: List[A]): List[Any] = {
    if (ls.nonEmpty) pack(ls).map(
      elem => elem.length match {
        case 1 => elem.head
        case _ => (elem.length, elem.head)
      }) else List()
  }
}