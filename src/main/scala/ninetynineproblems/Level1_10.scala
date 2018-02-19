package ninetynineproblems

import java.util.NoSuchElementException

import scala.annotation.tailrec

object Level1_10 extends App {
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

  println(flatten(List(List(1, 1), 4, Nil, List(2,3))))
}
