package ninetynineproblems

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
  @tailrec
  def nth[A](element:Int,ls: List[A]): A = {
    if (element < 0) throw new NoSuchElementException("Element must be greater or equals to 0")
    (element,ls) match {
      case (0, x :: xs) => x
      case (n, x :: xs) => nth(n-1,xs)
      case (n, Nil) => throw new NoSuchElementException("List too short")
    }
  }

//  P04 (*) Find the number of elements of a list.
//    Example:
//    scala> length(List(1, 1, 2, 3, 5, 8))
//  res0: Int = 6
  def lengthList[A](ls: List[A]): Int = {
    @tailrec
    def lengthRec[A](result:Int, ls: List[A]): Int = {
      ls match {
        case Nil => result
        case x :: xs=> lengthRec(result+1,xs)
      }
    }
    lengthRec(0,ls)
  }


//  P05 (*) Reverse a list.
//  Example:
//    scala> reverse(List(1, 1, 2, 3, 5, 8))
//  res0: List[Int] = List(8, 5, 3, 2, 1, 1)
  def reverse[A](ls: List[A]): List[A] = {
    @tailrec
    def reverseRec[A](result: List[A], ls: List[A]): List[A] = {
      ls match {
        case Nil => result
        case x :: xs=> reverseRec(x :: result,xs)
      }
    }
    reverseRec(Nil,ls)
  }

}
