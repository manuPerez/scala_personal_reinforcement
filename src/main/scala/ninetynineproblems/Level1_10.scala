package ninetynineproblems

object Level1_10 {

//  P01 (*) Find the last element of a list.
//  Example:
//    scala> lastListElement(List(1, 1, 2, 3, 5, 8))
//  res0: Int = 8
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
  def penultimate[A](ls: List[A]): A = {
    ls match {
      case x :: y :: Nil => x
      case x :: Nil => throw new NoSuchElementException("Bad input, list with only one element")
      case x :: xs => penultimate(xs)
      case _ => throw new NoSuchElementException("Bad input")
    }
  }

}
