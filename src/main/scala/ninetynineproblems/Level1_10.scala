package ninetynineproblems

object Level1_10 {

//  P01 (*) Find the last element of a list.
//  Example:
//    scala> lastCustom(List(1, 1, 2, 3, 5, 8))
//  res0: Int = 8
  def lastListElement[A](ls: List[A]): A = {
    ls match {
      case x :: Nil => x
      case x :: xs => lastListElement(xs)
      case _ => throw new NoSuchElementException("Bad input")
    }
  }


}
