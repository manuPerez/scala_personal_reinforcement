package ninetynineproblems

import java.util.NoSuchElementException

import org.scalatest._
import ninetynineproblems.Level1_10._

class Level1_10Test extends FlatSpec with Matchers {
  "The lastListElement method " should " return last element in a list" in {
    val lastElement = 9
    val list = 0 :: 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: 8 :: lastElement :: Nil
    val list2 = 0 :: 1 :: lastElement :: Nil
    lastListElement(list) shouldEqual lastElement
    lastListElement(list2) shouldEqual lastElement
    noException should be thrownBy lastListElement(list)
  }

  "The lastListElement method " should " throw an exception with empty list" in {
    the [NoSuchElementException] thrownBy {
      lastListElement(Nil)
    } should have message "Bad input"
  }

  "The penultimate method " should " return last but one element in a list" in {
    val lastElement = 9
    val lastButOneElement = 8
    val list = 0 :: 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: lastButOneElement :: lastElement :: Nil
    val list2 = 0 :: lastButOneElement :: lastElement :: Nil
    penultimate(list) shouldEqual lastButOneElement
    penultimate(list2) shouldEqual lastButOneElement
    noException should be thrownBy penultimate(list)
  }

  "The penultimate method " should " throw an exception with empty list" in {
    the [NoSuchElementException] thrownBy {
      penultimate(Nil)
    } should have message "Bad input"
  }

  "The penultimate method " should " throw an exception with list with only one element" in {
    the [NoSuchElementException] thrownBy {
      penultimate(0 :: Nil)
    } should have message "Bad input, list with only one element"
  }

  "The nth method " should " return nth element in a list" in {
    val nthPos = 2
    val nthValue = 42
    val list = 0 :: 1 :: nthValue :: 3 :: 4 :: 5 :: 6 :: 7 :: 8 :: 9 :: Nil
    val list2 = 0 :: 1 :: nthValue :: Nil
    nth(nthPos,list) shouldEqual nthValue
    nth(nthPos,list2) shouldEqual nthValue
    noException should be thrownBy nth(nthPos,list)
  }

  "The nth method " should " throw an exception with short list" in {
    the [NoSuchElementException] thrownBy {
      nth(0,Nil)
    } should have message "List too short"
    the [NoSuchElementException] thrownBy {
      nth(100,1 :: Nil)
    } should have message "List too short"
  }

  "The nth method " should " throw an exception with a negative element" in {
    the [NoSuchElementException] thrownBy {
      nth(-1,1 :: Nil)
    } should have message "Element must be greater or equals to 0"
  }

  "The lengthList method " should " return length in a list" in {
    val length1 = 10
    val length2 = 3
    val list = 0 :: 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: 8 :: 9 :: Nil
    val list2 = 0 :: 1 :: 2 :: Nil
    lengthList(list) shouldEqual length1
    lengthList(list2) shouldEqual length2
    lengthList(Nil) shouldEqual 0
    noException should be thrownBy lengthList(list)
  }

  "The reverse method " should " return the reversed list" in {
    val list = 0 :: 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: 8 :: 9 :: Nil
    val listReverse = 9 :: 8 :: 7 :: 6 :: 5 :: 4 :: 3 :: 2 :: 1 :: 0 :: Nil
    reverse(list) shouldEqual listReverse
    reverse(listReverse) shouldEqual list
    reverse(Nil) shouldEqual Nil
    noException should be thrownBy reverse(list)
  }

  "The isPalindrome method " should " return true if a list is palindrome" in {
    val listTrue = List(1, 2, 3, 4, 5, 4, 3, 2, 1)
    val listFalse = List(1, 2, 3, 4, 5, 4, 3, 2, 2)
    isPalindrome(listTrue) shouldBe true
    isPalindrome(listFalse) shouldBe false
  }

  "The flatten method " should " return a list with all elements" in {
    val listIni = List(List(1, 2, 3), 4, List(5, 6, 7), 8, 9)
    val listResult = List(1, 2, 3, 4, 5, 6, 7, 8, 9)
    flatten(listIni) shouldEqual listResult
  }

  "The removeConsecutiveDuplicates method " should " return a list with repeated elements replaced " +
    "with a single copy of the element" in {
    val listIni = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
    val listResult =  List('a, 'b, 'c, 'a, 'd, 'e)
    removeConsecutiveDuplicates(listIni) shouldEqual listResult
  }

  "The pack method " should " Pack consecutive duplicates of list elements into sublists" in {
    val listIniOne = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
    val listResultOne = List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e))
    val listIniTwo = List('a, 'b, 'c, 'd, 'e, 'e, 'e, 'e)
    val listResultTwo = List(List('a), List('b), List('c), List('d), List('e, 'e, 'e, 'e))
    val listIniThree = List()
    val listResultThree = List(List())
    val listIniFour = List('a)
    val listResultFour = List(List('a))
    val listIniFive = List('a, 'a)
    val listResultFive = List(List('a, 'a))

    pack(listIniOne) shouldEqual listResultOne
    pack(listIniTwo) shouldEqual listResultTwo
    pack(listIniThree) shouldEqual listResultThree
    pack(listIniFour) shouldEqual listResultFour
    pack(listIniFive) shouldEqual listResultFive
  }

  "The encode method " should " return a list of encoded tuples (N, E) where N is the number " +
    "of duplicates of the element E" in {
    val listIniOne = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
    val listResultOne = List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))
    val listIniTwo = List()
    val listResultTwo = List()

    encode(listIniOne) shouldEqual listResultOne
    encode(listIniTwo) shouldEqual listResultTwo
  }

  "The encodeModified method " should " return a list of encoded tuples (N, E) " +
    "where N is the number of duplicates of the element E, but if N == 1 it returns " +
    "only E" in {
    val listIniOne = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
    val listResultOne = List((4,'a), 'b, (2,'c), (2,'a), 'd, (4,'e))
    val listIniTwo = List()
    val listResultTwo = List()
    val listIniThree = List('a, 'b, 'c, 'c, 'e)
    val listResultThree = List('a, 'b, (2,'c), 'e)
    val listIniFour = List('a, 'e)
    val listResultFour = List('a, 'e)
    val listIniFive = List('a)
    val listResultFive = List('a)

    encodeModified(listIniOne) shouldEqual listResultOne
    encodeModified(listIniTwo) shouldEqual listResultTwo
    encodeModified(listIniThree) shouldEqual listResultThree
    encodeModified(listIniFour) shouldEqual listResultFour
    encodeModified(listIniFive) shouldEqual listResultFive
  }
}
