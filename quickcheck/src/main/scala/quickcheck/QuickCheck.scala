package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {
  property("bogus1/bogus2") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }
  
  property("bogus2") = forAll { (a: Int, b: Int) =>
    val h = insert(b, insert(a, empty))
    findMin(h) == math.min(a, b)
  }
    
  property("bogusAll") = forAll { (a: List[Int]) =>
    def sortedElems(h1: H): List[A] = {
      if (isEmpty(h1)) List[A]()
      else findMin(h1) :: sortedElems(deleteMin(h1))
    }
    
    val h = a.foldLeft(empty)((h1: H, a1: Int) => insert(a1, h1))
    
    val sortedA = a.sorted
    val sortedH = sortedElems(h)
    
    sortedA.corresponds(sortedH)(_ == _)
  }
  
  property("bogus5") = forAll { (h1: H, h2: H) =>
    val h = meld(h1, h2)
    findMin(h) == math.min(findMin(h1), findMin(h2))
  }
    
  
  /* Hints from assignment */
  property("hint1") = forAll { (a: Int, b: Int) =>
    val h = insert(b, insert(a, empty))
    findMin(h) == math.min(a, b)
  }

  property("hint2") = forAll { (a: Int) =>
    val h = deleteMin(insert(a, empty))
    isEmpty(h)
  }

  property("hint3") = forAll { (h: H) =>
    def elements(h1: H): List[A] = {
      if (isEmpty(h1)) List[A]()
      else findMin(h1) :: elements(deleteMin(h1))
    }

    val elems = elements(h)
    val sortedElems = elems.sorted

    elems.corresponds(sortedElems)(_ == _)
  }

  property("hint4") = forAll { (h1: H, h2: H) =>
    val h = meld(h1, h2)
    findMin(h) == math.min(findMin(h1), findMin(h2))
  }

  lazy val genHeap: Gen[H] = for {
    a <- arbitrary[A]
    h <- frequency((1, value(empty)), (10, genHeap))
  } yield insert(a, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)
}
