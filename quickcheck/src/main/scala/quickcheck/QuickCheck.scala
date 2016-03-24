package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("min2") = forAll { (a: Int, b: Int) =>
    val h = insert(insert(a, empty), b)
    findMin(h) == List(a, b).min
  }

  lazy val genHeap: Gen[H] =  for {
    v <- arbitrary[Int]
    m <- oneOf(const(Heap.empty[Int]), genHeap)
  } yield m.insert(v)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

}
