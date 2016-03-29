package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._
import Math._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("min2") = forAll { (a: Int, b: Int) =>
    val h = insert(b, insert(a, empty))
    findMin(h) == min(a, b)
  }

  property("hint1") = forAll {(a: Int, b: Int) =>
    val h = insert(b, insert(a, empty))
    findMin(deleteMin(h)) == max(a, b)
  }

  // 用到了后面定义的genHeap 函数，生成随机的heap来做测试
  property("minmeld") = forAll {(h1: H, h2: H) =>
    findMin(meld(h1, h2)) == min(findMin(h1), findMin(h2))
  }

  property("deletemin") = forAll {(h1: H, h2: H) =>
    findMin(meld(h1, h2)) == min(findMin(h1), findMin(h2))
  }


  lazy val genHeap: Gen[H] =  for {
    v <- arbitrary[A]
    m <- frequency((2,Gen.const(empty)),(8, genHeap))
  } yield insert(v, m)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

}
