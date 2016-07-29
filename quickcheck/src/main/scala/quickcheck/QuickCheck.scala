package quickcheck

import org.scalacheck.Gen._
import org.scalacheck.Prop._
import org.scalacheck._
import Arbitrary.arbitrary

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    v <- arbitrary[A]
    hh <- oneOf(const(empty), genHeap)
  } yield insert(v, hh)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("two inserts") = forAll { (x: A, y: A) =>
    val h = empty
    val h1 = insert(x, h)
    findMin(insert(y, h1)) == (if (x < y) x else y)
  }

  property("three inserts") = forAll { (x: A, y: A, z: A) =>
    val h = empty
    val h1 = insert(x, h)
    val h2 = insert(y, h1)
    findMin(insert(z, h2)) == (List(x, y, z).min)
  }

  property("insert one, delete one") = forAll { (x: A) =>
    val h = empty
    isEmpty(deleteMin(insert(x, h)))
  }

  property("continuously deleting") = forAll { (h: H) =>
    def findAndDelMin(h: H, acc: List[A]): List[A] =
      if (h == Nil) acc
      else findAndDelMin(deleteMin(h), findMin(h)::acc)

    val l = findAndDelMin(h, List.empty)
    l == l.sorted.reverse
  }

  property("melding two heaps") = forAll { (h1: H, h2: H) =>
    val min1 = findMin(h1)
    val min2 = findMin(h2)
    findMin(meld(h1, h2)) == (if (min1 < min2) min1 else min2)
  }

  property("insert minimal") = forAll { (h: H, x: A) =>
    val min = if (findMin(h) < x) findMin(h) else x
    if (!isEmpty(h))
      findMin(insert(min, h)) == min
    else
      true
  }

  property("insert non minimal") = forAll { (h: H, x: A) =>
    val nonmin = if (findMin(h) > x) findMin(h) else x
    if (!isEmpty(h))
      findMin(insert(nonmin, h)) == findMin(h)
    else
      true
  }

  property("insert minimal back") = forAll { (h: H) =>
    val min = findMin(h)
    findMin(insert(min, h)) == min
  }

  property("insert same values") = forAll { (x: A) =>
    val h = empty
    findMin(insert(x, insert(x, insert(x, h)))) == x
  }

  property("meld two same heap") = forAll { (h: H) =>
    findMin(meld(h, h)) == findMin(h)
  }

  property("meld two empty") = forAll { (x: A) =>
    val h1 = empty
    val h2 = empty
    isEmpty(meld(h1, h2))
  }

  property("meld two singleton heap") = forAll { (x: A, y: A) =>
    val h1 = insert(y, insert(x, empty))
    findMin(deleteMin(deleteMin(meld(h1, h1)))) == (if(x < y) y else x)
  }

  property("meld two heap with the same order") = forAll { (x: A, y: A, z: A, w: A) =>
    val h1 = insert(y, insert(x, empty))
    val h2 = insert(w, insert(z, empty))
    val l = List(x, y, z, w).sorted
    findMin((deleteMin(meld(h1, h2)))) == l(1)
    findMin((deleteMin(deleteMin(meld(h1, h2))))) == l(2)
    findMin(deleteMin(deleteMin(deleteMin(meld(h1, h2))))) == l(3)
  }
}
