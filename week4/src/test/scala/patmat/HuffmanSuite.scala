package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
	trait TestTrees {
		val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
		val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
    val t3 = Leaf('a',3)
	}


  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }


  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a','b','d'))
    }
  }


  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("test times function") {
    assert(times(List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd')) === List(('h',1), ('e',1), ('l',3), ('o',2), (',',1), (' ',1), ('w',1), ('r',1), ('d',1)))
  }


  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }


  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }

  test("test until function") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(until(singleton, combine)(leaflist) === List(Fork(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3),Leaf('x',4),List('e', 't', 'x'),7)))
  }

  test("test create code tree function") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(createCodeTree(string2Chars("hello, world") )=== Fork(Fork(Leaf('o',2),Fork(Leaf('d',1),Fork(Leaf('w',1),Leaf('r',1),List('w', 'r'),2),List('d', 'w', 'r'),3),List('o', 'd', 'w', 'r'),5),Fork(Leaf('l',3),Fork(Fork(Leaf(',',1),Leaf(' ',1),List(',', ' '),2),Fork(Leaf('h',1),Leaf('e',1),List('h', 'e'),2),List(',',' ', 'h', 'e'),4),List('l',',',' ', 'h', 'e'),7),List('o', 'd', 'w', 'r', 'l',',',' ' , 'h', 'e'),12))
  }

  test("test encode") {
    new TestTrees {
      assert(encode(t2)("ab".toList) === List(0, 0, 0, 1))
    }
  }
  test("test convert") {
    new TestTrees {
      assert(convert(t2) === List(('a', List(0, 0)), ('b', List(0, 1)), ('d', List(1))))
    }
  }

  test("test codebits") {
    new TestTrees {
      println(convert(t2))
      assert(codeBits(convert(t2))('b') === List(0,1))
    }
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

  test("test quick encode") {
    new TestTrees {
      assert(quickEncode(t1)("ab".toList) === List(0, 1))
    }
  }

}
