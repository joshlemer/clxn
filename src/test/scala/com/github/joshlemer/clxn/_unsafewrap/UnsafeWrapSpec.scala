package com.github.joshlemer.clxn._unsafewrap // intentionally in different package to "test" imports

import org.junit.Test
import org.junit.Assert.assertEquals

import scala.collection.{mutable => m, immutable => im}
import scala.util.Try

class UnsafeWrapSpec {
  @Test
  def IterableUnsafeWrap(): Unit = {

    import com.github.joshlemer.clxn.unsafewrap._
    def runTestCase(input: collection.Iterable[Any]): Unit = {
      val wrapped = Iterable.unsafeWrap(input)
      assertEquals(input, wrapped)
      assertEquals(input.iterator.toSeq, wrapped.iterator.toSeq)
    }

    val testCases: Seq[collection.Seq[Any]] = Seq(
      // immutable Seqs
      List(),
      List(1),
      List(1, 2, 3, 4),
      Vector(),
      Vector(1),
      Vector(1, 2, 3, 4),
      LazyList(),
      LazyList(1),
      LazyList(1, 2, 3, 4),
      // mutable Seqs
      m.ArrayBuffer(),
      m.ArrayBuffer(1),
      m.ArrayBuffer(1, 2, 3, 4),
      m.ListBuffer(),
      m.ListBuffer(1),
      m.ListBuffer(1, 2, 3, 4),
      m.ArrayDeque(),
      m.ArrayDeque(1),
      m.ArrayDeque(1, 2, 3, 4)
    )

    testCases.foreach(runTestCase)

  }

  @Test
  def SeqUnsafeWrap(): Unit = {

    import com.github.joshlemer.clxn.unsafewrap._
    def runTestCase(input: collection.Seq[Any]): Unit = {
      val wrapped = Seq.unsafeWrap(input)
      assertEquals(input, wrapped)
      val inputLength = input.length
      for { i <- 0 until inputLength } {
        assertEquals(input(i), wrapped(i))
      }
      assert(Try(wrapped(-1)).isFailure)
      assert(Try(wrapped(inputLength)).isFailure)
      assertEquals(input.iterator.toSeq, wrapped.iterator.toSeq)
    }

    val testCases: Seq[collection.Seq[Any]] = Seq(
      // immutable Seqs
      List(),
      List(1),
      List(1, 2, 3, 4),
      Vector(),
      Vector(1),
      Vector(1, 2, 3, 4),
      LazyList(),
      LazyList(1),
      LazyList(1, 2, 3, 4),
      // mutable Seqs
      m.ArrayBuffer(),
      m.ArrayBuffer(1),
      m.ArrayBuffer(1, 2, 3, 4),
      m.ListBuffer(),
      m.ListBuffer(1),
      m.ListBuffer(1, 2, 3, 4),
      m.ArrayDeque(),
      m.ArrayDeque(1),
      m.ArrayDeque(1, 2, 3, 4)
    )

    testCases.foreach(runTestCase)

  }
  @Test
  def IndexedSeqUnsafeWrap(): Unit = {

    import com.github.joshlemer.clxn.unsafewrap._
    def runTestCase(input: collection.IndexedSeq[Any]): Unit = {
      val wrapped = Seq.unsafeWrap(input)
      assertEquals(input, wrapped)
      val inputLength = input.length
      for { i <- 0 until inputLength } {
        assertEquals(input(i), wrapped(i))
      }
      assert(Try(wrapped(-1)).isFailure)
      assert(Try(wrapped(inputLength)).isFailure)
      assertEquals(input.iterator.toSeq, wrapped.iterator.toSeq)
    }

    val testCases: Seq[collection.IndexedSeq[Any]] = Seq(
      // immutable Seqs
      Vector(),
      Vector(1),
      Vector(1, 2, 3, 4),
      // mutable Seqs
      m.ArrayBuffer(),
      m.ArrayBuffer(1),
      m.ArrayBuffer(1, 2, 3, 4),
      m.ArrayDeque(),
      m.ArrayDeque(1),
      m.ArrayDeque(1, 2, 3, 4)
    )

    testCases.foreach(runTestCase)

  }
  @Test
  def SetUnsafeWrap(): Unit = {

    import com.github.joshlemer.clxn.unsafewrap._

    val sentinel = new AnyRef {}
    def runTestCase(input: collection.Set[Any]): Unit = {
      val wrapped = Set.unsafeWrap(input)
      assertEquals(input, wrapped)
      assertEquals(input.iterator.toSeq, wrapped.iterator.toSeq)
      input.foreach(x => assert(wrapped.contains(x)))
      wrapped.foreach(x => assert(input.contains(x)))
      assert(!wrapped.contains(sentinel))
    }

    val testCases: Seq[collection.Set[Any]] = Seq(
      // immutable Sets
      Set(),
      Set(1),
      Set(1, 2, 3, 4),
      im.HashSet(),
      im.HashSet(1),
      im.HashSet(1,2,3,4),
      // mutable Sets
      m.Set(),
      m.Set(1),
      m.Set(1, 2, 3, 4),
      m.HashSet(),
      m.HashSet(1),
      m.HashSet(1,2,3,4),
    )

    testCases.foreach(runTestCase)

  }
  @Test
  def SortedSetUnsafeWrap(): Unit = {

    import com.github.joshlemer.clxn.unsafewrap._

    def runTestCase(input: collection.Set[_]): Unit = {
      val wrapped = Set.unsafeWrap(input)
      assertEquals(input, wrapped)
      assertEquals(input.iterator.toSeq, wrapped.iterator.toSeq)
    }

    val testCases: Seq[collection.Set[_]] = Seq(
      // immutable Sets
      Set(),
      Set(1),
      Set(1, 2, 3, 4),
      im.HashSet(),
      im.HashSet(1),
      im.HashSet(1,2,3,4),
      // mutable Sets
      m.Set(),
      m.Set(1),
      m.Set(1, 2, 3, 4),
      m.HashSet(),
      m.HashSet(1),
      m.HashSet(1,2,3,4),
    )

    testCases.foreach(runTestCase)

  }
  @Test
  def MapUnsafeWrap(): Unit = {

    import com.github.joshlemer.clxn.unsafewrap._
    def runTestCase[K, V](input: collection.Map[K, V]): Unit = {
      val _map = input.toMap
      val wrapped = Map.unsafeWrap(input)
      assertEquals(input, wrapped)
      assertEquals(input.iterator.toSeq, wrapped.iterator.toSeq)
      input.foreach { case (k, v) =>
        assertEquals(wrapped.removed(k), _map.removed(k))
        assertEquals(wrapped.updated(k, 99), _map.updated(k, 99))
        assertEquals(wrapped.get(k), Some(v))
      }
    }

    val testCases: Seq[collection.Map[Int, Int]] = Seq(
      // immutable Sets
      Map(),
      Map(1 -> 1),
      Map(1 -> 1, 2 -> 2, 3 -> 3, 4 -> 4),
      im.HashMap(),
      im.HashMap(1 -> 1),
      im.HashMap(1 -> 1, 2 -> 2, 3 -> 3, 4 -> 4),
      // mutable Sets
      m.Map(),
      m.Map(1 -> 1),
      m.Map(1 -> 1, 2 -> 2, 3 -> 3, 4 -> 4),
      m.HashMap(),
      m.HashMap(1 -> 1),
      m.HashMap(1 -> 1, 2 -> 2, 3 -> 3, 4 -> 4),
    )

    testCases.foreach(runTestCase)

  }
}
