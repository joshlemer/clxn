package com.github.joshlemer.clxn

import scala.collection.Factory
import scala.collection.immutable.{SeqMap, SortedMap, SortedSet}

package object unsafewrap {

  implicit final class IterableObjectExtension(private val c: Iterable.type) extends AnyVal {
    def unsafeWrap[A](it: collection.Iterable[A]): Iterable[A] = it match {
      case xs: Iterable[A] => xs
      case xs if xs.knownSize == 0 => Nil
      case _ => new IterableWrapper(it)
    }
  }

  private final class IterableWrapper[+A](is: collection.Iterable[A]) extends Iterable[A] {
    override def iterator: Iterator[A] = is.iterator
  }


  implicit final class SeqObjectExtension(private val c: Seq.type) extends AnyVal {
    def unsafeWrap[A](it: collection.Seq[A]): Seq[A] = it match {
      case xs: IndexedSeq[A] => new IndexedSeqWrapper(xs)
      case xs: Seq[A] => xs
      case xs if xs.isEmpty => Nil
      case _ => new SeqWrapper(it)
    }
  }

  private final class SeqWrapper[+A](is: collection.Seq[A]) extends Seq[A] {
    override def apply(i: Int): A = is(i)
    override def length: Int = is.length
    override def iterator: Iterator[A] = is.iterator
  }

  implicit final class IndexedSeqObjectExtension(private val is: IndexedSeq.type) extends AnyVal {
    def unsafeWrap[A](it: collection.IndexedSeq[A]): IndexedSeq[A] = it match {
      case xs: IndexedSeq[A] => xs
      case xs if xs.isEmpty => Vector.empty
      case _ => new IndexedSeqWrapper(it)
    }
  }

  private final class IndexedSeqWrapper[+A](is: collection.IndexedSeq[A]) extends IndexedSeq[A] {
    override def apply(i: Int): A = is(i)
    override def length: Int = is.length
  }

  implicit final class SetObjectExtension(private val c: Set.type) extends AnyVal {
    def unsafeWrap[A](it: collection.Set[A]): Set[A] = it match {
      case xs: Set[A] => xs
      case xs if xs.isEmpty => Set.empty
      case _ => new SetWrapper(it)
    }
  }

  private final class SetWrapper[A](is: collection.Set[A]) extends Set[A] {
    override def incl(elem: A): Set[A] = if (is.contains(elem)) this else is.toSet.incl(elem)
    override def excl(elem: A): Set[A] = if (is.contains(elem)) is.toSet.excl(elem) else this
    override def contains(elem: A): Boolean = is.contains(elem)
    override def iterator: Iterator[A] = is.iterator
  }

  implicit final class SortedSetObjectExtension(private val c: SortedSet.type) extends AnyVal {
    def unsafeWrap[A](it: collection.SortedSet[A]): Set[A] = it match {
      case xs: SortedSet[A] => xs
      case xs if xs.isEmpty => SortedSet.empty(xs.ordering)
      case _ => new SortedSetWrapper(it)
    }
  }

  private final class SortedSetWrapper[A](is: collection.SortedSet[A]) extends SortedSet[A] {
    override def iteratorFrom(start: A): Iterator[A] = is.iterator
    override def ordering: Ordering[A] = is.ordering
    override def rangeImpl(from: Option[A], until: Option[A]): SortedSet[A] = {
      val result = is.rangeImpl(from, until)
      if (result eq is) this else new SortedSetWrapper(result)
    }
    override def incl(elem: A): SortedSet[A] =
      if (is.contains(elem)) this else is.to(SortedSet.evidenceIterableFactory(is.ordering)).incl(elem)
    override def excl(elem: A): SortedSet[A] =
      if (is.contains(elem)) is.to(SortedSet.evidenceIterableFactory(is.ordering)).excl(elem) else this
    override def contains(elem: A): Boolean = is.contains(elem)
    override def iterator: Iterator[A] = is.iterator
  }

  implicit final class MapObjectExtension(private val c: Map.type) extends AnyVal {
    def unsafeWrap[K, V](it: collection.Map[K, V]): Map[K, V] = it match {
      case xs: Map[K, V] => xs
      case xs if xs.isEmpty => Map.empty
      case _ => new MapWrapper(it)
    }
  }

  private final class MapWrapper[K, V](is: collection.Map[K, V]) extends Map[K, V] {
    override def removed(key: K): Map[K, V] = if (is.contains(key)) is.toMap.removed(key) else this
    override def updated[V1 >: V](key: K, value: V1): Map[K, V1] = is.toMap.updated(key, value)
    override def get(key: K): Option[V] = is.get(key)
    override def iterator: Iterator[(K, V)] = is.iterator
  }

  implicit final class SortedMapObjectExtension(private val c: SortedMap.type) extends AnyVal {
    def unsafeWrap[K, V](it: collection.SortedMap[K, V]): SortedMap[K, V] = it match {
      case xs: SortedMap[K, V] => xs
      case xs if xs.isEmpty => SortedMap.empty(it.ordering)
      case _ => new SortedMapWrapper(it)
    }
  }

  private final class SortedMapWrapper[K, V](is: collection.SortedMap[K, V]) extends SortedMap[K, V] {
    override def updated[V1 >: V](key: K, value: V1): SortedMap[K, V1] = is.to(SortedMap.sortedMapFactory(is.ordering))
    override def iteratorFrom(start: K): Iterator[(K, V)] = is.iteratorFrom(start)
    override def keysIteratorFrom(start: K): Iterator[K] = is.keysIteratorFrom(start)
    override def ordering: Ordering[K] = is.ordering
    override def rangeImpl(from: Option[K], until: Option[K]): SortedMap[K, V] = {
      val result = is.rangeImpl(from, until)
      if (result eq is) this else SortedMap.unsafeWrap(result)
    }
    override def removed(key: K): SortedMap[K, V] =
      if (is.contains(key)) {
        val fact: Factory[(K, V), SortedMap[K, V]] = SortedMap.sortedMapFactory(is.ordering)
        is.to(fact).removed(key)
      }
      else this

    override def get(key: K): Option[V] = is.get(key)
    override def iterator: Iterator[(K, V)] = is.iterator
  }

  implicit final class SeqMapObjectExtension(private val c: SeqMap.type) extends AnyVal {
    def unsafeWrap[K, V](it: collection.SeqMap[K, V]): SeqMap[K, V] = it match {
      case xs: SeqMap[K, V] => xs
      case xs if xs.isEmpty => SeqMap.empty
      case _ => new SeqMapWrapper(it)
    }
  }

  private final class SeqMapWrapper[K, V](is: collection.SeqMap[K, V]) extends SeqMap[K, V] {
    override def removed(key: K): SeqMap[K, V] = if (is.contains(key)) is.to(SeqMap) else this
    override def updated[V1 >: V](key: K, value: V1): SeqMap[K, V1] = is.to(SeqMap).updated(key, value)
    override def get(key: K): Option[V] = is.get(key)
    override def iterator: Iterator[(K, V)] = is.iterator
  }

}
