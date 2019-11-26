package com.github.joshlemer.clxn

import scala.collection.Factory
import scala.collection.immutable.{SeqMap, SortedMap, SortedSet}

package object unsafewrap {

  implicit final class IterableObjectExtension(private val c: Iterable.type) extends AnyVal {
    /** Produces an immutable Iterable from any collection.Iterable, without performing any copying.
      *
      * If the passed Iterable is already immutable, no wrapping is applied.
      *
      * Otherwise, an immutable wrapping Iterable will be returned which delegates to the passed
      * Iterable for all operations. Any further mutations performed on the passed Iterable will
      * result in undefined behaviour and is considered user error -- changes may or may not be
      * reflected in the resulting immutable.Iterable
      *
      * @param it the Iterable to wrap as immutable
      * @tparam A the type of element the Iterable contains
      * @return an immutable Iterable containing the same data as the passed Iterable
      */
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
    /** Produces an immutable Seq from any collection.Seq, without performing any copying.
      *
      * If the passed Seq is already immutable, no wrapping is applied.
      *
      * Otherwise, an immutable wrapping Seq will be returned which delegates to the passed
      * Seq for all operations. Any further mutations performed on the passed Seq will
      * result in undefined behaviour and is considered user error -- changes may or may not be
      * reflected in the resulting immutable.Seq
      *
      * @param it the Seq to wrap as immutable
      * @tparam A the type of element the Seq contains
      * @return an immutable Seq containing the same data as the passed Seq
      */
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
    /** Produces an immutable IndexedSeq from any collection.IndexedSeq, without performing any copying.
      *
      * If the passed IndexedSeq is already immutable, no wrapping is applied.
      *
      * Otherwise, an immutable wrapping IndexedSeq will be returned which delegates to the passed
      * IndexedSeq for all operations. Any further mutations performed on the passed IndexedSeq will
      * result in undefined behaviour and is considered user error -- changes may or may not be
      * reflected in the resulting immutable.IndexedSeq
      *
      * @param it the IndexedSeq to wrap as immutable
      * @tparam A the type of element the IndexedSeq contains
      * @return an immutable IndexedSeq containing the same data as the passed IndexedSeq
      */
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
    /** Produces an immutable Set from any collection.Set, without performing any copying.
      *
      * If the passed Set is already immutable, no wrapping is applied.
      *
      * Otherwise, an immutable wrapping Set will be returned which delegates to the passed
      * Set for all operations. Any further mutations performed on the passed Set will
      * result in undefined behaviour and is considered user error -- changes may or may not be
      * reflected in the resulting immutable.Set
      *
      * @param it the Set to wrap as immutable
      * @tparam A the type of element the Set contains
      * @return an immutable Set containing the same data as the passed Set
      */
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
    /** Produces an immutable SortedSet from any collection.SortedSet, without performing any copying.
      *
      * If the passed SortedSet is already immutable, no wrapping is applied.
      *
      * Otherwise, an immutable wrapping SortedSet will be returned which delegates to the passed
      * SortedSet for all operations. Any further mutations performed on the passed SortedSet will
      * result in undefined behaviour and is considered user error -- changes may or may not be
      * reflected in the resulting immutable.SortedSet
      *
      * @param it the SortedSet to wrap as immutable
      * @tparam A the type of element the SortedSet contains
      * @return an immutable SortedSet containing the same data as the passed SortedSet
      */
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
    /** Produces an immutable Map from any collection.Map, without performing any copying.
      *
      * If the passed Map is already immutable, no wrapping is applied.
      *
      * Otherwise, an immutable wrapping Map will be returned which delegates to the passed
      * Map for all operations. Any further mutations performed on the passed Map will
      * result in undefined behaviour and is considered user error -- changes may or may not be
      * reflected in the resulting immutable.Map
      *
      * @param it the Map to wrap as immutable
      * @tparam K the type of key Map contains
      * @tparam V the type of key Map contains
      * @return an immutable Map containing the same data as the passed Map
      */
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
    /** Produces an immutable SortedMap from any collection.SortedMap, without performing any copying.
      *
      * If the passed SortedMap is already immutable, no wrapping is applied.
      *
      * Otherwise, an immutable wrapping SortedMap will be returned which delegates to the passed
      * SortedMap for all operations. Any further mutations performed on the passed SortedMap will
      * result in undefined behaviour and is considered user error -- changes may or may not be
      * reflected in the resulting immutable.SortedMap
      *
      * @param it the SortedMap to wrap as immutable
      * @tparam K the type of key Map contains
      * @tparam V the type of key Map contains
      * @return an immutable SortedMap containing the same data as the passed Map
      */
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
    /** Produces an immutable SeqMap from any collection.SeqMap, without performing any copying.
      *
      * If the passed SeqMap is already immutable, no wrapping is applied.
      *
      * Otherwise, an immutable wrapping SeqMap will be returned which delegates to the passed
      * SeqMap for all operations. Any further mutations performed on the passed SeqMap will
      * result in undefined behaviour and is considered user error -- changes may or may not be
      * reflected in the resulting immutable.SeqMap
      *
      * @param it the SeqMap to wrap as immutable
      * @tparam K the type of key Map contains
      * @tparam V the type of key Map contains
      * @return an immutable SeqMap containing the same data as the passed Map
      */
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
