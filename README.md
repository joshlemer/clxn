# Clxn (pronounced "collection")

Clxn is a library that contains extra functionality for the Scala Standard Library Collections. 


# Features

### unsafewrap

The `com.github.joshlemer.clxn.unsafewrap` package provides decorators to allow the wrapping of mutable and unknown-mutability collections (things in the scala.collection package) in immutable wrappers. This is the fastest way to convert a mutable collection into an immutable one, for cases where performance matters, as it involves no copying but instead just wraps a mutable collection in an immutable facade.

Example usage: 

```scala

import com.github.joshlemer.clxn.unsafewrap._

import collection.immutable.{SeqMap, SortedMap, SortedSet}
import scala.collection.mutable

// wrap a mutable collection in an immutable wrapper
val seq1: Seq[Int] = Seq.unsafeWrap(mutable.ArrayBuffer(1,2,3)) 

// also works for collections of unknown mutability, or even immutable collections
val seq2: Seq[Int] = Seq.unsafeWrap(List(1,2,3) : collection.Seq[Int]) 

// if the collection passed for wrapping is already immutable, no wrapping is applied. The passed collection is returned
val seq3: Seq[Int] = Seq.unsafeWrap(List(1,2,3)) // List(1,2,3)


// also convert Sets, Maps, SeqMaps, SortedMaps, SortedSets, and plain old Iterabls
val set: Set[Int] = Set.unsafeWrap(mutable.HashSet(1,2,3))

val map: Map[Int, String] = Map.unsafeWrap(mutable.HashMap(1 -> "hi"))

val seqMap: SeqMap[Int, String] = SeqMap.unsafeWrap(mutable.LinkedHashMap(1 -> "hi"))

val sortedMap: SortedMap[Int, String] = SortedMap.unsafeWrap(mutable.TreeMap(1 -> "hi"))

val sortedSet: SortedSet[Int] = SortedSet.unsafeWrap(mutable.TreeSet(1))

val iterable: Iterable[Int] = Iterable.unsafeWrap(mutable.ArrayBuffer(1,2,3))

```
