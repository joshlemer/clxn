package com.github.joshlemer.clxn._unsafewrap // intentionally in different package to "test" imports


import org.junit.Test
import org.junit.Assert.assertEquals

import scala.util.Try

class UnsafeWrapSpec {

  @Test
  def SeqUnsafeWrap(): Unit = {

    import com.github.joshlemer.clxn.unsafewrap._
    def runTestCase(input: collection.Seq[Any]): Unit = {
      val wrapped = Seq.unsafeWrap(input)
      assertEquals(input, wrapped)
      val inputLength = input.length
      for { i <- 0 until inputLength} {
        assertEquals(input(i), wrapped(i))
      }
      assert(Try(wrapped(-1)).isFailure)
      assert(Try(wrapped(inputLength)).isFailure)
    }


    val testCases: Seq[collection.Seq[Any]] = Seq(
      // immutable Seqs
      List(),
      List(1),
      List(1,2,3,4),
      Vector(),
      Vector(1),
      Vector(1,2,3,4)
    )


  }
}
