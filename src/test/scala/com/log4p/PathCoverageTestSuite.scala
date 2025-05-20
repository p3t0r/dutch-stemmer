package com.log4p

import org.scalatest.funsuite.AnyFunSuite
import scala.xml._

class PathCoverageTestSuite extends AnyFunSuite {
  test("all transformation paths are reachable") {
    val doc = XML.loadFile("src/test/resources/test_input.xml")
    val tests = (doc \ "test")
    val allHistory = tests.map { ele =>
      val input = ele.attribute("input").get.toString
      DutchStemmer.stem(input).history
    }.flatten.toSet

    val expectedMarkers = Seq(
      "replaced 'heden' by 'heid'",
      "'en(e)' removed",
      "removed ending se?",
      "step 2 removed e",
      "step3a removed 'en(e)'",
      "3b removed 'ig'",
      "removed 'lijk'",
      "removed 'bar'",
      "removed 'baar'",
      "removed last character",
      "removed ing",
      "removed end"
    )

    expectedMarkers.foreach { marker =>
      assert(allHistory.exists(_.startsWith(marker)), s"No test triggers path: $marker")
    }
  }
}
