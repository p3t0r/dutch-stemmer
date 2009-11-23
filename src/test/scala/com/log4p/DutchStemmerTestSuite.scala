package com.log4p

import org.scalatest.FunSuite
import scala.xml._

class DutchStemmerTestSuite extends FunSuite {
  test("R1 should be the part after the first non-vowel after a vowel") {
    assert(Payload("sinterklaas").R1 === "terklaas")
  }

  test("if R1 starts within the first 3 characters its' start should be adjusted so R1 starts after that") {
    assert(Payload("andijvie").R1 === "ijvie")
  }

  test("R2 should be the part after the first non-vowel after a vowel with R1") {
    assert(Payload("sinterklaas").R2 === "klaas")
    assert(Payload("kostenvergelijking").R2 === "vergelijking")
    assert(Payload("aanmatigend").R2 === "igend")
  } 

  test("kostenvergelijking->kostenvergelijk") {
    val stem = DutchStemmer.stem("kostenvergelijking")
    assert(stem.word === "kostenvergelijk", "failed stemming, path is: %s".format(stem))
  }

  test("aanmatigend->aanmat") {
    val stem = DutchStemmer.stem("aanmatigend")
    assert(stem.word === "aanmat", "failed stemming, path is: %s".format(stem))
  }

  test("gebiedseigen->gebiedseig") {
    val stem = DutchStemmer.stem("gebiedseigen")
    assert(stem.word === "gebiedseig", "failed stemming, path is: %s".format(stem))
  }

  test("verify supplied vocabulary") {
    val doc = XML.loadFile("src/test/resources/test_input.xml")
    val tests = (doc \ "test")
    var count = 0
    tests.foreach { ele =>
      val input = ele.attribute("input").get.toString
      val output = ele.attribute("output").get.toString
      val stem = DutchStemmer.stem(input)
  
      assert(stem.word === output, "failed stemming[%s->%s] at: %d %s".format(input, output, count, stem))
      count = count + 1
    }
  }
}
