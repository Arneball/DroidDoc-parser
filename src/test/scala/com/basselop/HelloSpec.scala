

package com.basselop

import org.parboiled2.ParserInput
import org.scalatest.{FlatSpec, Matchers}

import scala.util.Success

class HelloSpec extends FlatSpec with Matchers {
  "Our parser" should "parse our mega library" in {
    val str = classOf[HelloSpec].getResourceAsStream("/public_api.txt")
    val testData = io.Source.fromInputStream(str).getLines().mkString("\n")
    new MyParser(ParserInput(testData)).Root.run() shouldBe a[Success[_]]
  }
}
