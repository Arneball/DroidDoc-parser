package com.basselop


import java.io.FileOutputStream

import org.parboiled2._

import scala.util.{Failure, Success}

object Kritta extends App {
  val hc = """package com.arne.test {
             | public class Majskorv {
             |   method public static android.animation.PropertyValuesHolder ofFloat(android.util.Property<?, java.lang.Float>, float...);
             |   method public static java.lang.String majsa() throws java.lang.IOException;
             |   method public static java.util.List<T> parse(java.util.List<? extends T>);
             |   field public static int KALLE = 3;
             | }
             |}""".stripMargin
  lazy val file = io.Source.fromFile("/home/raul/public_api.txt").getLines().mkString("\n")

  def time[T](f: => T) = {
    val before = System.currentTimeMillis()
    val ret = f
    println(s"Time took: ${System.currentTimeMillis() - before} ms")
    ret
  }

  time {
    val parser = new MyParser(ParserInput(file))

    (parser.Root.run(): @unchecked) match {
      case Failure(e: ParseError) => println {
        parser.formatError(e, new ErrorFormatter(showTraces = true))
      }
      case Success(that) =>
        new StubMaker(that).writeToOutput(new FileOutputStream("/tmp/slask.txt"))
    }
  }
}