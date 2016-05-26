package com.basselop

import java.io.{File, FileOutputStream}

import org.parboiled2.{ErrorFormatter, ParseError}

import scala.util.{Failure, Success}

case class Options(in: File = null, out: File = null, showTraces: Boolean = false)

/**
  * Created by raul on 2016-05-26.
  */
object Main {
  object Parser extends scopt.OptionParser[Options]("DroidDoc-parser") {
    private def validateFile(f: File) = if(f.getParentFile.exists()) success else failure(s"Parent dir to $f doesnt exist")
    opt[File]('i', "in") validate(validateFile) action { (f, opt) => opt.copy(in = f) }
    opt[File]('o', "out") validate(validateFile) action{ (f, opt) => opt.copy(out = f) }
    opt[Boolean]('d', "debug") optional() action{ (d, o) => o.copy(showTraces = d) }
  }

  def main(args: Array[String]) {
    val Some(options) = Parser.parse(args, Options())
    val inputStr = io.Source.fromFile(options.in).getLines().mkString("\n")
    val parser = new MyParser(inputStr)
    parser.Root.run() match {
      case Success(packages) =>
        val output = new FileOutputStream(options.out)
        try {
          new StubMaker(packages).writeToOutput(output)
        } finally output.close()
      case Failure(e: ParseError) =>
        println(parser.formatError(e, new ErrorFormatter(showTraces = options.showTraces)))
      case Failure(e) =>
        e.printStackTrace()
    }
  }
}
