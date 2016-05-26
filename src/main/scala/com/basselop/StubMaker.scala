package com.basselop

import java.io.{BufferedWriter, OutputStream, OutputStreamWriter}

/**
  * Created by raul on 2016-05-26.
  */
class StubMaker(packages: Packages) {
  private def formatParams(s: Seq[Parameter]) = s.map{ _.descriptor }.mkString("(", "", ")")

  def writeToOutput(output: OutputStream = System.out) = {
    val out = new BufferedWriter(new OutputStreamWriter(output))
    for {
      p <- packages.packages
      c <- p.classes
      m <- c.members
    } {
      val thatStr = m match {
        case Constructor(typ, params) => s"<init>${formatParams(params)}"
        case Method(rtyp, name, params) => s"$name${formatParams(params)}${rtyp.descriptor}"
        case Field(typ, name) => name
        case EnumConstant(typ, name) => name
      }
      out.write(s"+${p.name}.${c.name}#$thatStr\n")
    }
  }
}
