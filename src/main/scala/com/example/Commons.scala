package com.example

import org.parboiled2._

import scala.collection.immutable.Seq

/**
  * Created by raul on 2016-05-26.
  */
trait Commons { self: MyParser =>

  // NAMES
  def packageName: Rule1[String] = rule {
    capture(oneOrMore(CharPredicate.AlphaNum | anyOf("_.")))
  }
  def methodName: Rule1[String] = packageName
  def validName = rule { CharPredicate.AlphaNum }
  def classname: Rule1[String] = packageName

  // Type argument parser
  def typeArg: Rule1[Parameter] = rule {
    boundedWildcard | wildcard | parameterizedType
  }
  def boundedWildcard: Rule1[Parameter] = rule {
    "?" ~ (" super " | " extends ") ~ parameterizedType
  }
  def wildcard: Rule1[Parameter] = rule { capture("?") ~> ((s: String) => Parameter.WildCard) }

  def typeArgs = rule {
    "<" ~ oneOrMore(typeArg).separatedBy(", ") ~ ">"
  }

  def parameterizedType = rule {
    packageName ~ optional(typeArgs) ~ zeroOrMore("[]") ~ optional("...") ~>
      ((nme: String, seq: Option[Seq[Parameter]]) => Parameter(nme, seq.getOrElse(Nil)))
  }

  def comment = rule {
    ps ~ "//" ~ ws ~ zeroOrMore(CharPredicate.Visible | ' ')
  }

  def throwsClause = rule {
    " throws " ~ oneOrMore(classname).separatedBy(", ")
  }

  // MODIFIERS
  def modifiers: Rule1[Seq[String]] = rule {
    oneOrMore(modifier).separatedBy(ws)
  }
  def modifier: Rule1[String] = rule {
    capture(
      "transient" |
      "volatile" |
      "public" |
      "static" |
      "abstract" |
      "protected" |
      "private" |
      "final" |
      "deprecated" |
      "synchronized"
    )
  }

  def fieldBody = rule {
    (" = " ~ literalConstant ~ ";") | ";"
  }

  // LITERAL STUFF
  def strLiteral = rule {
    "\"\"" |
      '"' ~ oneOrMore("\\\"" | "\\\\" | CharPredicate.Printable -- '"') ~ '"'
  }

  def numericLiteral = rule {
    oneOrMore(anyOf("(/*+-0123456789)lLfFeE."))
  }

  def literalConstant = rule {
    "true" | "false" | strLiteral | numericLiteral
  }

  // Whitespaces
  def s = CharPredicate(" ")
  def ps = rule { zeroOrMore(s) }
  def ws = CharPredicate(" \t\n")
  def pws = rule { zeroOrMore(ws) }


  def implements = rule {
    s ~ "implements" ~ s ~ oneOrMore(classname).separatedBy(" ")
  }

  def extend = rule {
    s ~ "extends" ~ s ~ classname
  }

}
