package com.example

import org.parboiled2._

import scala.collection.immutable.Seq

/**
  * Created by raul on 2016-05-26.
  */

class MyParser(val input: ParserInput) extends Parser with Commons {
  def Root: Rule1[Packages] = rule {
    zeroOrMore(aPackage).separatedBy(pws) ~ EOI ~> ((l: Seq[Package]) => new Packages(l))
  }
  def aPackage: Rule1[Package] = rule {
    ("package" ~ s ~ packageName ~ s ~ "{\n" ~ zeroOrMore('\n') ~ classes ~ "}") ~> ((name: String, clz: Seq[Class]) => new Package(name, clz))
  }

  def classes: Rule1[Seq[Class]] = rule { zeroOrMore(clazz) }

  def clazz: Rule1[Class] = rule {
    ps ~ optional(modifiers) ~ (" class " | " interface ") ~ classname ~ optional(extend) ~ optional(implements) ~ " {" ~
      ws ~ zeroOrMore(methodOrField) ~
      ps ~ "}" ~ pws ~> ((mods: Option[Seq[String]], clname: String, ext: Option[String], impl: Option[Seq[String]], members: Seq[Member]) => Class(clname, members))
  }

  def methodOrField = rule { method | field | ctor | enum }

  def enum: Rule1[EnumConstant] = rule {
    ps ~ "enum_constant" ~ s ~ optional(modifiers) ~ s ~ parameterizedType ~ s ~ methodName ~ ";\n" ~>
      ((mods: Option[Seq[String]], typ: Parameter, nme: String) => EnumConstant(typ, nme))
  }

  def field: Rule1[Member] = rule {
    ps ~ "field" ~ s ~ modifiers ~ s ~ parameterizedType ~ s ~ methodName ~ fieldBody ~ optional(comment) ~ "\n" ~>
      ((mods: Seq[String], ret: Parameter, nme: String) => Field(ret, nme))
  }

  def method: Rule1[Member] = rule {
    (ps ~ "method" ~ s ~ modifiers ~ s ~ parameterizedType ~ optional(typeArgs) ~ s ~ methodName ~
      "(" ~ zeroOrMore(parameterizedType).separatedBy(", ") ~ ")" ~ optional(throwsClause) ~ ";\n") ~>
      ((mods: Seq[String], ret: Parameter, types: Option[Seq[Parameter]], mname: String, params: Seq[Parameter], opt: Option[Seq[String]]) => new Method(ret, mname, params))
  }

  def ctor = rule {
    ps ~ "ctor" ~ pws ~ modifiers ~ pws ~ classname ~ optional(typeArgs) ~ "(" ~ zeroOrMore(parameterizedType).separatedBy(", ") ~ ")" ~ optional(throwsClause) ~ ";\n" ~>
      ((mods: Seq[String], nme: String, types: Option[Seq[Parameter]], params: Seq[Parameter], throws: Option[Seq[String]]) => Constructor(nme, params))
  }
}