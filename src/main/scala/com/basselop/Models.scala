package com.basselop

import scala.collection.immutable.Seq

/**
  * Created by raul on 2016-05-26.
  */
case class Packages(packages: Seq[Package])
case class Package(name: String, classes: Seq[Class])
case class Class(name: String, members: Seq[Member])

sealed trait Member
case class Method(rtyp: Parameter, name: String, params: Seq[Parameter]) extends Member
case class Field(typ: Parameter, name: String) extends Member
case class Constructor(typ: String, params: Seq[Parameter]) extends Member
case class EnumConstant(typ: Parameter, name: String) extends Member

case class Parameter(typ: String, typeParameters: Seq[Parameter]) {
  override def toString = {
    if(typeParameters.nonEmpty) s"$typ${typeParameters.mkString("<", ", ", ">")}"
    else typ
  }
  def descriptor = typ match {
    case "byte" => "B"
    case "char" => "C"
    case "double" => "D"
    case "float" => "F"
    case "short" => "S"
    case "void" => "V"
    case "int" => "I"
    case "long" => "J"
    case "boolean" => "Z"
    case _ => s"L$typ;"
  }
}
object Parameter {
  object WildCard extends Parameter("*", Nil)
}