package definiti.scalatests.ast

case class Root(namespaces: Seq[Namespace])

case class Namespace(
  name: String,
  imports: Seq[Import],
  elements: Seq[Statement]
)

trait Statement

case object Blank extends Statement

case class PackageDeclaration(name: String) extends Statement

case class Import(name: String) extends Statement

case class ClassDef(
  name: String,
  extendz: Seq[String] = Seq.empty,
  body: Seq[Statement] = Seq.empty
) extends Statement

case class ObjectDef(
  name: String,
  extendz: Seq[String] = Seq.empty,
  body: Seq[Statement] = Seq.empty
) extends Statement

case class Def(
  name: String,
  typ: Type,
  body: Expression,
  generics: Seq[String] = Seq.empty,
  parameters: Seq[Parameter] = Seq.empty
) extends Statement

case class Type(name: String, generics: Seq[Type])

case class Parameter(name: String, typ: Type)

case class TestDeclaration(subject: String, name: String, body: Expression) extends Statement