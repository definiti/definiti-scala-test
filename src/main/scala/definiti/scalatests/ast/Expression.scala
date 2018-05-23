package definiti.scalatests.ast

sealed trait Expression

sealed trait Unambiguous

case class Value(value: String) extends Expression with Unambiguous

case class StringExpression(string: String) extends Expression with Unambiguous

case class BinaryOp(op: String, left: Expression, right: Expression) extends Expression

case class UnaryOp(op: String, inner: Expression) extends Expression

case class IfThenElse(cond: Expression, ifTrue: Expression, ifFalse: Expression) extends Expression

case class CallAttribute(target: Expression, name: String) extends Expression with Unambiguous

case class CallMethod(
  target: Expression,
  name: String,
  arguments: Seq[Expression] = Seq.empty,
  generics: Seq[Type] = Seq.empty
) extends Expression with Unambiguous

case class CallFunction(
  target: Expression,
  arguments: Seq[Expression],
  generics: Seq[Type] = Seq.empty
) extends Expression with Unambiguous

case class CallHigherOrderFunction(
  target: Expression,
  arguments: Seq[Expression],
  functionArguments: Seq[String],
  functionBody: Expression,
  generics: Seq[Type] = Seq.empty
) extends Expression with Unambiguous

case class CallCaseClass(
  name: String,
  arguments: Seq[NamedArgument]
) extends Expression with Unambiguous

case class NamedArgument(argument: String, expression: Expression)

case class New(
  clazz: String,
  arguments: Seq[Expression],
  generics: Seq[Type]
) extends Expression with Unambiguous

case class ForComprehension(
  cases: Seq[ForComprehensionCase],
  yielding: Expression
) extends Expression

case class ForComprehensionCase(
  name: String,
  operator: String,
  body: Expression
)