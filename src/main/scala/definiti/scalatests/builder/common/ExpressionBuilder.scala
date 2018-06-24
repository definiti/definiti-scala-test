package definiti.scalatests.builder.common

import definiti.common.ast.NativeClassDefinition
import definiti.scalatests.builder.BuilderContext
import definiti.scalatests.{ast => scalaAst}
import definiti.tests.validation.helpers.ScopedExpression
import definiti.tests.{ast => testsAst}

object ExpressionBuilder {
  def buildSimpleExpression(scoped: ScopedExpression[testsAst.Expression])(implicit builderContext: BuilderContext): scalaAst.Expression = {
    scoped.expression match {
      case boolean: testsAst.BooleanExpression => scalaAst.Value(boolean.value.toString)
      case number: testsAst.NumberExpression => scalaAst.Value(number.value.toString())
      case string: testsAst.StringExpression => scalaAst.StringExpression(string.value.substring(1, string.value.length - 1))
      case reference: testsAst.Reference => scalaAst.Value(reference.target)
      case structure: testsAst.StructureExpression => buildStructure(ScopedExpression(structure, scoped))
      case methodCall: testsAst.MethodCall => buildMethodCall(ScopedExpression(methodCall, scoped))
      case attributeCall: testsAst.AttributeCall => buildAttributeCall(ScopedExpression(attributeCall, scoped))
      case condition: testsAst.Condition => buildCondition(ScopedExpression(condition, scoped))
      case binary: testsAst.Binary => buildBinary(ScopedExpression(binary, scoped))
      case _: testsAst.GenerationExpression => sys.error("Generation building impossible")
    }
  }

  private def buildStructure(structure: ScopedExpression[testsAst.StructureExpression])(implicit builderContext: BuilderContext): scalaAst.Expression = {
    scalaAst.CallCaseClass(
      name = structure.typ.name,
      arguments = structure.fields.map { field =>
        scalaAst.NamedArgument(field.name, buildSimpleExpression(field.expression))
      }
    )
  }

  private def buildMethodCall(methodCall: ScopedExpression[testsAst.MethodCall])(implicit builderContext: BuilderContext): scalaAst.Expression = {
    if (isNative(methodCall.inner)) {
      scalaAst.CallMethod(
        target = scalaAst.Value(s"${methodCall.inner.typeOfExpression.name}Extension"),
        name = methodCall.method,
        arguments = buildSimpleExpression(methodCall.inner) +: methodCall.arguments.map(buildSimpleExpression),
        generics = methodCall.generics.map(TypeBuilder.buildType)
      )
    } else {
      scalaAst.CallMethod(
        target = buildSimpleExpression(methodCall.inner),
        name = methodCall.method,
        arguments = methodCall.arguments.map(buildSimpleExpression),
        generics = methodCall.generics.map(TypeBuilder.buildType)
      )
    }
  }

  private def buildAttributeCall(attributeCall: ScopedExpression[testsAst.AttributeCall])(implicit builderContext: BuilderContext): scalaAst.Expression = {
    if (isNative(attributeCall.inner)) {
      scalaAst.CallMethod(
        target = scalaAst.Value(s"${attributeCall.inner.typeOfExpression.name}Extension"),
        name = attributeCall.attribute,
        arguments = Seq(buildSimpleExpression(attributeCall.inner)),
        generics = Seq.empty
      )
    } else {
      scalaAst.CallAttribute(
        target = buildSimpleExpression(attributeCall.inner),
        name = attributeCall.attribute
      )
    }
  }

  private def buildCondition(condition: ScopedExpression[testsAst.Condition])(implicit builderContext: BuilderContext): scalaAst.Expression = {
    scalaAst.IfThenElse(
      cond = buildSimpleExpression(condition.condition),
      ifTrue = buildSimpleExpression(condition.thenCase),
      ifFalse = buildSimpleExpression(condition.elseCase)
    )
  }

  private def buildBinary(binary: ScopedExpression[testsAst.Binary])(implicit builderContext: BuilderContext): scalaAst.Expression = {
    scalaAst.BinaryOp(
      binaryOperatorToString(binary.operator),
      buildSimpleExpression(binary.left),
      buildSimpleExpression(binary.right)
    )
  }

  def binaryOperatorToString(operator: testsAst.BinaryOperator.Value): String = {
    operator match {
      case testsAst.BinaryOperator.or => "||"
      case testsAst.BinaryOperator.and => "&&"
      case testsAst.BinaryOperator.lower => "<"
      case testsAst.BinaryOperator.lowerOrEqual => "<="
      case testsAst.BinaryOperator.upper => ">"
      case testsAst.BinaryOperator.upperOrEqual => ">="
      case testsAst.BinaryOperator.equal => "=="
      case testsAst.BinaryOperator.different => "!="
      case testsAst.BinaryOperator.plus => "+"
      case testsAst.BinaryOperator.minus => "-"
      case testsAst.BinaryOperator.time => "*"
      case testsAst.BinaryOperator.divide => "/"
      case testsAst.BinaryOperator.modulo => "%"
    }
  }

  def scopedExpression(expression: testsAst.Expression)(implicit builderContext: BuilderContext): ScopedExpression[testsAst.Expression] = {
    new ScopedExpression[testsAst.Expression](expression, Map.empty, Seq.empty, builderContext.generators, builderContext.library)
  }

  def isNative(expression: ScopedExpression[_ <: testsAst.Expression])(implicit builderContext: BuilderContext): Boolean = {
    builderContext.library.typesMap.get(expression.typeOfExpression.name).collect { case x: NativeClassDefinition => x }.isDefined
  }
}
