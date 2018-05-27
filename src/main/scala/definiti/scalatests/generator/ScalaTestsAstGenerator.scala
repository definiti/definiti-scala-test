package definiti.scalatests.generator

import definiti.scalatests.ast._

object ScalaTestsAstGenerator {
  def inc(indent: String) = s"$indent  "

  def inParens(ast: Expression, indent: String): String = ast match {
    case _: Unambiguous => generateExpression(ast, indent)
    case _ => s"(${generateExpression(ast, indent)})"
  }

  def generateStatement(ast: Statement, indent: String): String = {
    ast match {
      case ast: PackageDeclaration => generatePackageDeclaration(ast)
      case ast: Import => generateImport(ast)
      case ast: Expression => generateExpression(ast, indent)
      case ast: ClassDef => generateClassDef(ast, indent)
      case ast: ObjectDef => generateObjectDef(ast, indent)
      case ast: Def => generateDef(ast, indent)
      case Blank => ""
      case ast: TestDeclaration => generateTestDeclaration(ast, indent)
    }
  }

  def generatePackageDeclaration(ast: PackageDeclaration): String = {
    s"package ${ast.name}"
  }

  def generateImport(ast: Import): String = {
    s"import ${ast.name}"
  }

  def generateExpression(ast: Expression, indent: String): String = {
    ast match {
      case ast: Value => generateValue(ast)
      case ast: StringExpression => generateStringExpression(ast)
      case ast: BinaryOp => generateBinaryOp(ast, indent)
      case ast: UnaryOp => generateUnaryOp(ast, indent)
      case ast: IfThenElse => generateIfThenElse(ast, indent)
      case ast: CallAttribute => generateCallAttribute(ast, indent)
      case ast: CallMethod => generateCallMethod(ast, indent)
      case ast: CallFunction => generateCallFunction(ast, indent)
      case ast: CallHigherOrderFunction => generateCallHigherOrderFunction(ast, indent)
      case ast: CallCaseClass => generateCallCaseClass(ast, indent)
      case ast: New => generateNew(ast, indent)
      case ast: ForComprehension => generateForComprehension(ast, indent)
    }
  }

  def generateValue(ast: Value): String = ast.value

  def generateStringExpression(ast: StringExpression): String = {
    s""""${ast.string}""""
  }

  def generateBinaryOp(ast: BinaryOp, indent: String): String = {
    s"${inParens(ast.left, indent)} ${ast.op} ${inParens(ast.right, indent)}"
  }

  def generateUnaryOp(ast: UnaryOp, indent: String): String = {
    s"${ast.op} ${inParens(ast.inner, indent)}"
  }


  def generateIfThenElse(ast: IfThenElse, indent: String): String = {
    s"""if (${generateExpression(ast.cond, indent)}) {
       |${indent}  ${generateExpression(ast.ifTrue, indent)}
       |${indent}} else {
       |${indent}  ${generateExpression(ast.ifFalse, indent)}
       |${indent}}""".stripMargin
  }

  def generateParameter(ast: Parameter): String = {
    s"${ast.name}: ${generateType(ast.typ)}"
  }

  def generateCallAttribute(ast: CallAttribute, indent: String): String = {
    s"${inParens(ast.target, indent)}.${ast.name}"
  }

  def generateCallMethod(ast: CallMethod, indent: String): String = {
    val target = s"${inParens(ast.target, indent)}"
    val method = s"${ast.name}"
    val generics = generateTypeGenerics(ast.generics)
    val arguments =
      if (ast.arguments.nonEmpty) ast.arguments.map(generateExpression(_, indent)).mkString("(", ", ", ")")
      else ""
    s"${target}.${method}${generics}${arguments}"
  }

  def generateCallFunction(ast: CallFunction, indent: String): String = {
    val inner = generateExpression(ast.target, indent)
    val generics = generateTypeGenerics(ast.generics)
    val arguments = ast.arguments.map(generateExpression(_, indent)).mkString(", ")
    s"${inner}${generics}(${arguments})"
  }

  def generateCallHigherOrderFunction(ast: CallHigherOrderFunction, indent: String): String = {
    val target = generateExpression(ast.target, indent)
    val arguments = ast.arguments.map(generateExpression(_, indent)).mkString(", ")
    val generics = generateTypeGenerics(ast.generics)
    s"""${target}${generics}(${arguments}) { (${ast.functionArguments.mkString(", ")}) =>
       |${inc(indent)}${generateExpression(ast.functionBody, inc(indent))}
       |${indent}}""".stripMargin
  }

  def generateCallCaseClass(ast: CallCaseClass, indent: String): String = {
    val arguments = ast.arguments.map { argument =>
      s"${argument.argument} = ${generateExpression(argument.expression, inc(indent))}"
    }.mkString(s",\n${inc(indent)}")
    s"""${ast.name}(
       |${inc(indent)}${arguments}
       |${indent})""".stripMargin
  }

  def generateNew(ast: New, indent: String): String = {
    val generics = if (ast.generics.nonEmpty) ast.generics.mkString("[", ", ", "]") else ""
    val parameters = ast.arguments.map(a => generateExpression(a, indent)).mkString(", ")
    s"new ${ast.clazz}${generics}(${parameters})"
  }

  def generateForComprehension(ast: ForComprehension, indent: String): String = {
    val cases = ast.cases.map(generateForComprehensionCase(_, indent)).mkString(s"\n${inc(indent)}")
    val yielding = generateExpression(ast.yielding, inc(indent))
    s"""for {
       |${inc(indent)}${cases}
       |${indent}} yield {
       |${inc(indent)}${yielding}
       |${indent}}""".stripMargin
  }

  def generateForComprehensionCase(ast: ForComprehensionCase, indent: String): String = {
    s"""${ast.name} ${ast.operator} ${generateExpression(ast.body, inc(indent))}"""
  }

  private def generateGenerics(generics: Seq[String]): String = {
    if (generics.nonEmpty) {
      generics.mkString("[", ",", "]")
    } else {
      ""
    }
  }

  private def generateType(typ: Type): String = {
    s"${typ.name}${generateTypeGenerics(typ.generics)}"
  }

  private def generateTypeGenerics(generics: Seq[Type]): String = {
    if (generics.nonEmpty) {
      generics.map(generateType).mkString("[", ",", "]")
    } else {
      ""
    }
  }

  def generateClassDef(ast: ClassDef, indent: String): String = {
    val extendz = if (ast.extendz.nonEmpty) s" extends ${ast.extendz.mkString(" with ")}" else ""
    s"""class ${ast.name}${extendz} {
       |${inc(indent)}${ast.body.map(generateStatement(_, inc(indent))).mkString(s"\n${inc(indent)}")}
       |${indent}}
     """.stripMargin
  }

  def generateObjectDef(ast: ObjectDef, indent: String): String = {
    val extendz = if (ast.extendz.nonEmpty) s" extends ${ast.extendz.mkString(" with ")}" else ""
    s"""object ${ast.name}${extendz} {
       |${inc(indent)}${ast.body.map(generateStatement(_, inc(indent))).mkString(s"\n${inc(indent)}")}
       |${indent}}
     """.stripMargin
  }

  def generateDef(ast: Def, indent: String): String = {
    s"""def ${ast.name}${generateGenerics(ast.generics)}(${ast.parameters.map(generateParameter).mkString(", ")}): ${generateType(ast.typ)} = {
       |${inc(indent)}${generateExpression(ast.body, inc(indent))}
       |${indent}}""".stripMargin
  }

  def generateTestDeclaration(ast: TestDeclaration, indent: String): String = {
    val subject = if (ast.subject == "it") "it" else s""""${ast.subject}""""
    s"""${subject} should "${ast.name}" in {
       |${inc(indent)}${generateExpression(ast.body, inc(indent))}
       |${indent}}""".stripMargin
  }
}
