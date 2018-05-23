package definiti.scalatests.generator

import java.nio.file.Path

import definiti.scalamodel.utils.StringUtils
import definiti.scalatests.Configuration
import definiti.scalatests.ast._

class ScalaTestsProjectGenerator(config: Configuration) {
  def generateProject(testsRoot: Root, generatorsRoot: Root): Map[Path, String] = {
    val testsFiles = testsRoot.namespaces.flatMap(generateTestFile)
    val generatorsFiles = generatorsRoot.namespaces.flatMap(generateGeneratorFile)
    (testsFiles ++ generatorsFiles).toMap
  }

  private def generateTestFile(namespace: Namespace): Option[(Path, String)] = {
    if (namespace.elements.exists(_.isInstanceOf[Statement])) {
      val packageStatement = Some(namespace.name).filter(_.nonEmpty).map(PackageDeclaration)
      Some(buildScalaFile(
        s"${StringUtils.excludeLastPart(namespace.name)}.${StringUtils.lastPart(namespace.name).capitalize}Spec",
        Seq(
          packageStatement.toSeq,
          Seq(Blank),
          namespace.imports,
          Seq(Blank),
          namespace.elements
        )
          .flatten
          .map(ScalaTestsAstGenerator.generateStatement(_, ""))
          .mkString("\n")
      ))
    } else {
      None
    }
  }

  private def generateGeneratorFile(namespace: Namespace): Option[(Path, String)] = {
    if (namespace.elements.exists(_.isInstanceOf[Statement])) {
      val packageStatement = Some(namespace.name).filter(_.nonEmpty).map(PackageDeclaration)
      Some(buildScalaFile(
        s"${StringUtils.excludeLastPart(namespace.name)}.${StringUtils.lastPart(namespace.name).capitalize}Generators",
        Seq(
          packageStatement.toSeq,
          Seq(Blank),
          namespace.imports,
          Seq(Blank),
          namespace.elements
        )
          .flatten
          .map(ScalaTestsAstGenerator.generateStatement(_, ""))
          .mkString("\n")
      ))
    } else {
      None
    }
  }

  private def buildScalaFile(rawPath: String, content: String): (Path, String) = {
    val dirname = StringUtils.excludeLastPart(rawPath).replaceAllLiterally(".", "/")
    val filename = StringUtils.lastPart(rawPath, '.')
    val path = config.destination.resolve(dirname).resolve(s"$filename.scala")
    path -> content
  }
}
