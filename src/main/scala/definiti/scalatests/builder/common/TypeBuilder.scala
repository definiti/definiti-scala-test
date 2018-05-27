package definiti.scalatests.builder.common

import definiti.tests.{ast => testsAst}
import definiti.scalatests.{ast => scalaAst}

object TypeBuilder {
  private val nativeMap = Map(
    "Date" -> "LocalDateTime",
    "Number" -> "BigDecimal"
  )

  def buildType(typ: testsAst.Type): scalaAst.Type = {
    scalaAst.Type(
      name = nativeMap.getOrElse(typ.name, typ.name),
      generics = typ.generics.map(buildType)
    )
  }
}
