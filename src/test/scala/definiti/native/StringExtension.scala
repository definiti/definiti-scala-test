package definiti.native

import java.util.regex.Pattern

object StringExtension {
  @inline def length(string: String): BigDecimal = string.length

  @inline def isEmpty(string: String): Boolean = string.isEmpty

  @inline def nonEmpty(string: String): Boolean = string.nonEmpty

  @inline def contains(string: String, other: String): Boolean = string.contains(other)

  @inline def startsWith(string: String, prefix: String): Boolean = string.startsWith(prefix)

  @inline def endsWith(string: String, suffix: String): Boolean = string.endsWith(suffix)

  @inline def matches(string: String, regex: String): Boolean = string.matches(regex)

  @inline def indexOf(string: String, substring: String): BigDecimal = string.indexOf(string)

  @inline def lastIndexOf(string: String, substring: String): BigDecimal = string.lastIndexOf(string)

  @inline def append(string: String, other: String): String = string + other

  @inline def prepend(string: String, other: String): String = other + string

  @inline def dropLeft(string: String, numberOfCharactersToDrop: BigDecimal): String = string.drop(numberOfCharactersToDrop.toInt)

  @inline def dropRight(string: String, numberOfCharactersToDrop: BigDecimal): String = string.dropRight(numberOfCharactersToDrop.toInt)

  @inline def trim(string: String): String = string.trim

  @inline def repeat(string: String, n: BigDecimal): String = string * n.toInt

  @inline def reverse(string: String): String = string.reverse

  @inline def substring(string: String, start: BigDecimal, length: BigDecimal): String = string.substring(start.toInt, string.length - length.toInt)

  @inline def toUpperCase(string: String): String = string.toUpperCase()

  @inline def toLowerCase(string: String): String = string.toLowerCase()

  @inline def replace(string: String, search: String, replacement: String): String = string.replace(search, replacement)

  @inline def replaceFirst(string: String, search: String, replacement: String): String = string.replaceFirst(Pattern.quote(search), replacement)

  @inline def replaceMatch(string: String, search: String, replacement: String): String = string.replaceAll(search, replacement)

  @inline def replaceFirstMatch(string: String, search: String, replacement: String): String = string.replaceFirst(search, replacement)
}
