package definiti.native

object BooleanExtension {
  def not(boolean: Boolean): Boolean = !boolean

  def xor(boolean: Boolean, other: Boolean): Boolean = boolean && !other || !boolean && other
}
