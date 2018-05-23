package definiti.native

import scala.math.BigDecimal.RoundingMode

object NumberExtension {
  @inline def abs(number: BigDecimal): BigDecimal = number.abs

  @inline def max(number: BigDecimal, other: BigDecimal): BigDecimal = number.max(other)

  @inline def min(number: BigDecimal, other: BigDecimal): BigDecimal = number.min(other)

  @inline def round(number: BigDecimal): BigDecimal = number.setScale(0, RoundingMode.HALF_UP).rounded

  @inline def floor(number: BigDecimal): BigDecimal = number.setScale(0, RoundingMode.FLOOR).rounded

  @inline def ceil(number: BigDecimal): BigDecimal = number.setScale(0, RoundingMode.CEILING).rounded

  @inline def roundWith(number: BigDecimal, decimals: BigDecimal): BigDecimal = number.setScale(decimals.toInt, RoundingMode.HALF_UP).rounded

  @inline def floorWith(number: BigDecimal, decimals: BigDecimal): BigDecimal = number.setScale(decimals.toInt, RoundingMode.FLOOR).rounded

  @inline def ceilWith(number: BigDecimal, decimals: BigDecimal): BigDecimal = number.setScale(decimals.toInt, RoundingMode.CEILING).rounded
}
