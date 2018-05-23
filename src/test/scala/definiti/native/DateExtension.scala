package definiti.native

import java.time.{LocalDateTime, ZoneOffset}

object DateExtension {
  @inline def timestamp(date: LocalDateTime): BigDecimal = date.toEpochSecond(ZoneOffset.UTC) + date.getNano

  @inline def year(date: LocalDateTime): BigDecimal = date.getYear

  @inline def month(date: LocalDateTime): BigDecimal = date.getMonthValue

  @inline def day(date: LocalDateTime): BigDecimal = date.getDayOfMonth

  @inline def hour(date: LocalDateTime): BigDecimal = date.getHour

  @inline def minute(date: LocalDateTime): BigDecimal = date.getMinute

  @inline def second(date: LocalDateTime): BigDecimal = date.getSecond

  @inline def millisecond(date: LocalDateTime): BigDecimal = nanoToMilli(date.getNano)

  @inline def plusYears(date: LocalDateTime, quantity: BigDecimal): LocalDateTime = date.plusYears(quantity.toLong)

  @inline def plusMonths(date: LocalDateTime, quantity: BigDecimal): LocalDateTime = date.plusMonths(quantity.toLong)

  @inline def plusDays(date: LocalDateTime, quantity: BigDecimal): LocalDateTime = date.plusDays(quantity.toLong)

  @inline def plusHours(date: LocalDateTime, quantity: BigDecimal): LocalDateTime = date.plusHours(quantity.toLong)

  @inline def plusMinutes(date: LocalDateTime, quantity: BigDecimal): LocalDateTime = date.plusMinutes(quantity.toLong)

  @inline def plusSeconds(date: LocalDateTime, quantity: BigDecimal): LocalDateTime = date.plusSeconds(quantity.toLong)

  @inline def plusMilleseconds(date: LocalDateTime, quantity: BigDecimal): LocalDateTime = date.plusNanos(milliToNano(quantity).toLong)

  @inline def minusYears(date: LocalDateTime, quantity: BigDecimal): LocalDateTime = date.minusYears(quantity.toLong)

  @inline def minusMonths(date: LocalDateTime, quantity: BigDecimal): LocalDateTime = date.minusMonths(quantity.toLong)

  @inline def minusDays(date: LocalDateTime, quantity: BigDecimal): LocalDateTime = date.minusDays(quantity.toLong)

  @inline def minusHours(date: LocalDateTime, quantity: BigDecimal): LocalDateTime = date.minusHours(quantity.toLong)

  @inline def minusMinutes(date: LocalDateTime, quantity: BigDecimal): LocalDateTime = date.minusMinutes(quantity.toLong)

  @inline def minusSeconds(date: LocalDateTime, quantity: BigDecimal): LocalDateTime = date.minusSeconds(quantity.toLong)

  @inline def minusMilleseconds(date: LocalDateTime, quantity: BigDecimal): LocalDateTime = date.minusNanos(milliToNano(quantity).toLong)

  @inline def isBefore(date: LocalDateTime, reference: LocalDateTime): Boolean = date.isBefore(reference)

  @inline def isAfter(date: LocalDateTime, reference: LocalDateTime): Boolean = date.isAfter(reference)

  @inline def isEqual(date: LocalDateTime, reference: LocalDateTime): Boolean = date.isEqual(reference)

  @inline def isBeforeOrEqual(date: LocalDateTime, reference: LocalDateTime): Boolean = date.isBefore(reference) || date.isEqual(reference)

  @inline def isAfterOrEqual(date: LocalDateTime, reference: LocalDateTime): Boolean = date.isAfter(reference) || date.isEqual(reference)

  private def milliToNano(milli: BigDecimal): BigDecimal = milli * 100000
  private def nanoToMilli(nano: BigDecimal): BigDecimal = nano / 100000
}