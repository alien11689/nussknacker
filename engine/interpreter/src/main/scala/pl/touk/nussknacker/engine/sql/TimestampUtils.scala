package pl.touk.nussknacker.engine.sql

import java.sql.Timestamp
import java.time.{LocalDateTime, ZoneId}

object TimestampUtils {

  def toTimestamp(localDateTime: LocalDateTime): Timestamp = {
    Timestamp.from(localDateTime.atZone(ZoneId.systemDefault()).toInstant)
  }

  def toLocalDateTime(t: Timestamp): LocalDateTime = {
    LocalDateTime.ofInstant(t.toInstant, ZoneId.systemDefault())
  }

}
