import java.time.{DayOfWeek, LocalDate}

import Schedule.Schedule

case class Meetup(month: Int, year: Int) {

  def day(dayOfWeek: Int, schedule: Schedule): LocalDate =
     println(dayOfWeek)
     println(schedule)

     val dayOfMonth = 13
     LocalDate.of(year, month, dayOfMonth)
}

object Schedule extends Enumeration {
  type Schedule = Value
  val Teenth, First, Second, Third, Fourth, Last = Value
}

object Meetup {
  val Mon: Int = DayOfWeek.MONDAY.getValue
  val Tue: Int = DayOfWeek.TUESDAY.getValue
  val Wed: Int = DayOfWeek.WEDNESDAY.getValue
  val Thu: Int = DayOfWeek.THURSDAY.getValue
  val Fri: Int = DayOfWeek.FRIDAY.getValue
  val Sat: Int = DayOfWeek.SATURDAY.getValue
  val Sun: Int = DayOfWeek.SUNDAY.getValue
}
