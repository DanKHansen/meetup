import Schedule.Schedule
import java.time.{DayOfWeek, LocalDate}

case class Meetup(month: Int, year: Int):

   def day(dayOfWeek: Int, schedule: Schedule): LocalDate =
      val MonthSeq = createMonthMap(year, month).toSeq.sortBy(_._1)
      val ym = java.time.YearMonth.of(year, month)
      val dayOfMonth = schedule match
         case Schedule.Teenth => MonthSeq.filter(p => p._1 > 12 && p._1 < 20).filter(p => p._2 == dayOfWeek).head._1
         case Schedule.First  => MonthSeq.filter(p => p._1 >= 1 && p._1 < 8).filter(p => p._2 == dayOfWeek).head._1
         case Schedule.Second => MonthSeq.filter(p => p._1 >= 8 && p._1 < 15).filter(p => p._2 == dayOfWeek).head._1
         case Schedule.Third  => MonthSeq.filter(p => p._1 >= 15 && p._1 < 22).filter(p => p._2 == dayOfWeek).head._1
         case Schedule.Fourth => MonthSeq.filter(p => p._1 >= 22 && p._1 < 28).filter(p => p._2 == dayOfWeek).head._1
         case Schedule.Last   => MonthSeq.filter(p => p._1 >= 28 && p._1 < ym.lengthOfMonth).filter(p => p._2 == dayOfWeek).head._1


      LocalDate.of(year, month, dayOfMonth)

object Schedule extends Enumeration:
   type Schedule = Value
   val Teenth, First, Second, Third, Fourth, Last = Value

object Meetup:
   val Mon: Int = DayOfWeek.MONDAY.getValue
   val Tue: Int = DayOfWeek.TUESDAY.getValue
   val Wed: Int = DayOfWeek.WEDNESDAY.getValue
   val Thu: Int = DayOfWeek.THURSDAY.getValue
   val Fri: Int = DayOfWeek.FRIDAY.getValue
   val Sat: Int = DayOfWeek.SATURDAY.getValue
   val Sun: Int = DayOfWeek.SUNDAY.getValue

def createMonthMap(year: Int, month: Int): Map[Int, Int] =
   val firstDayOfMonth = LocalDate.of(year, month, 1)
   val lastDayOfMonth = firstDayOfMonth.lengthOfMonth()
   (1 to lastDayOfMonth).map { day =>
      val date = firstDayOfMonth.plusDays(day - 1)
      day -> date.getDayOfWeek.getValue
   }.toMap
