import Schedule.Schedule
import java.time.{DayOfWeek, LocalDate}

case class Meetup(month: Int, year: Int):

   def day(dayOfWeek: Int, schedule: Schedule): LocalDate =
      val monthSeq = createMonthSeq(year, month).sortBy(_._1)
      val sa = monthSeq.sliding(7, 7).toArray
      val dayOfMonth = schedule match
         case Schedule.Teenth => monthSeq.filter(p => p._1 > 12 & p._1 < 20).filter(p => p._2 == dayOfWeek).head._1
         case Schedule.First  => sa(0).filter(p => p._2 == dayOfWeek).head._1
         case Schedule.Second => sa(1).filter(p => p._2 == dayOfWeek).head._1
         case Schedule.Third  => sa(2).filter(p => p._2 == dayOfWeek).head._1
         case Schedule.Fourth => sa(3).filter(p => p._2 == dayOfWeek).head._1
         case Schedule.Last   => monthSeq.filter(p => p._2 == dayOfWeek).last._1
      LocalDate.of(year, month, dayOfMonth)

   private def createMonthSeq(year: Int, month: Int): Seq[(Int, Int)] =
      val firstDayOfMonth = LocalDate.of(year, month, 1)
      val lastDayOfMonth = firstDayOfMonth.lengthOfMonth()
      (1 to lastDayOfMonth).map { day =>
         val date = firstDayOfMonth.plusDays(day - 1)
         day -> date.getDayOfWeek.getValue
      }

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
