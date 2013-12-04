package models

import java.io.{PrintWriter, File}
import org.joda.time.LocalDate
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

case class BaseSegmentation(
  source: String,
  lineNumber: Int,
  wordNumber: Int,
  word: String
)

case class Date(
  year: String,
  month: String,
  day: String
) {
  override def toString: String = s"$year-$month-$day"
  lazy val localDate = LocalDate.parse(this.toString)
}

object TempEval2 {
  val publicationDates = new mutable.HashMap[String, Date]()
  val outputAttributes = new mutable.HashMap[Int, (Int, String, String)]
  val outputExtents = new mutable.HashMap[Int, Int]
  val numberWords = List("[Zz]ero", "[Oo]ne", "[Tt]wo", "[Tt]hree", "[Ff]our", "[Ff]ive", "[Ss]ix", "[Ss]even", "[Ee]ight", "[Nn]ine", "[Tt]en", "[Ee]leven", "[Tt]welve", "[Tt]hirteen", "[Ff]ourteen", "[Ff]ifteen", "[Ss]ixteen", "[Ss]eventeen", "[Ee]ighteen", "[Nn]ineteen", "[Tt]wenty", "[Tt]wenty [Oo]ne", "[Tt]wenty [Tt]wo", "[Tt]wenty [Tt]hree", "[Tt]wenty [Ff]our")
  val monthWords = List("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")
  val dayNames = List("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
  val monthWordsShort = List("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sept?", "Oct", "Nov", "Dec")
  
  val matches =
    List(
      /*
       * JANUARY
       */
      ( // [Perfect] Ex: January 1983
        monthWords.mkString("^(", "|", ")( ,)?( |-)?[1-2]\\d{3}$").r,
        (value: String, segment: BaseSegmentation) =>
          s"${value.takeRight(4)}-${("0" + (1 + monthWords.indexWhere(_.r.findPrefixOf(value).isDefined))).takeRight(2)}",
        "DATE"
      )
      ,
      ( // [Perfect] Ex: Last January
        monthWords.mkString("^[Ll]ast (", "|", ")$").r,
        (value: String, segment: BaseSegmentation) => {
          val month = 1 + monthWords.indexWhere(_.r.findPrefixOf(value.drop(5)).isDefined)
          val minusYears =
            if (publicationDates(segment.source).localDate.getMonthOfYear > month) 0
            else 1

          s"${publicationDates(segment.source).localDate.minusYears(minusYears).toString("YYYY")}-${("0" + month).takeRight(2)}"
        },
        "DATE"
      )
      ,
      ( // [Perfect -1 val bug] In January last year
        monthWords.mkString("^[Ii]n (", "|", ") last year$").r,
        (value: String, segment: BaseSegmentation) =>
          s"${publicationDates(segment.source).localDate.minusYears(1).toString("YYYY")}-${("0" + (1 + monthWords.indexWhere(_.r.findPrefixOf(value.drop(3)).isDefined))).takeRight(2)}",
        "DATE"
      )
      ,
      ( // [Perfect] January this year
        monthWords.mkString("^(", "|", ") this year$").r,
        (value: String, segment: BaseSegmentation) =>
          s"${publicationDates(segment.source).year}-${("0" + (1 + monthWords.indexWhere(_.r.findPrefixOf(value).isDefined))).takeRight(2)}",
        "DATE"
      )
      ,
      ( // [Problem | FP] January 1, 1984
        // Gives "January 1 1984" while gold standard is "January , 1984"
        monthWords.mkString("^(", "|", ") \\d{1,2} (, )?[1-2]\\d{3}$").r,
        (value: String, segment: BaseSegmentation) =>
          s"${value.takeRight(4)}-${("0" + (1 + monthWords.indexWhere(_.r.findPrefixOf(value).isDefined))).takeRight(2)}-${("0" + value.dropRight(4).filter(_.isDigit)).takeRight(2)}",
        "DATE"
      )
      ,
      ( // [Perfect | -1 attr val can't fix] January 12
        monthWords.mkString("^(", "|", ") \\d{1,2}$").r,
        (value: String, segment: BaseSegmentation) =>
          s"${publicationDates(segment.source).year}-${("0" + (1 + monthWords.indexWhere(_.r.findPrefixOf(value).isDefined))).takeRight(2)}-${("0" + value.filter(_.isDigit)).takeRight(2)}",
        "DATE"
      )
      ,
      ( // [No test case] Jan 12 1998
        monthWordsShort.mkString("^(", "|", ") ?\\.? ?\\d{1,2} (, )?[1-2]\\d{3}$").r,
        (value: String, segment: BaseSegmentation) =>
          s"${value.takeRight(4)}-${("0" + (1 + monthWordsShort.indexWhere(_.r.findPrefixOf(value).isDefined))).takeRight(2)}-${("0" + value.dropRight(4).filter(_.isDigit)).takeRight(2)}",
        "DATE"
      )
      ,
      ( // [No test case] Jan 12 last year
        monthWordsShort.mkString("^(", "|", ") ?\\.? ?\\d{1,2} last year$").r,
        (value: String, segment: BaseSegmentation) =>
          s"${publicationDates(segment.source).localDate.minusYears(1).getYear}-${("0" + (1 + monthWordsShort.indexWhere(_.r.findPrefixOf(value).isDefined))).takeRight(2)}-${("0" + value.filter(_.isDigit)).takeRight(2)}",
        "DATE"
      )
      ,
      ( // [Good] Jan 12
        monthWordsShort.mkString("^(", "|", ") ?\\.? ?\\d{1,2}$").r,
        (value: String, segment: BaseSegmentation) =>
          s"${publicationDates(segment.source).year}-${("0" + (1 + monthWordsShort.indexWhere(_.r.findPrefixOf(value).isDefined))).takeRight(2)}-${("0" + value.filter(_.isDigit)).takeRight(2)}",
        "DATE"
      )
      ,
      ( // [OK] Jaunary
        monthWords.mkString("^(", "|", ")$").r,
        (value: String, segment: BaseSegmentation) => {
          val monthNumber = 1 + monthWords.indexWhere(_.r.findPrefixOf(value).isDefined)
          val publicationMonthNumber = publicationDates(segment.source).localDate.getMonthOfYear
          val subtractYears =
            if (publicationMonthNumber < monthNumber) 1
            else 0

          s"${publicationDates(segment.source).localDate.minusYears(subtractYears).getYear}-${("0" + monthNumber).takeRight(2)}"
        }
        ,
        "DATE"
      )
      ,

      /*
       * YEAR
       */
      (
        //
        numberWords.mkString("^([Aa]bout|[Aa]lmost|[Pp]robably|[Nn]early|[Rr]ougly) (", "|", ") [Yy]ears? ago$").r,
        (value: String, segment: BaseSegmentation) => {
          val monthName = value.split(" ")(1)
          val index = numberWords.indexWhere(_.r.findPrefixOf(monthName).isDefined)
          s"P${index}Y"
        },
        "DURATION"
        )
      ,
      (
        //
        numberWords.mkString("^(", "|", ") [Yy]ears? ago$").r,
        (value: String, segment: BaseSegmentation) => {
          publicationDates(segment.source)
            .localDate
            .minusYears(numberWords.indexWhere(_.r.findPrefixOf(value).isDefined))
            .getYear
            .toString
        },
        "DATE"
      )
      ,
      (
        // [Perfect] Two year
        numberWords.mkString("^(", "|", ")(-| )[Yy]ears?$").r,
        (value: String, segment: BaseSegmentation) => {
          val index = numberWords.indexWhere(_.r.findPrefixOf(value).isDefined)
          s"P${index}Y"
        },
        "DURATION"
      )
      ,
      (
        // [Perfect] 2 years
        "^\\d+(-| )[Yy]ears?$".r,
        (value: String, segment: BaseSegmentation) => {
          s"P${value.filter(_.isDigit)}Y"
        },
        "DURATION"
      )
      ,
      (
        // [Good] This year.
        "^[Tt]his year$".r,
        (value: String, segment: BaseSegmentation) => {
          publicationDates(segment.source).year
        },
        "DATE"
      )
      ,
      (
        // [Good] Last year.
        "^[Ll]ast year$".r,
        (value: String, segment: BaseSegmentation) => {
          publicationDates(segment.source).localDate.minusYears(1).getYear.toString
        },
        "DATE"
      )
      ,
      (
        // [OK | Problematic output] A year ago.
        "^[Aa] year ago$".r,
        (value: String, segment: BaseSegmentation) => {
          val quarter = 1 + ((publicationDates(segment.source).localDate.getMonthOfYear - 1) / 4)
          publicationDates(segment.source).localDate.minusYears(1).toString(s"YYYY-'Q$quarter'")
        },
        "DATE"
      )
      ,

      /*
       * MONTH
       */
      ( // [Perfect] A month ago
        "^[Aa] month ago$".r,
        (value: String, segment: BaseSegmentation) =>
          publicationDates(segment.source).localDate.minusMonths(1).toString("YYYY-MM"),
        "DATE"
      )
      ,
      ( // [OK | No test case] A month from now
        "^[Aa] month from now$".r,
        (value: String, segment: BaseSegmentation) =>
          publicationDates(segment.source).localDate.plusMonths(1).toString("YYYY-MM"),
        "DURATION"
      )
      ,
      ( // [OK] A month
        "^[Aa] month$".r,
        (value: String, segment: BaseSegmentation) => "P1M",
        "DURATION"
      )
      ,
      ( // [OK] The following / Next Month
        "^(([Tt]he )?[Ff]ollowing|[Nn]ext) month$".r,
        (value: String, segment: BaseSegmentation) =>
          publicationDates(segment.source).localDate.plusMonths(1).toString("YYYY-MM"),
        "DATE"
      )
      ,
      ( // [Perfect] The previous / last month
        "^(([Tt]he )?[Pp]revious|[Ll]ast) month$".r,
        (value: String, segment: BaseSegmentation) =>
          publicationDates(segment.source).localDate.minusMonths(1).toString("YYYY-MM"),
        "DATE"
      )
      ,
      ( // [Perfect] Two months ago
        numberWords.mkString("^(", "|", ") months? ago$").r,
        (value: String, segment: BaseSegmentation) => {
          val numberIndex = numberWords.indexWhere(_.r.findPrefixOf(value).isDefined)
          publicationDates(segment.source).localDate.minusMonths(numberIndex).toString("YYYY-MM")
        },
        "DATE"
      )
      ,
      ( // [OK | No test case] Two months from now.
        numberWords.mkString("^(", "|", ") months? from now").r,
        (value: String, segment: BaseSegmentation) => {
          val numberIndex = numberWords.indexWhere(_.r.findPrefixOf(value).isDefined)
          publicationDates(segment.source).localDate.plusMonths(numberIndex).toString("YYYY-MM")
        },
        "DATE"
      )
      ,
      ( // [No test case] 1 months ago
        "^\\d+ months? ago$".r,
        (value: String, segment: BaseSegmentation) =>
          publicationDates(segment.source).localDate.minusMonths(value.filter(_.isDigit).toInt).toString("YYYY-MM")
        ,
        "DATE"
      )
      ,
      ( // [No test case] 2 months form now
        "^\\d+ months? from now$".r,
        (value: String, segment: BaseSegmentation) =>
          publicationDates(segment.source).localDate.plusMonths(value.filter(_.isDigit).toInt).toString("YYYY-MM")
        ,
        "DATE"
      )
      ,
      ( // [Very good | 2 FP] (Past) two months
        numberWords.mkString("^(([Tt]he )?[Pp]ast )?(", "|", ")( |-)months?$").r,
        (value: String, segment: BaseSegmentation) => {
          val valueParts = value.split(" ")
          val numberIndex = numberWords.indexWhere(_.r.findPrefixOf(valueParts(valueParts.length - 2)).isDefined)
          s"P${numberIndex}M"
        },
        "DURATION"
      ),
      (
        // [Good] This month.
        "^[Tt]his month$".r,
        (value: String, segment: BaseSegmentation) => {
          publicationDates(segment.source).localDate.toString("YYYY-MM")
        },
        "DATE"
      )
      ,


      /*
       * WEEK
       */
      (
        // [Perfect | -1 AV] X weeks ago
        numberWords.mkString("^(", "|", ") [Ww]eeks? ago$").r,
        (value: String, segment: BaseSegmentation) => {
          publicationDates(segment.source)
            .localDate
            .minusWeeks(numberWords.indexWhere(_.r.findPrefixOf(value).isDefined))
            .toString
        },
        "DATE"
      )
      ,
      ( // [OK] A week ago (no test scenario)
        "^[Aa] week ago$".r,
        (value: String, segment: BaseSegmentation) =>
          publicationDates(segment.source).localDate.minusWeeks(1).toString("YYYY-MM-dd"),
        "DATE"
      )
      ,
      ( // [Good | -3 AV] This week
        "^[Tt]his week$".r,
        (value: String, segment: BaseSegmentation) =>
          publicationDates(segment.source).localDate.toString("YYYY-'W'ww"),
        "DATE"
      )
      ,
      ( //
        "^[Nn]ext week$".r,
        (value: String, segment: BaseSegmentation) =>
          publicationDates(segment.source).localDate.plusWeeks(1).toString("YYYY-'W'ww"),
        "DATE"
      )
      ,
      ( // [???] A week
        "^([Aa] )?week$".r,
        (value: String, segment: BaseSegmentation) => "P1W",
        "DURATION"
      )
      ,
      (
        // [Perfect] Two weeks
        numberWords.mkString("^(", "|", ") [Ww]eeks?$").r,
        (value: String, segment: BaseSegmentation) => {
          s"P${numberWords.indexWhere(_.r.findPrefixOf(value).isDefined)}W"
        },
        "DURATION"
      )
      ,

      /*
       * DAY
       */
      ( // [Perfect | -1 AV] Yesterday / A day ago
        "^([Yy]esterday|[Aa] day ago)$".r,
        (value: String, segment: BaseSegmentation) =>
          publicationDates(segment.source).localDate.minusDays(1).toString,
        "DATE"
      )
      ,
      ( // [GOOD | -3 AV, 2 FP] Today
        "^[Tt]oday$".r,
        (value: String, segment: BaseSegmentation) =>
          publicationDates(segment.source).toString,
        "DATE"
      )
      ,
      ( // [Perfect] Tomorrow
        "^[Tt]omorrow$".r,
        (value: String, segment: BaseSegmentation) =>
          publicationDates(segment.source).localDate.plusDays(1).toString,
        "DATE"
      )
      ,
      ( // [Perfect] 2 days ago
        "^\\d+ days? ago$".r,
        (value: String, segment: BaseSegmentation) =>
          publicationDates(segment.source).localDate.minusDays(value.filter(_.isDigit).toInt).toString,
        "DATE"
      )
      ,
      ( // [No test case] 2 from now
        "^\\d+ days? from now$".r,
        (value: String, segment: BaseSegmentation) =>
          publicationDates(segment.source).localDate.plusDays(value.filter(_.isDigit).toInt).toString,
        "DATE"
      )
      ,
      ( // [No test case] two days ago
        numberWords.mkString("^(", "|", ") days? ago$").r,
        (value: String, segment: BaseSegmentation) =>
          publicationDates(segment.source)
            .localDate
            .minusDays(numberWords.indexWhere(_.r.findPrefixOf(value).isDefined))
            .toString
        ,
        "DATE"
      )
      ,
      ( // [OK] day
        "^day$".r,
        (value: String, segment: BaseSegmentation) => "P1D",
        "DATE"
      )
      ,
      ( // [OK | 7 FP]
        numberWords.mkString("^(", "|", ")(-| )days?$").r,
        (value: String, segment: BaseSegmentation) =>
          s"P${numberWords.indexWhere(_.r.findPrefixOf(value).isDefined)}D"
        ,
        "DURATION"
      )
      ,
      ( // [Perfect] The last 24 hours
        "^(([Tt]he )?[Ll]ast )?(twenty ?four|24) hours".r,
        (value: String, segment: BaseSegmentation) => s"P1D",
        "DURATION"
      )
      ,

      /*
       * NUMERICS
       */
      (
        // [???] 1987-03
        "^([1-2]\\d\\d\\d)(-| )(\\d\\d)$".r,
        (value: String, segment: BaseSegmentation) => value,
        "DATE"
      )
      ,
      ( // [OK | -1 AV, 3 FP] 1998
        "^([1-2]\\d{3})$".r,
        (value: String, segment: BaseSegmentation) => value,
        "DATE"
      )
      ,
      /*
       * WORDS
       */
      ( // [Not great | 16 FP, also issue 3] Now
        "^[Nn]ow$".r,
        (value: String, segment: BaseSegmentation) => "PRESENT_REF",
        "DATE"
      )
      ,

      /*
       * MINUTE
       */
      ( // [No test case] 2 minutes
        "^\\d+ minutes?$".r,
        (value: String, segment: BaseSegmentation) =>
          s"PT${value.filter(_.isDigit)}M",
        "DURATION"
      )
      ,
      ( // [No test case] Two minutes
        numberWords.mkString("^(", "|", ")( |-)minutes?$").r,
        (value: String, segment: BaseSegmentation) => {
          val numberIndex = numberWords.indexWhere(_.r.findPrefixOf(value).isDefined)
          s"PT${numberIndex}M"
        },
        "DURATION"
      )
      ,
      ( // [Perfect] A few minutes
        "^[Aa] few minutes$".r,
        (value: String, segment: BaseSegmentation) => "PXM",
        "DURATION"
      )
      ,
      ( // [Perfect] Minute and half
        "^minute and a half$".r,
        (value: String, segment: BaseSegmentation) => "PT1M30S",
        "DURATION"
      )
      ,

      /*
       * HOUR
       */
      ( // [Perfect] (More / Less than) Two hours
        numberWords.mkString("^(([Ll]ess|[Mm]ore) than )?(", "|", ")( |-)hours?$").r,
        (value: String, segment: BaseSegmentation) => {
          val valueFix =
            if (value.slice(1, 4) == "ore" || value.slice(1, 4) == "ess") value.drop(10)
            else value

          s"PT${numberWords.indexWhere(_.r.findPrefixOf(valueFix).isDefined)}H"
        },
        "DURATION"
      )
      ,
      ( // [No test case] 2 hours
        "^\\d+( |-)hours?$".r,
        (value: String, segment: BaseSegmentation) => {
          s"PT${value.filter(_.isDigit)}H"
        },
        "DURATION"
      )
      ,
      ( // [No test case] Two hours
        numberWords.mkString("^(", "|", ")( |-)hours?$").r,
        (value: String, segment: BaseSegmentation) => {
          val numberIndex = numberWords.indexWhere(_.r.findPrefixOf(value).isDefined)
          s"PT${numberIndex}H"
        },
        "DURATION"
      )
      ,
      ( // [Perfect] (Nearly) an hour
        "^([Nn]early )?[Aa]n hour$".r,
        (value: String, segment: BaseSegmentation) => "PT1H",
        "DURATION"
      )
      ,

      /*
       * WEEKDAY
       */
      ( // [Good | -19 AV, -8 AT, -4 FP] Day name
        dayNames.mkString("^(", "|", ")$").r,
        (value: String, segment: BaseSegmentation) => {
          val publication = publicationDates(segment.source)
          val dayNumber = dayNames.indexOf(value) + 1
          val publicationDayNumber = publication.localDate.getDayOfWeek

          val addDays =
            if (dayNumber < publicationDayNumber) publicationDayNumber - dayNumber
            else if (dayNumber > publicationDayNumber) 7 - (dayNumber - publicationDayNumber)
            else 0

          publication.localDate.minusDays(addDays).toString
        },
        "DATE"
      )
    )

  def matchType(word: String, segment: BaseSegmentation): Option[(String, String)] = {
    matches find { case (regex, _, _) =>
      regex.findFirstIn(word).isDefined
    } map { case (_, value, kind) =>
      value(word, segment) -> kind
    }
  }

  def outputTimesAttributes(segment: BaseSegmentation, value: String, kind: String): Unit = {
    outputAttributes += segment.hashCode() -> (segment.wordNumber, value, kind)
  }

  val segmentBan = "^(,|[Ii]n)$".r

  def outputTimesExtents(segments: List[BaseSegmentation]): Unit = {
    for {
      segment <- segments
      if segmentBan.findFirstIn(segment.word).isEmpty
    } yield {
      outputExtents += segment.wordNumber -> segments.head.hashCode()
    }
  }.mkString

  def writeFiles(segments: List[BaseSegmentation], value: String, kind: String): Unit = {
    outputTimesAttributes(segments.find(s => segmentBan.findFirstIn(s.word).isEmpty).get, value, kind)
    outputTimesExtents(segments)
  }

  def apply(date: LocalDate, data: Seq[String]): (mutable.HashMap[Int, (Int, String, String)], mutable.HashMap[Int, Int]) = {
    val fakeId = System.currentTimeMillis().toString
    publicationDates.clear()
    outputAttributes.clear()
    outputExtents.clear()
    publicationDates += fakeId -> Date(date.getYear.toString, date.getMonthOfYear.toString, date.getDayOfMonth.toString)

    val lineSegments =
      for {
        (d, i) <- data.zipWithIndex
      } yield {
        BaseSegmentation(fakeId, 0, i, d)
      }

    var skip = 1

    for (segments <- lineSegments.sliding(5, 1) if {skip -= 1 ; skip == 0}) {
      skip = 1
      val segment1 = segments(0)
      val segment2 = segments.lift(1)
      val segment3 = segments.lift(2)
      val segment4 = segments.lift(3)
      val segment5 = segments.lift(4)

      val result5 =
        if (segment5.isEmpty) None
        else matchType(s"${segment1.word} ${segment2.get.word} ${segment3.get.word} ${segment4.get.word} ${segment5.get.word}", segment1)

      val result4 =
        if (result5.isDefined || segment4.isEmpty) None
        else matchType(s"${segment1.word} ${segment2.get.word} ${segment3.get.word} ${segment4.get.word}", segment1)

      val result3 =
        if (result5.isDefined || result4.isDefined || segment3.isEmpty) None
        else matchType(s"${segment1.word} ${segment2.get.word} ${segment3.get.word}", segment1)

      val result2 =
        if (result5.isDefined || result4.isDefined || result3.isDefined || segment2.isEmpty) None
        else matchType(s"${segment1.word} ${segment2.get.word}", segment1)

      val result1 =
        if (result5.isDefined || result4.isDefined || result3.isDefined || result2.isDefined) None
        else matchType(segment1.word, segment1)

      result5 map { case (value, kind) =>
        writeFiles(List(segment1, segment2.get, segment3.get, segment4.get, segment5.get), value, kind)
        skip = 5
      } getOrElse {
        result4 map { case (value, kind) =>
          writeFiles(List(segment1, segment2.get, segment3.get, segment4.get), value, kind)
          skip = 4
        } getOrElse {
          result3 map { case (value, kind) =>
            writeFiles(List(segment1, segment2.get, segment3.get), value, kind)
            skip = 3
          } getOrElse {
            result2 map { case (value, kind) =>
              writeFiles(List(segment1, segment2.get), value, kind)
              skip = 2
            } getOrElse {
              result1 foreach { case (value, kind) =>
                writeFiles(List(segment1), value, kind)
              }
            }
          }
        }
      }
    }

    outputAttributes -> outputExtents
  }
}