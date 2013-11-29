package controllers
import play.api.mvc._
import play.api.data._
import play.api.data.Forms._
import org.joda.time.LocalDate

object Application extends Controller {
  val inputForm =
    Form(
      tuple(
        "date" -> nonEmptyText.verifying( date => try {
          LocalDate.parse(date)
          true
        } catch {
          case _: Throwable => false
        }),
        "data" -> nonEmptyText
      )
    )

  def index = Action {
    Ok(views.html.index(inputForm, None))
  }

  def send = Action { implicit request =>
    val sentForm = inputForm.bindFromRequest()
    if (!sentForm.hasErrors) {
      val (date, data) = sentForm.get
      val dataParts = data
        .replace("\n", "\n~\n")
        .replace(" ", "\n")
        .replace(",", "\n,")
        .replace(".", "\n.")
        .replace("-", "\n-")
        .split("\n")

      val (attr, ext) = models.TempEval2(LocalDate.parse(date), dataParts)

      val result =
        for ((w, i) <- dataParts.zipWithIndex) yield {
          val lineExt = ext.get(i)
          if (lineExt.isDefined) {
            val wordAttr = attr(lineExt.get)
            val kind = if (wordAttr._3 == "DATE") "primary" else "success"
              s"""<a title="${wordAttr._2}"><span class="label label-$kind">$w</span></a>"""
          } else {
            w
          }

        }

      val resultHtml =
        result.mkString(" ")
          .replace("~", "<br />")
          .replace(" .", ".")
          .replace(" ,", ",")
          .replace(" -", "-")
      Ok(views.html.index(sentForm, Some(resultHtml)))
    } else {
      Ok(views.html.index(sentForm, None))
    }
  }
}