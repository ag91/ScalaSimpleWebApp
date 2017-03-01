package controllers

import javax.inject._
import play.api.mvc._
import play.api.data._
import play.api.data.Forms._
import play.api.Play.current
import play.api.i18n.Messages.Implicits._

@Singleton
class QueryController extends Controller {

  val queryForm = Form(single("query" -> nonEmptyText))

  def query = Action {
    Ok(views.html.query(queryForm))
  }

  def submit() = Action {implicit request =>
    queryForm.bindFromRequest().fold(
      formWithErrors => BadRequest("Your query cannot be empty"),
      query => {
        val newQuery = services.Query(query)
        val results = services.Query.run(newQuery)
        if (results == Nil) {
          Ok(new play.twirl.api.Html("No results found for: "+query))
        }
        else
          Ok(views.html.queryResults(query,results))
      }
    )
  }

}
