package controllers

import javax.inject._
import play.api.mvc._

@Singleton
class ReportController @Inject() extends Controller {

  def report = Action {
    // 10 countries with highest number of airports (with count)
    val cma : List[(String,(Int,List[String]))] = services.Report.get10cma
    //10 countries with lowest number of airports (with count)
    val cla : List[(String,(Int,List[String]))] = services.Report.get10cla
    // top 10 most common runway identifications (indicated in "le_ident" column)
    val mri : List[(String,Int)] = services.Report.get10mri
    Ok(views.html.report(cma,cla,mri))
       
  }

}
