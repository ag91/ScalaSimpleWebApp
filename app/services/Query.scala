package services
import com.github.tototoshi.csv._
import java.io._
import play.api.Play

case class Query(name_or_code: String)

object Query {
  // assuming here that the config contains these keys and that they point to existing file
  val csvCountries = Play.current.configuration.getString("csvCountriesPath").get
  val csvAirports  = Play.current.configuration.getString("csvAirportsPath").get
  val csvRunways   = Play.current.configuration.getString("csvRunwaysPath").get  

  def run (q : Query) : List[(String,List[String])] = {
    val r = CSVReader.open(new File(csvCountries))
    val cr = r.toStream
    val mc = cr.find(l => l(1).startsWith(q.name_or_code) || l(2).startsWith(q.name_or_code))
    val result : List[(String,List[String])] = {
      mc match {
        case None => Nil
        case Some(c) => // take first matching country
          val c_code = c(1)
          val c_continent = c(3)
          val r1 = CSVReader.open(new File(csvAirports))
          val ar = r1.toStream
          //both continent and code must match for an airport to be in a country
          val ma = ar.filter(l =>
            //I think the code is sufficient
            //l(7) == c_continent &&
            l(8) == c_code)
          val r2 = CSVReader.open(new File (csvRunways))
          val rr = r2.toStream
          val result = {
            ma.map(a => {
            val rs = rr.filter(l => l(1) == a(0))
            (a(3), rs.map(_(0)).toList) // only airport name and runways ids
            }).toList}
          val _ = r1.close()
          val _1 = r2.close()
          result
      }
    }
    val _ = r.close()
    result
  }
}
