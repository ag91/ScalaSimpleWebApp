package services
import com.github.tototoshi.csv._
import java.io._
import play.api.Play

object Report {

  // assuming here that the config contains these keys and that they point to existing file
  val csvCountries = Play.current.configuration.getString("csvCountriesPath").get
  val csvAirports  = Play.current.configuration.getString("csvAirportsPath").get
  val csvRunways   = Play.current.configuration.getString("csvRunwaysPath").get  


  def getMapAirportsCount(cr : Stream[List[String]]) : Map[String,(Int,List[String])] = {
    cr.foldLeft(Map[String,(Int,List[String])]())((acc,l) => {
      val a_id = l(0)
      val a_country = l(8)
      val k = a_country
      def inc (x : (Int,List[String])) : (Int,List[String]) = {
        val (c,l) = x
        (c + 1, a_id :: l)
      }
      val v = inc (acc getOrElse(k, (0,List())))
      acc + (k -> v)})}

  def getMapCountryIdName : Map[String,String] = {
    val r = CSVReader.open(new File(csvCountries))
    val cr = r.toStream
    val result = {
      cr.foldLeft(Map[String,String]())((acc,l) => {
        val c_code = l(1)
        val c_name = l(2)
        acc + (c_code -> c_name)
      })}
    val _ = r.close()
    result
    }

  def getMapAirportIdSurface : Map[String,String] = {
    val r2 = CSVReader.open(new File(csvRunways))
    val rr = r2.toStream
    val result = rr.foldLeft(Map[String,String]())((acc,l) => {
      val a_id = l(1)
      val surface = l(5)
      acc + (a_id -> surface)
    })
    val _ = r2.close()
    result
  }

  def getListCountriesWithCount = {
    val r1 = CSVReader.open(new File(csvAirports))
    val cr = r1.toStream
    // get map of airports count
    val m = getMapAirportsCount(cr)
    // get map (country -> country name)
    val m1 = getMapCountryIdName
    // get map (a_id -> surface)
    val m2 = getMapAirportIdSurface
    // substitute key (code,continent) with country name
    m.toList.map(x => {
      val (k,(c,l)) = x
      val l1 = l.map(a_id => m2 getOrElse(a_id,s"Surface relative to airport id $a_id does not exist"))
      // we do not want duplicates nor missing surfaces
      val l2 = l1.filterNot(s => s.startsWith("Surface relative to airport id")).distinct
      (m1 getOrElse(k, s"Country name relative to country code $k does not exist"),(c,l2))
    })
  }

  // 10 countries with highest number of airports (with count)
  def get10cma : List[(String,(Int,List[String]))] = {
    //get list of countries with airport count
    val cas : List[(String,(Int,List[String]))] = getListCountriesWithCount
    cas.sortWith((a,b) => b._2._1 < a._2._1).take(10)
  }

  //10 countries with lowest number of airports (with count)
  def get10cla : List[(String,(Int,List[String]))] = {
    //get list of countries with airport count
    val cas : List[(String,(Int,List[String]))] = getListCountriesWithCount
    cas.sortWith((a,b) => b._2._1 < a._2._1).takeRight(10)}

  // top 10 most common runway identifications (indicated in "le_ident" column)
  def get10mri : List[(String,Int)] = {
    val mri : List[(String,Int)] = {
      val r2 = CSVReader.open(new File (csvRunways))
      val rr = r2.toStream
      val m = rr.foldLeft(Map[String,Int]())((acc, l) => {
        val k = l(8) //position of "le_indent" header
        val v = 1 + (acc getOrElse(k, 0))
        acc + (k -> v)})
      val result = m.toList
      val _ = r2.close()
      result
    }
    mri.sortWith((a,b) => b._2 < a._2).take(10)
  }

}
