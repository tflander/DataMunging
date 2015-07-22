package todd
import scala.util.parsing.combinator.JavaTokenParsers

case class TemperatureData(
  day: Int,
  maxTemp: Int,
  minTemp: Int)
  
object TemperatureHelper {
  def findDayWithLeastTemperatureSpread(temperatures: List[TemperatureData]): TemperatureData = {
      val minTemperatureDiff = temperatures
    		  .map(temp => temp.maxTemp - temp.minTemp)
    		  .sortBy(_.toInt)
    		  .head
      return temperatures.filter(temperatureData => temperatureData.maxTemp - temperatureData.minTemp == minTemperatureDiff).head
  }
}

object MungerParser extends JavaTokenParsers {

  def readData(fileSpec: String): List[TemperatureData] = {

    val source = scala.io.Source.fromFile(fileSpec)
    val lines = source.getLines.drop(2).toList.dropRight(1)
    for (line <- lines) yield parse(line)
  }

  def parse(in: String) = {
    val r = parseAll(dataLine, in)
    if (!r.successful) {
      throw new IllegalArgumentException(r.toString)
    }
    r.get
  }

  val dayNum: Parser[Int] = wholeNumber ^^ (_.toInt)
  val temperature: Parser[Int] = wholeNumber ~ opt("*") ^^ { case n ~ s => (n.toInt) }
  val extraParams: Parser[List[String]] = rep("""\S""".r)
  val dataLine: Parser[TemperatureData] = dayNum ~ temperature ~ temperature ~ extraParams ^^
    { case d ~ mx ~ mn ~ f => TemperatureData(d, mx, mn) }
}

