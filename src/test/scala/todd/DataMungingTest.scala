package todd

import org.scalatest._

class DataMungingTest extends FunSpec with ShouldMatchers {

  describe("line parsing") {

    val dataLine = "   1  88    59    74          53.8       0.00 F       280  9.6 270  17  1.6  93 23 1004.5"
    val dataLineWithStar = "   9  86    32*   59       6  61.5       0.00         240  7.6 220  12  6.0  78 46 1018.6"

    it("parses a data line") {
      MungerParser.parse(dataLine) should be(TemperatureData(1, 88, 59))
    }

    it("parses a data line with star") {
      MungerParser.parse(dataLineWithStar) should be(TemperatureData(9, 86, 32))
    }
    
  }

  describe("data reading") {
    it("should read data") {
      val temperatures = MungerParser.readData("weather.dat")
      temperatures.size should be(30)
    }
    
    it("should find the least change in temperature") {
      val temperatures = MungerParser.readData("weather.dat")
      val leastChange = TemperatureHelper.findDayWithLeastTemperatureSpread(temperatures)
      leastChange should be (TemperatureData(14,61,59))
    }
  }

}