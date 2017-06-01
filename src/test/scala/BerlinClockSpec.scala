import org.scalatest.WordSpec
import org.scalatest.Matchers._

/**
  * Created by abhishek on 01/06/17.
  */
class BerlinClockSpec extends WordSpec {
  "Berlin clock" should {
    "Top Lamp should blink on/off every second" in {
      BerlinClock.topRow(1) shouldBe ""
      BerlinClock.topRow(10) shouldBe "Y"
    }

    "First Row should lit appropriately" in {
      BerlinClock.firstRow(1) shouldBe ""
      BerlinClock.firstRow(10) shouldBe "RR"
      BerlinClock.firstRow(14) shouldBe "RR"
      BerlinClock.firstRow(18) shouldBe "RRR"
      BerlinClock.firstRow(22) shouldBe "RRRR"
    }

    "Second Row should lit appropriately" in {
      BerlinClock.secondRow(1) shouldBe "R"
      BerlinClock.secondRow(10) shouldBe ""
      BerlinClock.secondRow(14) shouldBe "RRRR"
      BerlinClock.secondRow(18) shouldBe "RRR"
      BerlinClock.secondRow(22) shouldBe "RR"
    }

    "Third Row should lit appropriately" in {
      BerlinClock.thirdRow(1) shouldBe ""
      BerlinClock.thirdRow(10) shouldBe "YY"
      BerlinClock.thirdRow(25) shouldBe "YYRYY"
      BerlinClock.thirdRow(30) shouldBe "YYRYYR"
      BerlinClock.thirdRow(38) shouldBe "YYRYYRY"
      BerlinClock.thirdRow(45) shouldBe "YYRYYRYYR"
      BerlinClock.thirdRow(50) shouldBe "YYRYYRYYRY"
      BerlinClock.thirdRow(55) shouldBe "YYRYYRYYRYY"
      BerlinClock.thirdRow(58) shouldBe "YYRYYRYYRYY"
    }

    "Fourth Row should lit appropriately" in {
      BerlinClock.fourthRow(1) shouldBe "Y"
      BerlinClock.fourthRow(15) shouldBe ""
      BerlinClock.fourthRow(28) shouldBe "YYY"
      BerlinClock.fourthRow(59) shouldBe "YYYY"
    }

    "Parse to Berlin time" in {
      BerlinClock.parseToBerlinTime(15, 10, 5) shouldBe Right(List("", "RRR", "", "YY", ""))
      BerlinClock.parseToBerlinTime(21, 13, 6) shouldBe Right(List("Y", "RRRR", "R", "YY", "YYY"))
      BerlinClock.parseToBerlinTime(18, 21, 6) shouldBe Right(List("Y", "RRR", "RRR", "YYRY", "Y"))
      BerlinClock.parseToBerlinTime(23, 59, 59) shouldBe Right(List("", "RRRR", "RRR", "YYRYYRYYRYY", "YYYY"))
      BerlinClock.parseToBerlinTime(0, 0, 0) shouldBe Right(List("Y", "", "", "", ""))
    }

    "Fail to parse Berlin time" in {
      BerlinClock.parseToBerlinTime(25, 10, 5) shouldBe Left("Supplied values are not correct representation of time in HH:mm:ss format")
    }
  }
}
