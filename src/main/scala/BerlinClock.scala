/**
  * Created by abhishek on 01/06/17.
  */
object BerlinClock {

  def parseToBerlinTime(hour: Int, minute: Int, second: Int): Either[String, List[String]] = {

    (hour, minute, second) match {
      case _ if (0 to 23 contains hour) && (0 to 59 contains minute) && (0 to 59 contains second) =>
        val berlinTime = topRow(second) ::
          firstRow(hour) ::
          secondRow(hour) ::
          thirdRow(minute) ::
          fourthRow(minute) ::
          Nil
        Right(berlinTime)

      case _ => Left("Supplied values are not correct representation of time in HH:mm:ss format")
    }

  }

  def topRow(second: Int): String = {
    if (second % 2 == 0) "Y" else ""
  }

  def firstRow(hour: Int): String = {
    "R" * (hour / 5)
  }

  def secondRow(hour: Int): String = {
    "R" * (hour % 5)
  }

  def thirdRow(minute: Int): String = {
    val onQuaterLamps = minute / 15
    val onRemainingLamps = (minute % 15) / 5
    ("YYR" * onQuaterLamps) ++ ("Y" * onRemainingLamps)
  }

  def fourthRow(minute: Int): String = {
    "Y" * (minute % 5)
  }

}
