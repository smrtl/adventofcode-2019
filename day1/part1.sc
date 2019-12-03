import scala.io.Source

def getFuel(mass: Int): Int = mass / 3 - 2

val result = Source
  .fromFile("input.txt")
  .getLines
  .map(_.toInt)
  .map(getFuel)
  .reduce(_ + _)

println(result)
