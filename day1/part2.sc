import scala.io.Source

def getTotalFuel(mass: Int): Int = {
  val fuel = mass / 3 - 2
  if (fuel <= 0) 0 else fuel + getTotalFuel(fuel)
}

val result = Source
  .fromFile("input.txt")
  .getLines
  .map(_.toInt)
  .map(getTotalFuel)
  .reduce(_ + _)

println(result)
