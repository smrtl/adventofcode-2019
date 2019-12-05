import scala.io.Source
import $file.Common, Common._

def intersectionTotalLength(horizontal: Seg, vertical: Seg): Int =
  vertical.lengthBefore + (horizontal.start.y - vertical.start.y).abs +
    horizontal.lengthBefore + (vertical.start.x - horizontal.start.x).abs

def findShortestPathToIntersection(input: String): Int =
  findIntersections(input.trim.split('\n').map(parsePath), intersectionTotalLength)
    .filter(_ > 0)
    .min

def test(input: String, expected: Int) = {
  val result = findShortestPathToIntersection(input)
  assert(result == expected, s"$result != $expected")
}

test("R8,U5,L5,D3\nU7,R6,D4,L4", 30)
test("R75,D30,R83,U83,L12,D49,R71,U7,L72\nU62,R66,U55,R34,D71,R55,D58,R83", 610)

println(findShortestPathToIntersection(Source.fromFile("input.txt").mkString))
