import scala.io.Source
import $file.Common, Common._

def intersectionPoint(horizontal: Seg, vertical: Seg): Point =
  Point(vertical.start.x, horizontal.start.y)

def findClosestIntersection(input: String): Int =
  findIntersections(input.trim.split('\n').map(parsePath), intersectionPoint)
    .map(_.manhattan)
    .filter(_ > 0)
    .min

def test(input: String, expected: Int) = {
  val result = findClosestIntersection(input)
  assert(result == expected, s"$result != $expected")
}

test("R75,D30,R83,U83,L12,D49,R71,U7,L72\nU62,R66,U55,R34,D71,R55,D58,R83", 159)
test("R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51\nU98,R91,D20,R16,D67,R40,U7,R15,U6,R7", 135)

println(findClosestIntersection(Source.fromFile("input.txt").mkString))
