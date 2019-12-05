val Pattern = "^([UDLR])(\\d+)$".r

case class Point(x: Int, y: Int) {
  lazy val manhattan: Int = x.abs + y.abs
}

case class BoundingBox(x1: Int, y1: Int, x2: Int, y2: Int)

case class Seg(start: Point, length: Int, lengthBefore: Int, horizontal: Boolean) {
  lazy val end: Point =
    if (horizontal) Point(start.x + length, start.y)
    else Point(start.x, start.y + length)

  lazy val totalLength: Int =
    lengthBefore + length.abs

  lazy val bbox: BoundingBox =
    if (start.x > end.x || start.y > end.y) BoundingBox(end.x, end.y, start.x, start.y)
    else BoundingBox(start.x, start.y, end.x, end.y)

  def move(horizontally: Boolean, length: Int): Seg =
    Seg(end, length, totalLength, horizontally)

  override def toString: String =
    s"(${start.x},${start.y})->(${end.x},${end.y})"
}

def parsePath(path: String): Seq[Seg] =
  path
    .split(',')
    .scanLeft(Seg(Point(0, 0), 0, 0, true)) {
      case (seg, Pattern("U", l)) => seg.move(false, l.toInt)
      case (seg, Pattern("D", l)) => seg.move(false, -l.toInt)
      case (seg, Pattern("R", l)) => seg.move(true, l.toInt)
      case (seg, Pattern("L", l)) => seg.move(true, -l.toInt)
    }
    .tail

def intersect[T](horizontals: Seq[Seg], verticals: Seq[Seg], callback: (Seg, Seg) => T): Seq[T] =
  for {
    h <- horizontals;
    v <- verticals;
    if (v.bbox.x1 >= h.bbox.x1 && v.bbox.x1 <= h.bbox.x2 && h.bbox.y1 >= v.bbox.y1 && h.bbox.y1 <= v.bbox.y2)
  } yield callback(h, v)

def findIntersections[T](paths: Seq[Seq[Seg]], callback: (Seg, Seg) => T): Seq[T] = {
  assert(paths.length == 2)

  // Group by horizontal/vertical
  val grouped = paths.map(_.groupBy(_.horizontal))

  grouped
    .map(_(true)) // horizontal segments of path1, path2
    .zip(grouped.map(_(false)).reverse) // vertical segments of path2, path1
    .flatMap { case (horizontals, verticals) => intersect(horizontals, verticals, callback) }
}
