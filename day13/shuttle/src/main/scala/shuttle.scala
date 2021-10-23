import scala.io.Source

object Shuttle extends App {
    if(args.size != 1) {
        println("Input file required!")
        System.exit(1)
    }

    val input = Source.fromFile(args(0)).getLines().toList
    val timestamp = input(0).toInt
    val intervals = input(1).split(",").filter(_ != "x").map(_.toInt).toVector

    val min_waiting_time = intervals.map(interval => waiting_time(interval, timestamp) -> interval).toMap.min
    println(min_waiting_time._1 * min_waiting_time._2)

    def waiting_time(interval: Int, time: Int): Int = {
        ((time / interval) + 1) * interval - time
    }

    val intervals2 = input(1)
                        .split(",")
                        .zipWithIndex
                        .map { case (interval, idx) => if (interval != "x") (interval.toInt -> idx) }
                        .filter(_ != ())
                        .toVector
                        .asInstanceOf[Vector[(Int, Int)]]
    val min_waiting_time2 = intervals2.foldLeft((BigInt(1), BigInt(1)))(waiting_time2(_, _))._1
    println(min_waiting_time2)

    def waiting_time2(left: (BigInt, BigInt), right: (Int, Int)): (BigInt, BigInt) = {
        var candidate = left._1
        val step = left._2
        val next = right._1
        val remainder = right._2
        while ((candidate + remainder) % next != 0) {
            candidate += step
        }
        (candidate, step * next)
    }
}