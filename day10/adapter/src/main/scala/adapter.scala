import scala.io.Source
import scala.collection.mutable

object Adapter extends App {
    if (args.length != 1) {
        println("Input file required!")
        System.exit(1)
    }

    val joltages = Source.fromFile(args(0)).getLines().map(_.toInt).toVector.sorted
    val diffs = joltages.sliding(2).map( pair => pair(1) - pair(0)).toVector ++ Vector(joltages.min, 3)
    println(diffs.count(_ == 1) * diffs.count(_ == 3))

    var cached_counts = mutable.Map[Vector[Int], BigInt]()
    println(count_arrangements(0 +: joltages))

    def count_arrangements(joltages: Vector[Int], max_step: Int = 3): BigInt = {
        if (cached_counts.contains(joltages)) {
            cached_counts(joltages)
        }
        else {
            val count: BigInt = joltages match {
                case x +: xs => if (xs.size == 0) 1 else ((xs.head - x) to max_step)
                                .map( step => count_arrangements(rest(xs, x + step)))
                                .sum
                case _ => 0
            }
            cached_counts(joltages) = count
            count
        }
    }

    def rest(joltages: Vector[Int], candidate: Int): Vector[Int] = {
        joltages.indexOf(candidate) match {
            case -1 => Vector()
            case idx => joltages.drop(idx)
        }
    }
}