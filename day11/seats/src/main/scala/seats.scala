import scala.io.Source
import scala.math.signum

object Seats extends App {
    if(args.size != 1) {
        println("Input file required!")
        System.exit(1)
    }

    val empty_seat_map = Source.fromFile(args(0)).getLines().toVector.map(_.toVector)
    val row_count = empty_seat_map.size
    val row_length = empty_seat_map(0).size

    val directions = (-1 to 1).map( i => (-1 to 1).map( j => Tuple2(i, j)))
                            .flatten
                            .filter(_ != Tuple2(0, 0))

    println(solve(immediate_neighbor_idxs, 4))

    val visibile_seats = get_visible_seat_idxs(empty_seat_map)

    println(solve(visible_neighbor_idxs, 5))

    def visible_neighbor_idxs(row_idx: Int, seat_idx: Int) = {
        visibile_seats(row_idx)(seat_idx)
    }

    def immediate_neighbor_idxs(row_idx: Int, seat_idx: Int) = {
        apply_offsets(row_idx, seat_idx, directions).unzip._1
    }

    def apply_offsets(row_idx: Int, seat_idx: Int, offsets: IndexedSeq[(Int, Int)]) = {
        offsets
            .map(offset => Tuple2(offset._1 + row_idx, offset._2 + seat_idx))
            .zip(offsets)
            .filter({ case (idx, _) => (0 <= idx._1) && (idx._1 < row_count) && (0 <= idx._2) && (idx._2 < row_length) })
    }

    def solve(neighbor_idxs: (Int, Int) => IndexedSeq[(Int, Int)], occupancy_limit: Int) = {
        var next_seat_map = empty_seat_map
        var previous_seat_map = Vector[Vector[Char]]()
        while (next_seat_map != previous_seat_map) {
            // next_seat_map.foreach(row => println(row.mkString))
            // println()
            previous_seat_map = next_seat_map
            next_seat_map = (0 to row_count - 1)
                                .map( row_idx => (0 to row_length - 1)
                                    .map(seat_idx => update_seat(row_idx, seat_idx, previous_seat_map,
                                                                 neighbor_idxs(row_idx, seat_idx),
                                                                 occupancy_limit))
                                    .toVector).toVector
        }
        next_seat_map.map(_.count(_ == '#')).sum
    }

    def update_seat(row_idx: Int, seat_idx: Int, seat_map: Vector[Vector[Char]],
                    neighbor_idxs:IndexedSeq[(Int, Int)], occupancy_limit: Int) = {
        val neighbors = neighbor_idxs.map(seat => seat_map(seat._1)(seat._2))
        seat_map(row_idx)(seat_idx) match {
            case '.' => '.'
            case 'L' => if (neighbors.count(_ == '#') == 0) '#' else 'L'
            case '#' => if (neighbors.count(_ == '#') >= occupancy_limit) 'L' else '#'
        }
    }

    def get_visible_seat_idxs(seat_map: Vector[Vector[Char]]): Array[Array[Vector[(Int, Int)]]] = {
        var visible_seat_idx = Array.ofDim[Vector[(Int, Int)]](row_count, row_length)
        for (row_idx <- (0 to row_count - 1)) {
            for (seat_idx <- (0 to row_length - 1)) {
                visible_seat_idx(row_idx)(seat_idx) = Vector[(Int, Int)]()
                var neighbor_idxs_to_check = apply_offsets(row_idx, seat_idx, directions)
                while (! neighbor_idxs_to_check.isEmpty) {
                    val split_idxs = neighbor_idxs_to_check.partition({ case (seat, _) => seat_map(seat._1)(seat._2) != '.' })
                    visible_seat_idx(row_idx)(seat_idx) ++= split_idxs._1.unzip._1
                    neighbor_idxs_to_check = split_idxs._2
                                                .map({ case (seat, direction) => ((seat._1 + direction._1,
                                                                                   seat._2 + direction._2),
                                                                                  direction) })
                                                .filter({ case (idx, _) => (0 <= idx._1) && (idx._1 < row_count) && (0 <= idx._2) && (idx._2 < row_length) })
                }
            }
        }
        return visible_seat_idx
    }
}