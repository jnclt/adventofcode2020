import scala.io.Source
import scala.math.sin, scala.math.cos, scala.math.toRadians

object Manhattan extends App {
    if (args.size != 1) {
        println("Input file required!")
        System.exit(1)
    }

    val instructions = Source.fromFile(args(0)).getLines().toVector
    val manhattan_dist1 = instructions.foldLeft(List(90, 0, 0))(execute1).tail.map(_.abs).sum
    println(manhattan_dist1)

    val ship_start = List(0, 0)
    val waypoint_start = (10, 1)
    val manhattan_dist2 = instructions.foldLeft((ship_start, waypoint_start))(execute2)._1.map(_.abs).sum
    println(manhattan_dist2)

    def execute1(state: List[Int], instruction: String): List[Int] = {
        val maneuver = instruction.head
        val value = instruction.tail.toInt
        maneuver match {
            case 'L' => (state.head - value) :: state.tail
            case 'R' => (state.head + value) :: state.tail
            case 'F' => state.head :: List(state(1) + (value * sin(toRadians(state.head)).toInt),  // x
                                           state(2) + (value * cos(toRadians(state.head)).toInt))  // y
            case 'N' => state.head :: List(state(1), state(2) + value)
            case 'S' => state.head :: List(state(1), state(2) - value) 
            case 'E' => state.head :: List(state(1) + value, state(2)) 
            case 'W' => state.head :: List(state(1) - value, state(2)) 
        }
    }

    def execute2(state: (List[Int], (Int, Int)), instruction: String): (List[Int], (Int, Int)) = {
        val maneuver = instruction.head
        val value = instruction.tail.toInt
        maneuver match {
            case 'F' => (List(state._1(0) + value * state._2._1, 
                              state._1(1) + value * state._2._2),
                         state._2)
            case 'L' => (state._1, rotate(state._2, (-value + 360) % 360))
            case 'R' => (state._1, rotate(state._2, value % 360))
            case 'N' => (state._1, (state._2._1, state._2._2 + value))
            case 'S' => (state._1, (state._2._1, state._2._2 - value))
            case 'E' => (state._1, (state._2._1 + value, state._2._2))
            case 'W' => (state._1, (state._2._1 - value, state._2._2))
        }
    }

    def rotate(position: (Int, Int), angle: Int): (Int, Int) = {
        // theta (polar) starts at 12 o'clock, counts clockwise
        val theta = math.atan2(position._1, position._2) + toRadians(angle)
        val r = math.hypot(position._1, position._2)
        // cos/sin start at 3 o'clock, count counterclockwise
        ((r * cos(toRadians(90) - theta)).round.toInt, (r * sin(toRadians(90) - theta)).round.toInt)
    }
}
