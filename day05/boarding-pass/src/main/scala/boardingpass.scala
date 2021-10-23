import scala.io.Source

object BoardingPass extends App {
    if (args.length != 1) {
        println("Input file required!")
        System.exit(1)
    }

    val seat_ids = Source.fromFile(args(0)).getLines().map(
        code => Integer.parseInt(code.map(c => if (c == 'F' || c == 'L') '0' else '1'), 2)
        ).toList
    println(seat_ids.max)

    val missing_seat_id = seat_ids.sorted.sliding(2).map(
        pair => if (pair(1) - pair(0) == 2) pair(0) + 1 else 0
        ).find(_ > 0).get
    println(missing_seat_id)
}