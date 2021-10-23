import scala.io.Source

object Xmas extends App {
    if (args.length != 2) {
        println("Input file and window size required!")
        System.exit(1)
    }

    val codes = Source.fromFile(args(0)).getLines().map(BigInt(_)).toList
    val invalid = codes.sliding(args(1).toInt + 1).find(! is_valid(_)).get.last
    println(invalid)

    def is_valid(sequence: List[BigInt]): Boolean = {
        sequence.init.combinations(2).map(_.sum).contains(sequence.last)
    }

    val valid_codes = codes.take(codes.indexOf(invalid))
    var weakness = BigInt(0)
    var size = 2
    while (weakness == 0 && size < valid_codes.size) {
        weakness = valid_codes.sliding(size).map(_.sum).indexOf(invalid) match {
            case -1 => 0
            case idx => val slice = valid_codes.slice(idx, idx + size - 1); slice.min + slice.max
        }
        size += 1
    }
    println(weakness)
}