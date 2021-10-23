import scala.io.Source
import scala.collection.mutable

object Boot extends App {
    if (args.length != 1) {
        println("Input file required!")
        System.exit(1)
    }

    val lines = Source.fromFile(args(0)).getLines().toList
    val program_end = lines.size
    var result = execute(lines)
    println(result._1)  // solve1
    
    val program_lines = result._3

    var candidate_idx = 0
    while (result._2 < program_end && candidate_idx < program_lines.size) {
        val line_idx = program_lines(candidate_idx)
        if (! lines(line_idx).startsWith("acc")) result = execute(modify(lines, line_idx))
        candidate_idx += 1
    }
    println(result._1)  // solve2

    def execute(program: List[String]): Tuple3[Int, Int, List[Integer]] = {
        var next_line = 0
        var acc = 0
        var visited = mutable.Set[Integer]()

        while (! visited.contains(next_line) && (next_line < program_end)) {
            visited += next_line
            program(next_line).split(" ") match {
                case Array("nop", _) => next_line += 1
                case Array("jmp", arg) => next_line += arg.toInt
                case Array("acc", arg) => next_line += 1; acc += arg.toInt
            }
        }
        (acc, next_line, visited.toList)
    }

    def modify(lines: List[String], i: Int): List[String] = {
        var modified_lines = mutable.ListBuffer.empty ++ lines
        modified_lines(i) = lines(i).split(" ") match {
            case Array("nop", arg) => "jmp " + arg
            case Array("jmp", arg) => "nop " + arg
        }
        modified_lines.toList
    }
}