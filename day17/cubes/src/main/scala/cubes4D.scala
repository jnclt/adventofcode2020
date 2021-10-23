import scala.io.Source

object Cubes4D extends App {
    if (args.length != 2) {
        println("Number of steps and input file required!")
        System.exit(1)
    }

    case class Cell(w: Int, z: Int, y: Int, x: Int)
    type Space = Set[Cell]

    val steps = args(0).toInt
    val lines = Source.fromFile(args(1)).getLines().toVector

    val (start_space, size) = init_space(lines, steps)
    val final_space = (steps - 1 to 0 by -1).foldLeft(start_space)( (space, offset) => step(size)(space, offset))
    println(final_space.size)

    def step(size: Cell)(space: Space, offset: Int): Space = {
        println(s"step ${offset}: ==================")
        val next =
            for (w <- offset to (size.w - 1 - offset);
                 z <- offset to (size.z - 1 - offset);
                 y <- offset to (size.y - 1 - offset);
                 x  <- offset to (size.x -1 - offset);
                 cell = Cell(w, z, y, x);
                 neighbors_count = count_neighbors(space, cell);
                 if ((space.contains(cell) && (Vector(2, 3) contains neighbors_count)) ||
                     (!space.contains(cell) && (neighbors_count == 3))))
                     yield cell
        next.toSet
    }

    def count_neighbors(space: Space, cell: Cell): Int = {
        val neighborhood = 
            for (w <- cell.w - 1 to cell.w + 1;
                 z <- cell.z - 1 to cell.z + 1;
                 y <- cell.y - 1 to cell.y + 1;
                 x <- cell.x - 1 to cell.x + 1)
                 yield Cell(w, z, y, x)
        (space & neighborhood.toSet - cell).size
    }

    def init_space(lines: Vector[String], steps: Int): (Space, Cell) = {
        val space = 
            for ((line, y) <- lines.zipWithIndex;
                 (letter, x) <- line.zipWithIndex if (letter == '#'))
                 yield Cell(0 + steps, 0 + steps, y + steps, x + steps)

        (space.toSet,
         Cell(1 + 2 * steps,
              1 + 2 * steps,
              lines.size + 2 * steps,
              lines(0).size + 2 * steps))
            }

    def pprint(space: Space, size: Cell) = {
        println(space)
        println()
        for (hyper <- 0 to size.w - 1 ) {
            for (plane <- 0 to size.z - 1 ) {
                for (line <- 0 to size.y - 1) {
                    println((0 to size.x - 1).map( cell => if (space.contains(Cell(hyper, plane, line, cell))) '#' else '.').mkString)
                }
                println(s"z: ${plane}, w: ${hyper}")
            }
            println(s"w: ${hyper} ########")
        }
    }
}