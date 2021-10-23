import scala.io.Source

object Toboggan extends App {
  if (args.length != 1) {
    println("Input file required!")
    System.exit(1)
  }

  val rows = Source.fromFile(args(0)).getLines().toList
  val second_rows = for (i <- 0 to rows.length by 2) yield rows(i)

  val tree_count = traverse(rows, 0, 3)
  println(tree_count)

  val tree_count_product = 
    traverse(rows, 0, 1) * 
    traverse(rows, 0, 3) * 
    traverse(rows, 0, 5) * 
    traverse(rows, 0, 7) *
    traverse(second_rows.toList, 0, 1)
  println(tree_count_product)

  def traverse(rows: List[String], longitude: Int, step: Int): BigInt = {
    rows match {
      case Nil => 0
      case x::xx => {
        val is_tree = if (x.charAt(longitude % x.length) == '#') 1 else 0
        is_tree + traverse(xx, longitude + step, step)
      }
    }
  }
}