import scala.io.Source

object Expense extends App {
  if (args.length != 1) {
    println("Input file required!")
    System.exit(1)
  }

  val expenses = Source.fromFile(args(0)).getLines().toList.map(_.toInt)
  val expense = get_matching_expense(2020, expenses.head, expenses.tail)
  println(expense * (2020 - expense))

  def get_matching_expense(sum: Int, expense_to_match: Int, expenses: List[Int]): Int = {
    if (expenses.contains(sum - expense_to_match)) {
      sum - expense_to_match
    } else {
      get_matching_expense(sum, expenses.head, expenses.tail)
    }
  }
}
