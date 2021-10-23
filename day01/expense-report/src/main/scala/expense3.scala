import scala.io.Source

object Expense3 extends App {
  if (args.length != 1) {
    println("Input file required!")
    System.exit(1)
  }

  val expenses = Source.fromFile(args(0)).getLines().toList.map(_.toInt)
  println(get_matching_expense3(2020, expenses.head, expenses.tail))

  def get_matching_expense3(sum: Int, expense_to_match: Int, expenses: List[Int]): Int = {
    val matching_expense = expenses match {
      case Nil => 0
      case x::xx => get_matching_expense2(sum - expense_to_match, x, xx)
    }
    matching_expense match {
      case 0 => get_matching_expense3(sum, expenses.head, expenses.tail)
      case _ => expense_to_match * matching_expense
    } 
  }

  def get_matching_expense2(sum: Int, expense_to_match: Int, expenses: List[Int]): Int = {
    expenses match {
      case Nil => 0
      case x if x.contains(sum - expense_to_match) => (sum - expense_to_match) * expense_to_match
      case x::xx => get_matching_expense2(sum, x, xx)
    }
  }
}

