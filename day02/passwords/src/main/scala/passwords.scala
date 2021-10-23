import scala.io.Source

object Passwords extends App {
  if (args.length != 2) {
    println("Input file and test name ([test1|test2]) required!")
    System.exit(1)
  }

  var count_valid = Source.fromFile(args(0)).getLines().map(is_valid).sum
  println(count_valid)
  
  def is_valid(record: String): Int = {
    val (rule, rest) = record.split(":") match { case Array(f1,f2) => (f1,f2)}
    val (range, letter) = rule.split(" ") match { case Array(f1,f2) => (f1,f2)}
    val password = rest.tail
    val (first, second) = range.split("-").map(_.toInt) match { case Array(f1,f2) => (f1,f2)}
    args(1) match {
      case "test1" => test1(password, letter.charAt(0), first, second)
      case "test2" => test2(password, letter.charAt(0), first, second)
    }
  }

  def test1(password: String, letter: Char, min: Int, max: Int): Int = {
    password.count(_ == letter) match {
      case x if (x >= min) && (x <= max) => 1
      case _ => 0
    }
  }

  def test2(password: String, letter: Char, first: Int, second: Int) = {
    val firstl = password.charAt(first - 1)
    val secondl = password.charAt(second - 1)
    if (letter == firstl && letter == secondl) 0
    else if (letter != firstl && letter != secondl) 0
    else 1
  }
}
