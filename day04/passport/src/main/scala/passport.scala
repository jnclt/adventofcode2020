import scala.io.Source

object Passport extends App {
  if (args.length != 1) {
    println("Input file required!")
    System.exit(1)
  }

  val required = Set("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid")
  val optional = required + "cid"

  val passport_count1 = Source.fromFile(args(0)).mkString.split("\\n\\n").map(is_passport1).sum
  println(passport_count1)
  val passport_count2 = Source.fromFile(args(0)).mkString.split("\\n\\n").map(is_passport2).sum
  println(passport_count2)

  def is_passport1(record: String): Int = {
    val present = record.split("\\s").map(_.substring(0, 3)).toSet
    if ((present == required) || (present == optional)) 1 else 0
  }

  def is_passport2(record: String): Int = {
    if (is_passport1(record) == 1) validate_fields(record) else 0
  }

  def validate_fields(record: String): Int = {
    var invalid_count = 0
    for (field <- record.split("\\s")) {
      val (key, text) = field.split(":") match { case Array(f1,f2) => (f1,f2)}
      val invalid_field = key match {
        case "byr" if (text.toInt < 1920 || text.toInt > 2002) => 1
        case "iyr" if (text.toInt < 2010 || text.toInt > 2020) => 2
        case "eyr" if (text.toInt < 2020 || text.toInt > 2030) => 4 
        case "hgt" if 
          (
            ((text.takeRight(2) != "in") && (text.takeRight(2) != "cm")) ||
            ((text.takeRight(2) == "in") && (text.stripSuffix("in").toInt < 59 || text.stripSuffix("in").toInt > 76)) ||
            ((text.takeRight(2) == "cm") && (text.stripSuffix("cm").toInt < 150 || text.stripSuffix("cm").toInt > 193))
          ) => 8
        case "hcl" if (! text.matches("#[a-f0-9]{6}")) => 16
        case "ecl" if (! Set("amb", "blu", "brn", "gry", "grn", "hzl", "oth").contains(text)) => 32
        case "pid" if (! text.matches("[0-9]{9}")) => 64
        case _ => 0
      }
      invalid_count += invalid_field
    }
    // if (invalid_count > 0) println(s"${invalid_count}: ${record.split("\\s").mkString(" ")}")
    if (invalid_count == 0) 1 else 0
  }
}
