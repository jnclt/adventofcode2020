import scala.io.Source
import scala.collection.mutable.Map

object Bags extends App {
    if (args.length != 1) {
        println("Input file required!")
        System.exit(1)
    }

    val lines = Source.fromFile(args(0)).getLines().toList

    val outside = lines.map(parse1).foldLeft(Map[String, List[String]]())(merge)
    val outside_bags = collect_outside_bags(outside, outside("shinygold").toSet)
    println(outside_bags.size)

    var inside = lines.foldLeft(Map[String, Map[String, Int]]())((map, line) => map += parse2(line))
    val inside_bags_count = count_inside_bags(inside, "shinygold")
    println(inside_bags_count)

    def collect_outside_bags(outside: Map[String, List[String]], bags: Set[String]): Set[String] = {
        if (bags.isEmpty) Set() else bags | collect_outside_bags(outside, bags.map(outside.getOrElse(_, Nil).toSet).reduce(_ | _))
    }

    def count_inside_bags(inside: Map[String, Map[String, Int]], bag: String): Int = {
        if (! inside.contains(bag)) 0 else
        inside(bag).map({case (inner_bag, count) => count * (1 + count_inside_bags(inside, inner_bag))}).sum
    }

    def merge(outside: Map[String, List[String]], rule: List[String]): Map[String, List[String]] = {
        for (color <- rule.tail) {
            if (outside.contains(color)) {
                outside(color) = rule.head :: outside(color)
            }
            else {
                outside(color) = List(rule.head)
            }
        }
        outside
    }

    def parse1(rule: String): List[String] = {
      rule.replace("contain", ",")
          .replace("bags", "")
          .replace("bag", "")
          .replaceAll("[1-9. ]", "")
          .split(',').toList
    }

    def parse2(rule: String): Tuple2[String, Map[String, Int]] = {
        val colors = rule.init.replace("contain", ",")
            .replace("bags", "")
            .replace("bag", "")
            .replace(" ", "")
            .replace("noother", "")
            .split(',')
        (colors.head, colors.tail.foldLeft(Map[String, Int]())((map, color) => map += (color.tail -> color.head.toString.toInt)))
    }
}