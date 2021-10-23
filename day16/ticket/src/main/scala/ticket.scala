import scala.io.Source

object Ticket extends App {
    if (args.length != 1) {
        println("Input file required!")
        System.exit(1)
    }

    val (rules, myticket, tickets) = parse_input(Source.fromFile(args(0)).getLines().toVector)
    val valid_values = rules.values.reduce(_ union _)
    println(tickets.map(invalid_sum(valid_values)(_)._1).sum)   // solve1

    val valid_tickets = tickets.filter(!invalid_sum(valid_values)(_)._2)
    val init_field_rule_idx_sets = Vector.fill(myticket.length)(rules.keys.toSet)
    val possible_field_rule_idx_sets = valid_tickets.foldLeft(init_field_rule_idx_sets)(update_rules(_, _))
    val field_rule_idxs = possible_field_rule_idx_sets.zipWithIndex.sortBy(_._1.size)    // sort by subsets while keeping the indices of the fields
                            // extract the unique rule for the field from the set by substracting the preceding set
                            .prepended((Set[Int](), -1)).sliding(2).map(x => (x.last._1 diff x.head._1, x.last._2))
                            .toVector.sortBy(_._2).unzip                                 // re-sort by the field indices
                            ._1                                                          // ditch the field indices
    field_rule_idxs.map(idxs => assert(idxs.size == 1))
    // departure fields are the first six fields
    println(( for (idx <- 0 to 5) yield myticket(field_rule_idxs.indexOf(Set(idx))).toLong).product)    // solve2

    def update_rules(rule_idxs: Vector[Set[Int]], ticket: Vector[Int]): Vector[Set[Int]] = {
        rule_idxs.zip(ticket)
                .map{ case (rule_idx, value) => rule_idx.filter( rules(_).contains(value)) }
    }

    def parse_input(lines: Vector[String]): (Map[Int, Set[Int]], Vector[Int], Vector[Vector[Int]]) = {
        val first_empty_line_idx = lines.indexOf("")
        val rules = lines
                        .slice(0, first_empty_line_idx)
                        .zipWithIndex
                        .map{ case (line, idx) => idx -> parse_rule(line) }
                        .toMap
        val myticket = parse_ticket(lines(first_empty_line_idx + 2))
        val tickets = lines
                        .drop(first_empty_line_idx + 5)
                        .map(parse_ticket)
        (rules, myticket, tickets)
    }

    def parse_ticket(line: String): Vector[Int] =
        line.split(",").map(_.toInt).toVector

    def parse_rule(line: String): Set[Int] = {
        val segments = line.dropWhile(_ != ':').split(" ")
        parse_range(segments(1)) union parse_range(segments(3))
    }

    def parse_range(range: String): Set[Int] = {
        val limits = range.split("-")
        (limits(0).toInt to limits(1).toInt).toSet
    }

    def invalid_sum(valid_values: Set[Int])(ticket: Vector[Int]): (Int, Boolean) =
        ticket.foldLeft(0, false)((x, y) => if (!valid_values.contains(y)) (x._1 + y, true) else x)
}