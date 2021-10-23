import scala.io.Source

object Customs extends App {
    if (args.length != 1) {
        println("Input file required!")
        System.exit(1)
    }

    val group_forms = Source.fromFile(args(0)).mkString.split("\\n\\n")

    val group_counts_sum1 = group_forms.map(
        _.replace("\n", "").toSet.size
        ).sum
    println(group_counts_sum1)

    val group_counts_sum2 = group_forms.map(
        _.split("\n").map(_.toSet).reduce(_ & _).size
        ).sum
    println(group_counts_sum2)
}