object Memory extends App {
    if(args.length < 2 ) {
        println("No game without some starting numbers!")
        System.exit(1)
    }

    val end +: start_numbers = args.map(_.toInt).toVector
    var turn = start_numbers.length + 1
    var next = (start_numbers.last, turn - 1)
    var numbers = start_numbers.zip(1 to turn - 1).to(collection.mutable.Map)
    while (turn <= end) {
        val latest = next
        next = (numbers.get(latest._1) match {
            case None => 0
            case Some(previous) => latest._2 - previous
        },
        turn)
        numbers(latest._1) = latest._2
        turn += 1
    }
    println(next._1)
}