package eu.rtkaczyk.fitter

object Fitter extends App {

  val mode :: inputs = args.toList

  val run: Mode = mode match {
    case "merge" => new Merge
    case "continue" => new Continue

    case other =>
      _ => println(s"Invalid mode: $other")
  }

  run(inputs)
}
