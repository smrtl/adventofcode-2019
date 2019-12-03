import $file.Computer

val Expected = 19690720

val values =
  for (noun <- (0 to 99).view; verb <- (0 to 99).view)
    yield (noun, verb, Computer.run(Computer.program, noun, verb))

val result = values
  .collectFirst { case (noun, verb, Expected) => noun * 100 + verb }
  .getOrElse { throw new RuntimeException("Oh no! There's no solution") }

println(s"answer is $result")
