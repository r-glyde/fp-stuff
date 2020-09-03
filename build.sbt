name := "fp-stuff"
version := "0.1.0"
scalaVersion := "2.13.3"

// Benchmarking
lazy val Benchmark = config("bench") extend Test
configs(Benchmark)
inConfig(Benchmark)(Defaults.testSettings)

libraryDependencies += "com.storm-enroute" %% "scalameter" % "0.19" % Benchmark
testFrameworks += new TestFramework("org.scalameter.ScalaMeterFramework")
parallelExecution in Benchmark := false
