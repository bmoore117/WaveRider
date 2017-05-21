name := "WaveRider"

version := "1.0"

scalaVersion := "2.12.1"

// https://mvnrepository.com/artifact/com.tictactec/ta-lib
libraryDependencies += "com.tictactec" % "ta-lib" % "0.4.0"

libraryDependencies += "com.github.tototoshi" %% "scala-csv" % "1.3.4"

libraryDependencies += "org.deeplearning4j" % "deeplearning4j-core" % "0.8.0"
libraryDependencies += "org.nd4j" % "nd4j-native-platform" % "0.8.0"
libraryDependencies += "org.datavec" % "datavec-api" % "0.8.0"

libraryDependencies += "org.slf4j" % "slf4j-simple" % "1.7.25"

libraryDependencies += "org.deeplearning4j" % "rl4j-core" % "0.8.0"

libraryDependencies += "org.mongodb" % "mongodb-driver" % "3.4.2"