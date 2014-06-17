libraryDependencies  ++= Seq(
  // other dependencies here
  "org.scalanlp" %% "breeze" % "0.8.1",
  // native libraries are not included by default. add this if you want them (as of 0.7)
  // native libraries greatly improve performance, but increase jar sizes.
  "org.scalanlp" %% "breeze-natives" % "0.8.1"
)

libraryDependencies += "com.github.scala-incubator.io" %% "scala-io-file" % "0.4.2"

libraryDependencies += "org.mongodb" %% "casbah" % "2.5.0"

resolvers ++= Seq(
  // other resolvers here
  // if you want to use snapshot builds (currently 0.8-SNAPSHOT), use this.
  "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/",
  "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/"
)