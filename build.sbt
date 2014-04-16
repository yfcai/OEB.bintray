scalaVersion := "2.10.3"

scalacOptions ++= Seq("-feature", "-deprecation")

resolvers += Resolver.sonatypeRepo("releases")

addCompilerPlugin("org.scalamacros" % "paradise" % "2.0.0-M3" cross CrossVersion.full)

resolvers += bintray.Opts.resolver.repo("yfcai", "maven")

libraryDependencies += "com.mueller" % "Conversion-Macro" % "0.1"
