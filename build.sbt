resolvers += bintray.Opts.resolver.repo("yfcai", "maven")
//seq(bintrayResolverSettings:_*)

// want: http://dl.bintray.com/bjoern-mueller/generic/macros_2.10-1.0.0.jar
// got: http://dl.bintray.com/content/bjoern-mueller/generic/me/bjoern-mueller/Conversion-Macro/test/Conversion-Macro-test.pom
libraryDependencies += "com.mueller" % "Conversion-Macro" % "0.1"
