
// Jacoco (Code covery)
 
// libraryDependencies ++= Seq(
//   "org.jacoco" % "org.jacoco.core" % "0.5.9.201207300726" artifacts(Artifact("org.jacoco.core", "jar", "jar")),
//   "org.jacoco" % "org.jacoco.report" % "0.5.9.201207300726" artifacts(Artifact("org.jacoco.report", "jar", "jar")))

// addSbtPlugin("de.johoop" % "jacoco4sbt" % "1.2.4")

addSbtPlugin("net.virtual-void" % "sbt-dependency-graph" % "0.9.2")

// Ensime plugin

//addSbtPlugin("org.ensime" % "ensime-sbt-cmd" % "0.1.0")



// SBT-Assemby (one jar)

// commented out, since it doesn't seem to work anymore
//resolvers += Resolver.url("artifactory", url("http://scalasbt.artifactoryonline.com/scalasbt/sbt-plugin-releases"))(Resolver.ivyStylePatterns)

//addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "0.13.0")
addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "0.14.9")

// // java doc
// commented out since as is does not compile with current sbt version
// if we want to support javadoc, the code has to be uncommented and
// adapted
//
// lazy val JavaDoc = config("genjavadoc") extend Compile

// lazy val javadocSettings = inConfig(JavaDoc)(Defaults.configSettings) ++ Seq(
//   libraryDependencies += compilerPlugin("com.typesafe.genjavadoc" %%
//     "genjavadoc-plugin" % "0.9" cross CrossVersion.full),
//   scalacOptions <+= target map (t => "-P:genjavadoc:out=" + (t / "java")),
//   packageDoc in Compile <<= packageDoc in JavaDoc,
//   sources in JavaDoc <<=
//     (target, compile in Compile, sources in Compile) map ((t, c, s) =>
//       (t / "java" ** "*.java").get ++ s.filter(_.getName.endsWith(".java"))),
//   javacOptions in (Compile,doc) ++= Seq("-exclude",
// 					"uk.ac.man.cs.lethe.internal"),
//   javacOptions in JavaDoc := Seq("-exclude",
// 			       "uk.ac.man.cs.lethe.internal"),
//   artifactName in packageDoc in JavaDoc :=
//     ((sv, mod, art) =>
//       "" + mod.name + "_" + sv.binary + "-" + mod.revision + "-javadoc.jar")
// )
