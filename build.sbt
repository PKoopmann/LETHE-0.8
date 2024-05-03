

// General settings
ThisBuild / name := "lethe"
ThisBuild / organization := "de.tu-dresden.inf.lat"
ThisBuild / version := "0.8-SNAPSHOT"
ThisBuild / scalaVersion := "2.12.6"
ThisBuild / retrieveManaged := true // copy dependencies
ThisBuild / scalacOptions += "-target:jvm-1.8"
ThisBuild / javacOptions ++= Seq("-source", "1.8", "-target", "1.8")
ThisBuild / libraryDependencies ++= Seq(
  "org.scala-lang.modules" %% "scala-swing" % "2.1.1",
  "junit" % "junit" % "4.4" % Test,
  "ch.qos.logback" % "logback-classic" % "1.2.3",
  "com.typesafe.scala-logging" %% "scala-logging" % "3.9.2",
  "org.scalatest" %% "scalatest" % "3.0.8" % Test,
  "com.novocode" % "junit-interface" % "0.11" % Test,
  "net.sf.trove4j" % "trove4j" % "3.0.3",
  "org.json4s" %% "json4s-native" % "3.6.7",
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2")
// ThisBuild / assemblyJarName := name.value+"-standalone-"+version.value+".jar"
ThisBuild / test := { }

ThisBuild / assembly / assemblyMergeStrategy := {
  //  case PathList("net.sourceforge.owlapi", "owlapi-distribution", xs @
  //      _*)         => MergeStrategy.first
  case PathList(ps @ _*) if ps.last endsWith ".class" => MergeStrategy.first
  //  case PathList(ps @ _*) if ps.last endsWith ".html" => MergeStrategy.first
  //  case "application.conf"                            => MergeStrategy.concat
  //  case "unwanted.txt"                                =>
  //  MergeStrategy.discard
  case "META-INF/axiom.xml" => MergeStrategy.first
  case x => 
    val oldStrategy = (assemblyMergeStrategy in assembly).value
    oldStrategy(x)
}

ThisBuild / publishMavenStyle := true
ThisBuild / publishTo := Some(Resolver.file("localRepository", file("../../Ivy"))(Patterns("[organisation]/[module]/[revision]/[type]s/[artifact].[ext]")))
ThisBuild / scalacOptions in (Compile, doc) ++= Seq("-skip-packages", "uk.ac.man.cs.lethe.internal:uk.ac.man.cs.lethe.klappoExperiments")



// subprojects:
lazy val lethe_core = Project(
  id = "core",
  base = file ("LETHE-core"))
  .settings(
    name := "lethe-core"
  )


lazy val lethe_owlapi4 = Project(
  id = "owlapi4",
  base = file("LETHE-owlapi4"))
  .settings(
    name := "lethe-owlapi4",
    libraryDependencies ++= Seq(
      "net.sourceforge.owlapi" % "owlapi-distribution" % "4.5.20",
      "net.sourceforge.owlapi" % "org.semanticweb.hermit" % "1.3.8.413"
    ))
  .dependsOn(lethe_core)


lazy val lethe_owlapi5 = Project(
  id = "owlapi5",
  base = file ("LETHE-owlapi5"))
  .settings(
    name := "lethe-owlapi5",
    libraryDependencies ++= Seq(
      "net.sourceforge.owlapi" % "owlapi-distribution" % "5.1.7",
      "net.sourceforge.owlapi" % "org.semanticweb.hermit" % "1.4.3.517"),
//    target := file(file("LETHE-owlapi5/target").getAbsolutePath) //alternative method to explicitly adding scalaSource in Compile
    scalaSource in Compile := file(file("LETHE-owlapi4/src/main/scala").getAbsolutePath),
    scalaSource in Test := file(file("LETHE-owlapi4/src/main/scala").getAbsolutePath),
    sbtTestDirectory := file(file("LETHE-owlapi4/src/test/scala").getAbsolutePath)
  )
  .dependsOn(lethe_core)

lazy val lethe_abduction = Project(
  id = "abduction",
  base = file ("LETHE-abduction"))
  .settings(
    name := "lethe-abduction"
  )
  .dependsOn(lethe_core)

lazy val lethe_abduction_owlapi4 = Project(
  id = "abduction_owlapi4",
  base = file ("LETHE-abduction-owlapi4"))
  .settings(
    name := "lethe-abduction-owlapi4"
  )
  .dependsOn(lethe_abduction,lethe_owlapi4)

lazy val lethe_abduction_owlapi5 = Project(
  id = "abduction-owlapi5",
  base = file ("LETHE-abduction-owlapi5"))
  .settings(
    name := "lethe-abduction-owlapi5",
    scalaSource in Compile := file(file("LETHE-abduction-owlapi4/src/main/scala").getAbsolutePath),
    scalaSource in Test := file(file("LETHE-abduction-owlapi4/src/main/scala").getAbsolutePath),
    sbtTestDirectory := file(file("LETHE-abduction-owlapi4/src/test/scala").getAbsolutePath)
  )
  .dependsOn(lethe_abduction,lethe_owlapi5)

lazy val unused = Project(
  id = "unused",
  base = file ("LETHE-unused"))
  .settings(
    name := "lethe-unused")
  .dependsOn(lethe_core, lethe_owlapi5)


lethe_owlapi5 / assembly / assemblyMergeStrategy := {
  case PathList(ps @ _*) if ps.last endsWith ".class" => MergeStrategy.first
  case "META-INF/axiom.xml" => MergeStrategy.first
  case x => 
    val oldStrategy = (assemblyMergeStrategy in assembly).value
    oldStrategy(x)
}

lethe_owlapi4 / assembly / assemblyMergeStrategy := {
  case PathList(ps @ _*) if ps.last endsWith ".class" => MergeStrategy.first
  case "META-INF/axiom.xml" => MergeStrategy.first
  case x => 
    val oldStrategy = (assemblyMergeStrategy in assembly).value
    oldStrategy(x)
}

lethe_abduction_owlapi5 / assembly / assemblyMergeStrategy := {
  case PathList(ps @ _*) if ps.last endsWith ".class" => MergeStrategy.first
  case "META-INF/axiom.xml" => MergeStrategy.first
  case x =>
    val oldStrategy = (assemblyMergeStrategy in assembly).value
    oldStrategy(x)
}

lethe_abduction_owlapi4 / assembly / assemblyMergeStrategy := {
  case PathList(ps @ _*) if ps.last endsWith ".class" => MergeStrategy.first
  case "META-INF/axiom.xml" => MergeStrategy.first
  case x =>
    val oldStrategy = (assemblyMergeStrategy in assembly).value
    oldStrategy(x)
}
