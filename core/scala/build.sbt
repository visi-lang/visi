import AssemblyKeys._ // put this at the top of the file

//Project Information
name := "Visi"

scalaVersion := "2.9.2"

scalacOptions += "-deprecation"

publishMavenStyle := true

autoCompilerPlugins := true

checksums := Nil

organization := "visi.la" 

// seq(webSettings :_*)

resolvers += "Scala Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/" 

resolvers += "Scala" at "https://oss.sonatype.org/content/groups/scala-tools/"

resolvers += "Java.net Maven2 Repository" at "http://download.java.net/maven/2/"

version := "0.1-SNAPSHOT"

// crossScalaVersions in ThisBuild    := Seq("2.9.2") 

libraryDependencies ++= {
  val liftVersion = "2.5-M3"
  Seq(
    "net.liftweb" %% "lift-util" % liftVersion % "compile",
    "net.liftweb" %% "lift-json-ext" % liftVersion % "compile")
}

libraryDependencies ++= {
  Seq(
    "org.yaml" % "snakeyaml" % "1.10",
    "junit" % "junit" % "4.7" % "test",
    "ch.qos.logback" % "logback-classic" % "1.0.6" % "compile" ,
    "eu.henkelmann" %% "actuarius" % "0.2.4",
    "org.eclipse.jgit" % "org.eclipse.jgit" % "1.3.0.201202151440-r",
    "org.scala-tools.testing" %% "specs" % "1.6.9" % "test"
  )
}

publishTo <<= version { (v: String) =>
  val nexus = "https://oss.sonatype.org/"
  if (v.trim.endsWith("SNAPSHOT"))
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases"  at nexus + "service/local/staging/deploy/maven2")
}

assemblySettings

jarName in assembly := "visi.jar"

test in assembly := {}

mainClass in assembly := Some("visi.Main")

publishArtifact in Test := false

pomIncludeRepository := { _ => false }

pomExtra := (
  <url>http://hoisted.org</url>
  <licenses>
    <license>
      <name>Apache 2.0</name>
      <url>http://www.opensource.org/licenses/Apache-2.0</url>
      <distribution>repo</distribution>
    </license>
  </licenses>
  <scm>
    <url>https://github.com/visi-lang/visi.git</url>
    <connection>scm:git:git://github.com/visi-lang/visi.git</connection>
  </scm>
  <developers>
    <developer>
      <id>dpp</id>
      <name>David Pollak</name>
      <url>http://blog.goodstuff.im</url>
    </developer>
  </developers>)

