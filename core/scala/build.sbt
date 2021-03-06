import AssemblyKeys._ // put this at the top of the file

//Project Information
name := "Visi"

scalaVersion := "2.10.0"

scalacOptions += "-deprecation"

publishMavenStyle := true

autoCompilerPlugins := true

checksums := Nil

organization := "la.visi" 

// seq(webSettings :_*)

resolvers += "Scala Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/" 

resolvers += "Scala" at "https://oss.sonatype.org/content/groups/scala-tools/"

resolvers += "Java.net Maven2 Repository" at "http://download.java.net/maven/2/"

version := "0.1.2-SNAPSHOT"

// crossScalaVersions in ThisBuild    := Seq("2.9.2") 

libraryDependencies ++= {
  val liftVersion = "3.0-SNAPSHOT"
  Seq(
    "net.liftweb" %% "lift-util" % liftVersion % "compile",
    "net.liftweb" %% "lift-json-ext" % liftVersion % "compile")
}

libraryDependencies ++= {
  Seq(
    "org.yaml" % "snakeyaml" % "1.10",
    "junit" % "junit" % "4.7" % "test",
    "rhino" % "js" % "1.7R2" % "compile",
    "ch.qos.logback" % "logback-classic" % "1.0.6" % "compile" ,
    "org.hoisted" %% "actuarius" % "0.2.5-SNAPSHOT",
    "org.eclipse.jgit" % "org.eclipse.jgit" % "1.3.0.201202151440-r",
    "org.parboiled" %% "parboiled-scala" % "1.1.4",
    "org.specs2" %% "specs2" % "1.12.3" % "test"
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

