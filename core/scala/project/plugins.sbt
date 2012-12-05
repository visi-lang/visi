//webplugin
resolvers += "Web plugin repo" at "http://siasia.github.com/maven2"

//Following means libraryDependencies += "com.github.siasia" %% "xsbt-web-plugin" % "0.1.1-<sbt version>""
// Note the above comment is in fact a filthy filthy lie. You put the sbt version number first and then the correct xsbt
//version. Go here: https://oss.sonatype.org/content/groups/scala-tools/com/github/siasia/xsbt-web-plugin_2.9.1/
libraryDependencies <+= sbtVersion(v => "com.github.siasia" %% "xsbt-web-plugin" % (v+"-0.2.11"))

//Idea plugin
resolvers += "sbt-idea-repo" at "http://mpeltonen.github.com/maven/"

addSbtPlugin("com.github.mpeltonen" % "sbt-idea" % "1.0.0")

resolvers += Resolver.url("artifactory", url("http://scalasbt.artifactoryonline.com/scalasbt/sbt-plugin-releases"))(Resolver.ivyStylePatterns)

addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "0.8.3")