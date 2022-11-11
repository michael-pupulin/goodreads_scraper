ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.10"

lazy val root = (project in file("."))
  .settings(
    name := "gr_scrape"
  )

libraryDependencies ++= Seq(
  "org.jsoup" % "jsoup" % "1.15.3",
  "net.sourceforge.htmlunit" % "htmlunit" % "2.66.0"
)
