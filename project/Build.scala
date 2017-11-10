import sbt._

object NGenBuild extends Build with BuildUtil {

  System.setProperty("showSuppressedErrors", "true")

  val virtScala: String = Option(System.getenv("SCALA_VIRTUALIZED_VERSION")).getOrElse("2.11.2")

  object ProjectsDependencies {
    lazy val lms = RootProject(uri("git://github.com/astojanov/virtualization-lms-core.git#develop"))
  }

  lazy val NGenProject = Project("NGen", file("."), settings = Defaults.defaultSettings)
    .dependsOn(ProjectsDependencies.lms)
}

trait BuildUtil {

  def virtLib (lib: ModuleID) = {
    val scala ="org.scala-lang"
    lib.exclude (scala, "scala-library").exclude (scala, "scala-compiler").exclude (scala, "scala-reflect")
  }
}
