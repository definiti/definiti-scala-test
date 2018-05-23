package definiti.scalatests

import java.nio.file.{Path, Paths}

import com.typesafe.config.{Config, ConfigFactory}

private[scalatests] trait Configuration {
  def destination: Path
}

private[scalatests] class FileConfiguration(config: Config) extends Configuration {
  def this() {
    this(ConfigFactory.load())
  }

  lazy val destination: Path = getFirstDefinedPath(
    "definiti.scalatests.destination",
    "definiti.build.destination"
  ).getOrElse(Paths.get("target", "scalatests"))

  private def getFirstDefinedPath(keys: String*): Option[Path] = {
    keys
      .map(getPathOpt)
      .collectFirst {
        case Some(path) => path
      }
  }

  private def getPathOpt(configurationPath: String): Option[Path] = {
    if (config.hasPath(configurationPath)) {
      val rawPath = config.getString(configurationPath)
      val path = Paths.get(rawPath)
      Some(path)
    } else {
      None
    }
  }
}