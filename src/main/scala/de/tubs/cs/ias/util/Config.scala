package de.tubs.cs.ias.util

import de.tubs.cs.ias.util.{FileSystemInteraction => fsi}
import spray.json._

object Config extends DefaultJsonProtocol {

  implicit val appCategoryFormat: RootJsonFormat[AppCategory] = jsonFormat2(
    AppCategory)

  implicit val appPermissionFormat: RootJsonFormat[AppPermission] = jsonFormat1(AppPermission)

  implicit val loginConfigFormat: RootJsonFormat[LoginConfig] = jsonFormat3(
    LoginConfig)

  implicit val telegramConfigFormat: RootJsonFormat[TelegramConfig] =
    jsonFormat3(TelegramConfig)

  implicit val iosConfigFormat: RootJsonFormat[iOSConfig] = jsonFormat4(
    iOSConfig)

  implicit val androidConfigFormat: RootJsonFormat[AndroidConfig] =
    jsonFormat4(AndroidConfig)

  implicit val fdroidConfigFormat: RootJsonFormat[FDroidConfig] =
    jsonFormat4(FDroidConfig)

  implicit val configFormat: RootJsonFormat[Config] = jsonFormat7(Config.apply)

  def read(path: String): Config = {
    fsi.readInTextFile(path).parseJson.convertTo[Config]
  }

}

case class AppCategory(name: String, id: String)

case class AppPermission(name: String)

case class LoginConfig(email: String, password: String, twoFA: String)

case class TelegramConfig(enable: Boolean, apiKey: String, chatId: String)

case class iOSConfig(categories: List[AppCategory],
                     login: LoginConfig,
                     ipatoolpy: String,
                     osFolderName: String)

case class AndroidConfig(categories: List[AppCategory],
                         login: LoginConfig,
                         osFolderName: String,
                         googleplay: String)

case class FDroidConfig(categories: List[AppCategory],
                        permissions: List[AppPermission],
                        osFolderName: String,
                        fdroidcl: String)

case class Config(ios: iOSConfig,
                  android: AndroidConfig,
                  fdroid: FDroidConfig,
                  telegram: TelegramConfig,
                  maxAmountApps: Int,
                  downloadFolderRoot: String,
                  downloaderConfig: String)
