package de.tubs.cs.ias.apps.android

import de.tubs.cs.ias.apps.android.GooglePlayClient.out
import de.tubs.cs.ias.util.FileSystemInteraction
import java.text.SimpleDateFormat
import scala.collection.mutable.ListBuffer
import scala.sys.process.{ProcessLogger, _}
import wvlet.log.LogSupport

class GooglePlayClient(googleplay: String) extends LogSupport {
  private val DEVICE_TYPE =
    X86 //this is X86 and works
  private val playCli = s"$googleplay"

  def getAppInfo(
      appId: String,
      architecture: Architecture = DEVICE_TYPE
  ): Either[PlayAppInfo, Panic] = {
    val errors: ListBuffer[String] = ListBuffer()
    try {
      this.writeResult(s"searching for $appId")
      val appInfo = s"$playCli -a $appId -p ${architecture.id}"
        .!!(ProcessLogger(_ => (), err => errors.append(err)))
        .split('\n')
      val panic = errors.filter(_.contains("panic"))

      if (panic.isEmpty) {
        this.writeResult("panic empty")
        if (appInfo.length == 12) {
          val playAppInfo = parsePlayResponse(appInfo)
          this.writeResult(s"app available: $appId - ${playAppInfo.price} €")
          Left(playAppInfo)
        } else {
          this.writeResult(s"wrong line count: ${appInfo.mkString("\n")}")
          Right(
            UnknownPanic(
              s"unexpected output line count:\n ${appInfo.mkString("\n")}",
              "app info"
            )
          )
        }
      } else {
        this.writeResult(s"panic: ${panic.toString()}")
        Right(Panic.getPanic(panic, "app info"))
      }
    } catch {
      case x: Throwable =>
        this.writeResult(s"exception: ${x.getMessage}")
        Right(Panic.getPanic(errors, "app info"))
    }
  }

  private def parsePlayResponse(rawResponse: Array[String]): PlayAppInfo = {
    this.writeResult(s"parsing ${rawResponse.mkString("|")}")
    val headerLength = 2
    val downloads = getAppValue(rawResponse(headerLength + 0))

    val parsedFiles = getAppValue(rawResponse(headerLength + 1))
    val files = if (parsedFiles.nonEmpty) parsedFiles.split(' ').toList else List()

    val name = getAppValue(rawResponse(headerLength + 2))

    val developer = getAppValue(rawResponse(headerLength + 3))

    val parsedPrice = getAppValue(rawResponse(headerLength + 4))
    val price = if (parsedPrice.nonEmpty) parsedPrice.split(' ')(0).toDouble else 0

    val requires = getAppValue(rawResponse(headerLength + 5))

    val size = getAppValue(rawResponse(headerLength + 6))

    val updated = getAppValue(rawResponse(headerLength + 7))
    val dateFormat = new SimpleDateFormat("dd.MM.yyyy")
    val lastUpdated = if (updated.nonEmpty) Some(dateFormat.parse(updated)) else None

    val parsedVersionCode = getAppValue(rawResponse(headerLength + 8))

    val versionName = getAppValue(rawResponse(headerLength + 9))

    val versionCode = if (parsedVersionCode.nonEmpty) parsedVersionCode.toLong else -1L
    val version = PlayAppVersion(versionCode, versionName)

    PlayAppInfo(
      downloads,
      files,
      name,
      developer,
      price,
      requires,
      size,
      lastUpdated,
      version
    )
  }

  private def getAppValue(rawLine: String): String = {
    val split = rawLine.split(':')
    val appValue = if (split.length > 1) split.drop(1).mkString(":") else ""
    appValue.strip()
  }

  private def writeResult(message: String): Unit = {
    FileSystemInteraction.appendToFile(message + "\n", out)
  }

}

object GooglePlayClient {
  val DELAY_TIME_MAX: Long = 10_000
  val out = "/home/pc/ba/fdroid/20231012/lists/playavailable.txt"
}

sealed trait Architecture {
  val id: Int
}

case object X86 extends Architecture {
  override val id = 0
}

case object Armeabi extends Architecture {
  override val id = 1
}

case object Arm64 extends Architecture {
  override val id = 2
}
