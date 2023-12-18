package de.tubs.cs.ias.apps.android

import de.tubs.cs.ias.util.AndroidConfig
import java.io.File
import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.sys.process._
import scala.util.Random
import wvlet.log.LogSupport

object AppDownloader extends LogSupport {

  val DEVICE_TYPE = 0 //this is x86 and works on our GALAXY A13
  val DELAY_TIME_MAX: Long = 10_000

  /** try to call the googleplay exec
    *
    * @param googleplay path to the googleplay exec
    */
  def readinessCheckGooglePlayTool(googleplay: String): Unit =
    s"$googleplay".!!

  def getAppVersion(
      app: String,
      googleplay: String
  ): (GooglePlayResult, GooglePlayResult) = {
    val playCli = new GooglePlayClient(googleplay)
    var appVersionArchitecture = playCli.getAppInfo(app, X86) match {
      case Left(appInfo) =>
        (StringResult(appInfo.version.code.toString), ArchitectureResult(X86))
      case Right(panic) => (panic, IncompatibleDevice(panic.msg, panic.context))
    }
    // currently not supported as the google play tool does not create a corresponding .bin file for arm64
    /*if (appVersionArchitecture._2 != ArchitectureResult(X86)) {
      appVersionArchitecture = playCli.getAppInfo(app, Arm64) match {
        case Left(appInfo) =>
          (
            StringResult(appInfo.version.code.toString),
            ArchitectureResult(Arm64)
          )
        case Right(panic) =>
          (panic, IncompatibleDevice(panic.msg, panic.context))
      }
    }*/
    appVersionArchitecture
  }

  def purchase(app: String, googleplay: String, architecture: Architecture = X86): GooglePlayResult = {
    val lines = ListBuffer[String]()
    try {
      val ret = s"$googleplay -acquire -p ${architecture.id} -a $app" ! ProcessLogger(
        _ => (),
        err => lines.append(err)
      )
      val panic = lines.filter(_.contains("panic:"))
      if (ret != 0 || panic.nonEmpty) {
        Panic.getPanic(lines, "purchase")
      } else {
        Success
      }
    } catch {
      case _: Throwable =>
        Panic.getPanic(lines, "purchase")
    }
  }

  def downloadApk(
      app: String,
      version: String,
      folder: String,
      googleplay: String,
      architecture: Architecture = X86
  ): GooglePlayResult = {
    val lines: ListBuffer[String] = ListBuffer()
    try {
      Thread.sleep(
        Random.nextLong(DELAY_TIME_MAX)
      ) // waiting between 0 and DELAY_TIME_MAX to avoid too many requests
      val download = s"$googleplay -p ${architecture.id} -a $app -s -v $version"
      val ret = Process(download, new File(folder)) ! ProcessLogger(
        _ => (),
        err => lines.append(err)
      )
      val panic = lines.filter(_.contains("panic:"))
      if (ret != 0) {
        Panic.getPanic(lines, "download")
      } else if (panic.nonEmpty) {
        Panic.getPanic(panic, "download")
      } else {
        Success
      }
    } catch {
      case _: Throwable =>
        val panic = lines.filter(_.contains("panic:"))
        if (panic.nonEmpty) {
          Panic.getPanic(panic, "download")
        } else {
          Panic.getPanic(lines, "download")
        }
    }
  }

  @tailrec
  def download(
      app: String,
      folder: String,
      config: AndroidConfig,
      maxRetries: Int = 3
  ): GooglePlayResult = {
    getAppVersion(app, config.googleplay) match {
      case (Success, _) =>
        throw new RuntimeException(
          "getAppVersion must not return success object"
        )
      case (StringResult(version), ArchitectureResult(architecture)) =>
        purchase(app, config.googleplay, architecture) match {
          case x: Panic => x
          case StringResult(_) =>
            throw new RuntimeException("purchase does not return a result")
          case Success =>
            downloadApk(app, version, folder, config.googleplay, architecture) match {
              case x: BadRequest =>
                if (maxRetries == 0) {
                  x
                } else {
                  warn("encountered bad request ... retrying")
                  Thread.sleep(Random.nextLong(DELAY_TIME_MAX))
                  download(app, folder, config, maxRetries - 1)
                }
              case panic: Panic => panic
              case StringResult(_) =>
                throw new RuntimeException(
                  "download Apk does not return a result"
                )
              case Success => Success
            }

        }
      case x: (GooglePlayResult, GooglePlayResult) => x._1
    }
  }

}
