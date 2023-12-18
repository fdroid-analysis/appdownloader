package de.tubs.cs.ias.labels

import de.tubs.cs.ias.OperatingSystem.{ANDROID, IOS, OperatingSystem}
import de.tubs.cs.ias.labels.android.{LabelDownloader => AndroidLabelDownloader}
import de.tubs.cs.ias.labels.ios.{LabelDownloader => iOSLabelDownloader}
import de.tubs.cs.ias.util.{ActionReport, AsciiProgressBar, FileSystemInteraction => fsi}
import java.io.File
import scala.collection.mutable.{Map => MMap}
import wvlet.log.LogSupport

object LabelAction extends LogSupport {

  def download(appIds: List[String],
               folder: String,
               os: OperatingSystem): ActionReport = {
    val bar =
      AsciiProgressBar.create("Downloading Labels ", appIds.length.toLong)
    val failures = MMap[String, String]()
    try {
      appIds.foreach { id =>
        try {
          val file = s"$folder/$id.json"
          if (!new File(file).exists()) {
            val content: String = os match {
              case ANDROID => AndroidLabelDownloader.getPrivacyLabel(id)
              case IOS => iOSLabelDownloader.getPrivacyLabel(id)
            }
            fsi.writeFile(content, file)
          }
        } catch {
          case TooManyTriesException =>
            Thread.sleep(60000) // if we have to break out of the download due to excess try ... calm down for a minute
          case e: Exception =>
            error(e.getMessage)
            failures.addOne(id -> e.getMessage)
          case e: Error =>
            error(e.getMessage)
            failures.addOne(id -> e.getMessage)
        } finally {
          bar.step()
        }
      }
    } finally {
      bar.close()
    }
    ActionReport(appIds.length - failures.size, failures.size, failures.toMap)
  }
}
