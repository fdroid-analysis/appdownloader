package de.tubs.cs.ias.labels

trait LabelDownloader {
  def getPrivacyLabel(appId: String): String
}
