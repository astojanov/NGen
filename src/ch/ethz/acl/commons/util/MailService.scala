/**
  *      ___    ______ __       ______ ____   __  ___ __  ___ ____   _   __ _____
  *     /   |  / ____// /      / ____// __ \ /  |/  //  |/  // __ \ / | / // ___/
  *    / /| | / /    / /      / /    / / / // /|_/ // /|_/ // / / //  |/ / \__ \
  *   / ___ |/ /___ / /___   / /___ / /_/ // /  / // /  / // /_/ // /|  / ___/ /
  *  /_/  |_|\____//_____/   \____/ \____//_/  /_//_/  /_/ \____//_/ |_/ /____/
  *
  *  Advanced Computing Laboratory
  *  Department of Computer Science
  *  ETH Zurich, Switzerland
  *
  *  Copyright (C) 2017 Alen Stojanov (astojanov@inf.ethz.ch)
  *
  *  This program is free software: you can redistribute it and/or modify
  *  it under the terms of the GNU General Public License as published by
  *  the Free Software Foundation, either version 3 of the License, or
  *  (at your option) any later version.
  *
  *  This program is distributed in the hope that it will be useful,
  *  but WITHOUT ANY WARRANTY; without even the implied warranty of
  *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  *  GNU General Public License for more details.
  *
  *  You should have received a copy of the GNU General Public License
  *  along with this program. If not, see http://www.gnu.org/licenses/.
  */

package ch.ethz.acl.commons.util

import java.util.Properties
import javax.mail._
import javax.mail.internet.{InternetAddress, MimeMessage}

import com.typesafe.config.ConfigFactory

import scala.collection.JavaConverters._

object MailService {

  val config = ConfigFactory.load("ch.ethz.acl.settings")

  private val username   = config.getString("settings.mail.username")
  private val password   = config.getString("settings.mail.password")
  private val email      = config.getString("settings.mail.email")
  private val recipients = config.getStringList("settings.recipients").asScala.toList


  def sendMail (subject: String, text: String) = {
    val props = new Properties()
    props.put("mail.smtp.host", "smtp.gmail.com")
    props.put("mail.smtp.socketFactory.port", "465")
    props.put("mail.smtp.socketFactory.class", "javax.net.ssl.SSLSocketFactory")
    props.put("mail.smtp.auth", "true")
    props.put("mail.smtp.port", "465")

    val session = Session.getDefaultInstance(props, new javax.mail.Authenticator() {
      override def getPasswordAuthentication() = {
        new PasswordAuthentication(username, password)
      }
    })

    try {
      val message = new MimeMessage(session);
      message.setFrom(new InternetAddress(email))
      val to = InternetAddress.parse(recipients.mkString(", ")).asInstanceOf[Array[Address]]
      message.setRecipients(Message.RecipientType.TO, to)
      message.setSubject("[SpiralS] " + subject)
      message.setText(text)
      Transport.send(message);
    } catch {
      case e: MessagingException => // Don't fail in case the MailService fails ... please
    }
  }

}

