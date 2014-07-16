package controllers

import play.api._
import play.api.mvc._

import java.io.FileWriter
import java.io.File

import scala.io.Source
import scala.collection.mutable.HashSet

object Cookies {
	val filename = "cookies"
	val cookies	= new HashSet[String]

	if(new File(filename).exists()) {
		cookies ++= Source.fromFile(new File(filename)).getLines.toSet
	} else {
		new File(filename).createNewFile()
	}

	var firstLine: Boolean = (cookies.size == 0)
	def lookupCookie(cookie: String) = cookies contains cookie
	def getNumRegisteredCookies() = cookies.size

	def resetCounter() {
		new File(filename).delete()
		new File(filename).createNewFile()
		firstLine = true
		cookies.clear
	}

	def postCookie(cookie: String) = {
		if(!lookupCookie(cookie)) {
			val fw = new FileWriter(filename, true)
			
			if(!firstLine) {
				fw.write("\n")
			} else {
				firstLine = false
			}

			fw.write(cookie)
			fw.close()

			cookies += cookie
		}
	}
}

object Application extends Controller {
	def getSecret() = {
		Source.fromFile(new File("secret")).getLines.toList(0)
	}

  def get(cookie: String) = Action {
  	if(Cookies.lookupCookie(cookie)) {
  		println("cookie " + cookie + " is listed")
  		Ok("{\"success\" : true}").as("application/json")
  	} else {
  		println("cookie " + cookie + " is not listed")
  		Ok("{\"success\" : false}").as("application/json")
  	}
  }

  def post = Action { implicit request => {
  	val values = request.body.asFormUrlEncoded

  	val cookie: Option[String] = values match {
  		case None => None
  		case Some(map) => 
  			val seq = map.get("cookie")
  			if(seq.isDefined && seq.get.size == 1) {
  				Some(seq.get.head)
  			} else {
  				None
  			}
  	}

  	if(cookie.isDefined) {
  		Cookies.postCookie(cookie.get)
  		println("cookie posted: " + cookie.get)
  		Ok("{\"success\" : true}").as("application/json")
  	} else {
  		println("post cookie request rejected")
  		Ok("{\"success\" : false}").as("application/json")
  	}
  }}

  def count = Action { implicit request => {
  	val secret = request.queryString.get("secret").map(_.mkString)

  	secret match {
  		case Some(secret) if secret == getSecret() => {
  			val numRegisteredCookies = Cookies.getNumRegisteredCookies()
  			println("#current registered cookies: " + numRegisteredCookies)
  			Ok("{\"registered-cookies\" : " + numRegisteredCookies + "}").as("application/json")
  		}
  		case _ => {
  			println("count request rejected")
  			Ok("{ }").as("application/json")
  		}
  	}
  }}

  def reset = Action { implicit request => {
  	val secret = request.queryString.get("secret").map(_.mkString)

  	secret match {
  		case Some(secret) if secret == getSecret() => {
  			Cookies.resetCounter()
  			println("counter reseted")
  			Ok("{\"success\" : true}").as("application/json")
  		}
  		case _ => {
  			println("reset request rejected")
  			Ok("{\"success\" : false}").as("application/json")
  		}
  	}
  }}
}