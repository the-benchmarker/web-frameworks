package controllers

import javax.inject.Inject

import play.api._
import play.api.mvc._

class Application @Inject() (action: DefaultActionBuilder)
    extends ControllerHelpers {

  def index = action {
    Ok("")
  }

  def g(id: Int) = action {
    Ok(id.toString)
  }

  def p = action {
    Ok("")
  }

}
