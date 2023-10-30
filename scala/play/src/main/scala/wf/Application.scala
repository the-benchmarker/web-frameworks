package controllers

import javax.inject._

import play.api._
import play.api.mvc._

@Singleton
class Application @Inject()(val controllerComponents: ControllerComponents) extends BaseController {

  def index = Action {
    Ok("")
  }

  def g(id: Int) = Action {
    Ok(id.toString)
  }

  def p = Action {
    Ok("")
  }

}
