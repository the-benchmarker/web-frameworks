# frozen_string_literal: true

require 'plezi'

#Controllers
class UserController
  # HTTP
  def index
    ""
  end
  def show
    params[:id].to_s
  end
end

class HomeController
  # HTTP
  def index
    ""
  end
  def author
    "Boaz Segev"
  end
end

# Routes
Plezi.route '/user/(:id)', UserController
Plezi.route '/', HomeController

run Plezi.app
