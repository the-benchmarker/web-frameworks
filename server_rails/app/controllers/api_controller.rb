class ApiController < ApplicationController

  def index
    head 200
  end

  def user
    render text: params["id"]
  end

  def register_user
    head 200
  end
end
