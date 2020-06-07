# frozen_string_literal: true

class ApiController < ApplicationController
  def index
    head(200)
  end

  def user
    render(plain: params["id"])
  end

  def register_user
    head(200)
  end
end
