require "jasper_helpers"

class ApplicationController < Amber::Controller::Base

  include JasperHelpers
  LAYOUT = "application.slang"

  def index
  end

  def get
    return params[:id]
  end

  def create
  end
end
