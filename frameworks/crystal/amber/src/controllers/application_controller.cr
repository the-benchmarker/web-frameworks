class ApplicationController < Amber::Controller::Base

  def index
  end

  def get
    return params[:id]
  end

  def create
  end
end
