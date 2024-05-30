class ApplicationController < RageController::API
  def index
    head 200
  end

  def user
    render plain: params[:id]
  end

  def register_user
    head 200
  end
end
