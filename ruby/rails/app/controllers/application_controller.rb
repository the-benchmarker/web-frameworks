class ApplicationController < ActionController::Base
  skip_before_action :verify_authenticity_token

  def index
    head 200
  end

  def user
    render plain: params['id']
  end

  def register_user
    head 200
  end
end
