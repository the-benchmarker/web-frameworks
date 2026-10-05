class ApplicationController < ActionController::API
  rescue_from ActionController::ParameterMissing, ActionController::BadRequest, with: :bad_request

  after_action :prevent_response_caching

  private

  def prevent_response_caching
    response.headers["Cache-Control"] = "no-store"
  end

  def bad_request
    head :bad_request
  end
end
