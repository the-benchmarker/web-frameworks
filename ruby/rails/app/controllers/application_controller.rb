class ApplicationController < ActionController::API
  after_action :prevent_response_caching

  private

  def prevent_response_caching
    response.headers["Cache-Control"] = "no-store"
  end
end
