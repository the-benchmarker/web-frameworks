class ApplicationController < ActionController::API
  # The contract specifies root JSON fields rather than Rails model wrappers.
  wrap_parameters false

  before_action :prevent_response_caching

  rescue_from ActionController::BadRequest,
              ActionDispatch::Http::Parameters::ParseError,
              Rack::BadRequest,
              with: :bad_request

  private

  def prevent_response_caching
    response.headers['Cache-Control'] = 'no-store'
  end

  def bad_request
    head :bad_request
  end

  def unsupported_media_type
    head :unsupported_media_type
  end
end
