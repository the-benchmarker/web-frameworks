class ApplicationController < ActionController::API
  # The contract specifies root JSON fields rather than Rails model wrappers.
  wrap_parameters false

  rescue_from ActionController::ParameterMissing,
              ActionController::BadRequest,
              ActionDispatch::Http::Parameters::ParseError,
              Rack::BadRequest,
              with: :bad_request

  after_action :prevent_response_caching

  private

  def prevent_response_caching
    response.headers['Cache-Control'] = 'no-store'
  end

  def bad_request
    # after_action is skipped when a before_action halts or an exception occurs.
    prevent_response_caching
    head :bad_request
  end

  def unsupported_media_type
    prevent_response_caching
    head :unsupported_media_type
  end
end
