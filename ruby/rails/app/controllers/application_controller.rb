class ApplicationController < ActionController::API
  # The contract specifies root JSON fields rather than Rails model wrappers.
  wrap_parameters false

  rescue_from ActionController::BadRequest,
              ActionDispatch::Http::Parameters::ParseError,
              Rack::BadRequest,
              with: :bad_request

  private

  def bad_request
    head :bad_request
  end

  def require_media_type(expected_type)
    head :unsupported_media_type unless request.content_mime_type == Mime::Type.lookup(expected_type)
  end
end
