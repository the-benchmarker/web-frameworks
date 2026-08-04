# Error handling concern for Rails API controllers
# Centralized error handling following HTTP standards

module ErrorHandler
  extend ActiveSupport::Concern

  included do
    rescue_from ActiveRecord::RecordNotFound, with: :not_found
    rescue_from ActiveRecord::RecordInvalid, with: :unprocessable_entity
    rescue_from ActionController::ParameterMissing, with: :bad_request
    rescue_from StandardError, with: :internal_server_error
  end

  private

  def not_found(exception)
    render json: {
      error: "Not found",
      message: exception.message,
    }, status: :not_found
  end

  def unprocessable_entity(exception)
    render json: {
      error: "Validation failed",
      errors: exception.record.errors.full_messages,
    }, status: :unprocessable_entity
  end

  def bad_request(exception)
    render json: {
      error: "Bad request",
      message: exception.message,
    }, status: :bad_request
  end

  def internal_server_error(exception)
    # Don't expose internal errors in production
    message = Rails.env.development? ? exception.message : "Internal server error"
    Rails.logger.error [exception.class, exception.message, exception.backtrace].join("\n")
    render json: {
      error: "Internal server error",
      message: message,
    }, status: :internal_server_error
  end
end
