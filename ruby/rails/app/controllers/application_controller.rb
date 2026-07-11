# Base application controller for Rails API
# Clean implementation with minimal functionality

class ApplicationController < ActionController::API
  # Security: Set request ID for tracing
  before_action :set_request_id

  # Error handling - delegated to concerns
  include ErrorHandler
end
