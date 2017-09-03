class Raze::ExceptionHandler
  include HTTP::Handler
  INSTANCE = new

  def call(context)
    begin
      call_next(context)
    rescue ex : Raze::Exceptions::RouteNotFound
      call_exception_with_status_code(context, ex, 404)
    rescue ex : Raze::Exceptions::CustomException
      call_exception_with_status_code(context, ex, context.response.status_code)
    rescue ex : Exception
      # log("Exception: #{ex.inspect_with_backtrace}")
      call_exception_with_status_code(context, ex, 500)
    end
  end

  private def call_exception_with_status_code(context, exception, status_code)
    if Raze.config.error_handlers.has_key?(status_code)
      context.response.content_type = "text/html" unless context.response.headers.has_key?("Content-Type")
      context.response.print Raze.config.error_handlers[status_code].call(context, exception)
      context.response.status_code = status_code
      context
    end
  end
end
