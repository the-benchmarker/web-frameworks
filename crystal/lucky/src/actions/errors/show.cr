class Errors::Show < Lucky::ErrorAction
  default_format :html

  def handle_error(error : JSON::ParseException) : Lucky::Response
    if json?
      json({error: "There was a problem parsing the JSON. Please check that it is formed correctly"}, status: 400)
    else
      head status: 400
    end
  end

  def default_render(error : Exception) : Lucky::Response
    plain_text("", status: 500)
  end

  def report(error : Exception) : Nil
    nil
  end

  def handle_error(error : Exception) : Lucky::Response
    error.inspect_with_backtrace(STDERR)

    if json?
      json({error: "An unexpected error occured"}, status: 500)
    else
      response.status_code = 500
      plain_text "Something went wrong"
    end
  end
end
