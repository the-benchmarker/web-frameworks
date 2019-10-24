class Errors::Show < Lucky::ErrorAction
  default_format :html

  def handle_error(error : JSON::ParseException)
    if json?
      json({error: "There was a problem parsing the JSON. Please check that it is formed correctly"}, status: 400)
    else
      head status: 400
    end
  end

  def default_render(error : Exception)
    render_text("", status: 500)
  end

  def report(error : Exception)
    nil
  end

  def handle_error(error : Exception)
    error.inspect_with_backtrace(STDERR)

    if json?
      json({error: "An unexpected error occured"}, status: 500)
    else
      response.status_code = 500
      render_text "Something went wrong"
    end
  end
end
