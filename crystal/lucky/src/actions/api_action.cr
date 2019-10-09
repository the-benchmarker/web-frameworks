abstract class ApiAction < Lucky::Action
  accepted_formats [:html, :json], default: :html
  # Add pipes and methods that are for all API requests
end
