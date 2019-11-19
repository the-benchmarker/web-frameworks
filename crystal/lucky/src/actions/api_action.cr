abstract class ApiAction < Lucky::Action
  accepted_formats [:html, :json], default: :html
end
