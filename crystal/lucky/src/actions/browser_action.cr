abstract class BrowserAction < Lucky::Action
  include Lucky::ProtectFromForgery

  default_format :html
end
