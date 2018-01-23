abstract class BrowserAction < Lucky::Action
  include Lucky::ProtectFromForgery
  # If something should always be exposed to your pages, expose them here.
  #
  # Example:
  #
  #  expose current_user
  #
  #  def current_user
  #    find_the_user...
  #  end
  #
  # Then add an assign for it in your MainLayout
end
